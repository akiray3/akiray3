# app.R
#
# 耳朶容積脈波(PPG) HRV解析Shinyアプリ。
# 本ファイルはUIとリアクティブ制御のみを行い、解析ロジックはR/以下の
# 関数を呼び出すだけとする（section 10）。

library(shiny)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

for (f in list.files(file.path(getwd(), "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

APP_CONFIG <- load_app_config(file.path(getwd(), "config", "defaults.yml"))
APP_VERSION <- APP_CONFIG$app$version %||% "0.1.0"

KNOWN_TIME_COL_HINT <- paste(KNOWN_TIME_COLS, collapse = ", ")
KNOWN_PPG_COL_HINT <- paste(KNOWN_PPG_COLS, collapse = ", ")

## ---- UI ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("耳朶容積脈波(PPG) HRV解析"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. データ入力"),
      fileInput("raw_file", "ローデータファイル (CSV/TSV)", accept = c(".csv", ".tsv", ".txt")),
      uiOutput("column_select_ui"),
      helpText(sprintf("既知の時刻列: %s / 既知のPPG列: %s", KNOWN_TIME_COL_HINT, KNOWN_PPG_COL_HINT)),
      verbatimTextOutput("fs_info"),
      checkboxInput("fs_manual_toggle", "サンプリング周波数を手動で上書きする", value = FALSE),
      conditionalPanel(
        "input.fs_manual_toggle == true",
        numericInput("fs_manual", "サンプリング周波数 (Hz)", value = 200, min = 1, max = 2000)
      ),

      hr(),
      h4("2. 解析モード"),
      radioButtons("mode", NULL,
        choices = c("legacy（PulseWaveTools忠実再現）" = "legacy",
                    "recommended（改良版）" = "recommended"),
        selected = "legacy"
      ),
      conditionalPanel(
        "input.mode == 'recommended'",
        numericInput("min_peak_distance_sec", "最小ピーク距離 (秒)", value = 0.35, min = 0.1, max = 2, step = 0.05),
        numericInput("resample_hz", "再サンプリング周波数 (Hz)", value = 4, min = 1, max = 20, step = 1)
      ),

      hr(),
      h4("3. 解析対象範囲"),
      fluidRow(
        column(6, numericInput("analysis_start", "開始秒", value = 0)),
        column(6, numericInput("analysis_end", "終了秒", value = 100))
      ),
      actionButton("full_range_btn", "全範囲を使用"),

      hr(),
      h4("4. 窓設定"),
      radioButtons("window_mode", NULL,
        choices = c("単一区間" = "single", "固定窓長+ステップ" = "fixed", "複数区間CSV" = "multi_csv"),
        selected = "single"
      ),
      conditionalPanel(
        "input.window_mode == 'fixed'",
        numericInput("window_length_sec", "窓長 (秒)", value = 300, min = 1),
        numericInput("step_sec", "ステップ幅 (秒、既定=窓長)", value = 300, min = 1)
      ),
      conditionalPanel(
        "input.window_mode == 'multi_csv'",
        fileInput("window_csv", "複数区間CSV (window_start_sec, window_end_sec[, window_id])", accept = ".csv")
      ),

      hr(),
      h4("5. 周波数帯設定"),
      fluidRow(
        column(6, numericInput("lf_lower", "LF下限(Hz)", value = 0.04, step = 0.01)),
        column(6, numericInput("lf_upper", "LF上限(Hz)", value = 0.15, step = 0.01))
      ),
      fluidRow(
        column(6, numericInput("hf_lower", "HF下限(Hz)", value = 0.15, step = 0.01)),
        column(6, numericInput("hf_upper", "HF上限(Hz)", value = 0.40, step = 0.01))
      ),

      hr(),
      actionButton("run_analysis", "解析実行", class = "btn-primary"),

      hr(),
      h4("6. ダウンロード"),
      downloadButton("download_results", "結果+QC CSV"),
      downloadButton("download_peaks_rr", "ピーク/RR CSV"),
      downloadButton("download_conditions", "解析条件 (JSON)")
    ),

    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("波形・ピーク", plotOutput("waveform_plot", height = "320px")),
        tabPanel("RR tachogram", plotOutput("tachogram_plot", height = "320px")),
        tabPanel("補正前後RR比較", plotOutput("rr_correction_plot", height = "320px")),
        tabPanel("PSD",
          uiOutput("psd_window_select_ui"),
          plotOutput("psd_plot", height = "320px")
        ),
        tabPanel("結果表", DT::dataTableOutput("results_table")),
        tabPanel("警告・QC要約", verbatimTextOutput("qc_summary"))
      )
    )
  )
)

## ---- Server -------------------------------------------------------------

server <- function(input, output, session) {

  imported <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)

  observeEvent(input$raw_file, {
    res <- tryCatch(
      import_shimmer_file(input$raw_file$datapath),
      error = function(e) {
        showNotification(paste("ファイルを読み込めません:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(res)) return()
    imported(res)
    updateNumericInput(session, "analysis_start", value = 0)
    updateNumericInput(session, "analysis_end", value = max(res$time_sec, na.rm = TRUE))
    if (length(res$warnings) > 0L) {
      showNotification(paste(res$warnings, collapse = "\n"), type = "warning", duration = 10)
    }
  })

  output$column_select_ui <- renderUI({
    req(imported())
    cols <- imported()$columns$all
    tagList(
      selectInput("time_col", "時刻列", choices = cols, selected = imported()$columns$time_col),
      selectInput("ppg_col", "PPG列", choices = cols, selected = imported()$columns$ppg_col)
    )
  })

  observeEvent(list(input$time_col, input$ppg_col), {
    req(imported(), input$raw_file, input$time_col, input$ppg_col)
    if (input$time_col == imported()$columns$time_col && input$ppg_col == imported()$columns$ppg_col) {
      return()
    }
    res <- tryCatch(
      import_shimmer_file(input$raw_file$datapath, time_col = input$time_col, ppg_col = input$ppg_col),
      error = function(e) {
        showNotification(paste("列の選択が不正です:", conditionMessage(e)), type = "error")
        NULL
      }
    )
    if (!is.null(res)) {
      imported(res)
      updateNumericInput(session, "analysis_end", value = max(res$time_sec, na.rm = TRUE))
    }
  }, ignoreInit = TRUE)

  output$fs_info <- renderText({
    req(imported())
    d <- imported()
    sprintf(
      "推定サンプリング周波数: %.3f Hz (dt中央値=%.5f 秒)\n使用中のfs: %.3f Hz (%s)\nサンプル数: %d / 記録長: %.1f 秒",
      d$fs_estimated_hz %||% NA_real_, d$dt_median_sec %||% NA_real_,
      d$fs_hz, d$fs_source, d$n_samples, max(d$time_sec, na.rm = TRUE)
    )
  })

  observeEvent(input$full_range_btn, {
    req(imported())
    updateNumericInput(session, "analysis_start", value = 0)
    updateNumericInput(session, "analysis_end", value = max(imported()$time_sec, na.rm = TRUE))
  })

  fs_used <- reactive({
    req(imported())
    if (isTRUE(input$fs_manual_toggle) && is.finite(input$fs_manual) && input$fs_manual > 0) {
      input$fs_manual
    } else {
      imported()$fs_hz
    }
  })

  observeEvent(input$run_analysis, {
    d <- imported()
    if (is.null(d)) {
      showNotification("先にローデータファイルをアップロードしてください。", type = "error")
      return()
    }

    freq_bands <- list(
      lf_lower_hz = input$lf_lower, lf_upper_hz = input$lf_upper,
      hf_lower_hz = input$hf_lower, hf_upper_hz = input$hf_upper
    )
    if (!(freq_bands$lf_lower_hz < freq_bands$lf_upper_hz) ||
        !(freq_bands$hf_lower_hz < freq_bands$hf_upper_hz)) {
      showNotification("周波数帯の下限・上限が不正です。", type = "error")
      return()
    }

    multi_csv_df <- NULL
    if (input$window_mode == "multi_csv") {
      if (is.null(input$window_csv)) {
        showNotification("複数区間CSVをアップロードしてください。", type = "error")
        return()
      }
      multi_csv_df <- tryCatch(
        read_window_csv(input$window_csv$datapath),
        error = function(e) {
          showNotification(paste("複数区間CSVを解釈できません:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(multi_csv_df)) return()
    }

    res <- tryCatch({
      run_hrv_analysis(
        time_sec = d$time_sec, ppg = d$ppg, fs_hz = fs_used(),
        analysis_start_sec = input$analysis_start, analysis_end_sec = input$analysis_end,
        mode = input$mode, window_mode = input$window_mode,
        window_length_sec = input$window_length_sec, step_sec = input$step_sec,
        multi_csv_df = multi_csv_df, freq_bands = freq_bands,
        psd_config = list(
          window_sec = APP_CONFIG$psd$window_sec, overlap_ratio = APP_CONFIG$psd$overlap_ratio,
          nfft = APP_CONFIG$psd$nfft
        ),
        peak_params = list(min_peak_distance_sec = input$min_peak_distance_sec, min_peak_height = NULL),
        sd_multiplier = APP_CONFIG$outlier$legacy$sd_multiplier,
        resample_hz = input$resample_hz %||% APP_CONFIG$resampling$recommended$default_hz,
        config = APP_CONFIG,
        file_name = input$raw_file$name %||% "",
        analysis_id = format(Sys.time(), "%Y%m%d%H%M%S")
      )
    }, error = function(e) {
      showNotification(paste("解析に失敗しました:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })

    if (!is.null(res)) {
      analysis_result(res)
      showNotification("解析が完了しました。", type = "message")
    }
  })

  ## ---- 表示 -----------------------------------------------------------

  output$waveform_plot <- renderPlot({
    req(imported())
    d <- imported()
    disp <- decimate_for_display(d$time_sec, d$ppg, APP_CONFIG$display$waveform_decimation_max_points)
    plot(disp$x, disp$y, type = "l", col = "steelblue", xlab = "時間 (秒)", ylab = "PPG振幅",
         main = "PPG波形とピーク（表示は間引き、解析には全データを使用）")
    r <- analysis_result()
    if (!is.null(r) && nrow(r$peaks) > 0L) {
      points(r$peaks$ms / 1000, r$peaks$Amplitude, col = "red", pch = 19, cex = 0.6)
    }
  })

  output$tachogram_plot <- renderPlot({
    r <- analysis_result()
    req(r)
    plot(r$rr$ms / 1000, r$rr$RR_ms_corrected, type = "o", pch = 16, cex = 0.4, col = "darkgreen",
         xlab = "時間 (秒)", ylab = "RR間隔 (ms、補正後)", main = "RR tachogram")
  })

  output$rr_correction_plot <- renderPlot({
    r <- analysis_result()
    req(r)
    plot(r$rr$ms / 1000, r$rr$RR_ms, type = "l", col = "grey60",
         xlab = "時間 (秒)", ylab = "RR間隔 (ms)", main = "補正前後RR比較")
    lines(r$rr$ms / 1000, r$rr$RR_ms_corrected, col = "red")
    legend("topright", legend = c("補正前", "補正後"), col = c("grey60", "red"), lty = 1)
  })

  output$psd_window_select_ui <- renderUI({
    r <- analysis_result()
    req(r)
    selectInput("psd_window_id", "表示する窓", choices = names(r$psd_by_window))
  })

  output$psd_plot <- renderPlot({
    r <- analysis_result()
    req(r, input$psd_window_id)
    p <- r$psd_by_window[[input$psd_window_id]]
    req(p)
    plot(p$freq, p$psd, type = "l", col = "purple",
         xlab = "周波数 (Hz)", ylab = sprintf("PSD (%s)", p$psd_unit),
         main = sprintf("PSD - %s", input$psd_window_id))
    abline(v = c(input$lf_lower, input$lf_upper, input$hf_lower, input$hf_upper), lty = 2, col = "grey50")
  })

  output$results_table <- DT::renderDataTable({
    r <- analysis_result()
    req(r)
    DT::datatable(r$results_df, options = list(scrollX = TRUE, pageLength = 10))
  })

  output$qc_summary <- renderText({
    r <- analysis_result()
    req(r)
    tab <- table(r$results_df$warning_code)
    lines <- c(
      sprintf("外れ値補正方法: %s", r$outlier_method),
      sprintf("等間隔化方法: %s", r$resample_method),
      "",
      "warning_code 集計:",
      paste(sprintf("  %s: %d 窓", names(tab), as.integer(tab)), collapse = "\n")
    )
    paste(lines, collapse = "\n")
  })

  ## ---- ダウンロード -----------------------------------------------------

  output$download_results <- downloadHandler(
    filename = function() sprintf("hrv_results_%s.csv", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file) {
      r <- analysis_result()
      validate(need(!is.null(r), "先に解析を実行してください。"))
      write_csv_utf8(r$results_df, file)
    }
  )

  output$download_peaks_rr <- downloadHandler(
    filename = function() sprintf("hrv_peaks_rr_%s.csv", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file) {
      r <- analysis_result()
      validate(need(!is.null(r), "先に解析を実行してください。"))
      write_csv_utf8(build_peak_rr_table(r$peaks, r$rr), file)
    }
  )

  output$download_conditions <- downloadHandler(
    filename = function() sprintf("hrv_conditions_%s.json", format(Sys.time(), "%Y%m%d%H%M%S")),
    content = function(file) {
      r <- analysis_result()
      d <- imported()
      validate(need(!is.null(r) && !is.null(d), "先に解析を実行してください。"))
      cond <- build_analysis_conditions(
        app_version = APP_VERSION,
        pulsewavetools_version = "引き継ぎ書section4のアルゴリズム仕様に基づく再実装（原ソース未入手）",
        input_columns = d$columns,
        sampling_rate_hz = fs_used(),
        peak_condition = list(mode = input$mode, min_peak_distance_sec =
          if (input$mode == "legacy") 0.5 else input$min_peak_distance_sec),
        outlier_condition = list(mode = input$mode, sd_multiplier = APP_CONFIG$outlier$legacy$sd_multiplier),
        resampling_condition = list(mode = input$mode, resample_hz = r$resampled$resample_hz),
        psd_condition = APP_CONFIG$psd,
        freq_bands = list(lf_lower_hz = input$lf_lower, lf_upper_hz = input$lf_upper,
                           hf_lower_hz = input$hf_lower, hf_upper_hz = input$hf_upper)
      )
      write_analysis_conditions_json(cond, file)
    }
  )
}

shinyApp(ui, server)
