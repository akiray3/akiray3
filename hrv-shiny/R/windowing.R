# windowing.R
#
# 時間窓の作成とRR系列の窓分割。
#
# 重要方針（section 5）：窓ごとに生波形へ戻ってピーク検出をやり直さない。
# 全解析範囲で一度検出したRR系列（および等間隔化系列）を、ここで窓へ分割する。
#
# 区間規則（section 5）：[start, end) とし、最終窓のみ必要に応じて終端を含める。
# 重複窓（オーバーラップするスライディング窓）を許容する。

#' 複数区間CSVを読み込む（window_start_sec, window_end_sec [, window_id]）
#'
#' @param path CSVファイルパス
#' @return data.frame(window_id, window_start_sec, window_end_sec)
read_window_csv <- function(path) {
  dt <- tryCatch(
    data.table::fread(path, data.table = FALSE, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0L) {
    stop("複数区間CSVを解釈できません。")
  }
  names(dt) <- trimws(names(dt))
  req_cols <- c("window_start_sec", "window_end_sec")
  if (!all(req_cols %in% names(dt))) {
    stop("複数区間CSVには window_start_sec, window_end_sec 列が必要です。")
  }
  if (!("window_id" %in% names(dt))) {
    dt$window_id <- sprintf("W%03d", seq_len(nrow(dt)))
  }
  dt$window_start_sec <- as.numeric(dt$window_start_sec)
  dt$window_end_sec <- as.numeric(dt$window_end_sec)
  if (any(!is.finite(dt$window_start_sec)) || any(!is.finite(dt$window_end_sec)) ||
      any(dt$window_end_sec <= dt$window_start_sec)) {
    stop("複数区間CSVの開始・終了範囲が不正です。")
  }
  dt[, c("window_id", "window_start_sec", "window_end_sec")]
}

#' 解析窓のリストを作成する（単一区間 / 固定窓長+ステップ / 複数区間CSV）
#'
#' @param analysis_start_sec 解析対象範囲の開始秒
#' @param analysis_end_sec 解析対象範囲の終了秒
#' @param window_mode "single" | "fixed" | "multi_csv"
#' @param window_length_sec 固定窓長（fixedモード）
#' @param step_sec ステップ幅（fixedモード。NULLならwindow_length_secと同じ=非重複）
#' @param multi_csv_df read_window_csv()の出力（multi_csvモード）
#' @param record_start_sec 記録全体の開始秒（範囲外チェック用、通常0）
#' @param record_end_sec 記録全体の終了秒（範囲外チェック用）
#' @return data.frame(window_id, window_start_sec, window_end_sec, window_length_sec,
#'                     step_sec, include_end, out_of_range)
build_windows <- function(analysis_start_sec, analysis_end_sec,
                           window_mode = c("single", "fixed", "multi_csv"),
                           window_length_sec = NULL, step_sec = NULL,
                           multi_csv_df = NULL,
                           record_start_sec = NULL, record_end_sec = NULL) {
  window_mode <- match.arg(window_mode)

  if (!is.finite(analysis_start_sec) || !is.finite(analysis_end_sec) ||
      analysis_end_sec <= analysis_start_sec) {
    stop("開始・終了範囲が不正です。")
  }

  if (window_mode == "single") {
    windows <- data.frame(
      window_id = "W001",
      window_start_sec = analysis_start_sec,
      window_end_sec = analysis_end_sec,
      window_length_sec = analysis_end_sec - analysis_start_sec,
      step_sec = NA_real_,
      include_end = TRUE,
      stringsAsFactors = FALSE
    )
  } else if (window_mode == "fixed") {
    if (is.null(window_length_sec) || !is.finite(window_length_sec) || window_length_sec <= 0) {
      stop("窓長が不正です。")
    }
    if (is.null(step_sec) || !is.finite(step_sec) || step_sec <= 0) {
      step_sec <- window_length_sec
    }
    # seq()の浮動小数点累積誤差でstep*n==durationのときに極小の余分な窓が
    # 生成されるのを避けるため、窓数を整数演算で先に求めてから開始秒を作る。
    n_windows <- floor((analysis_end_sec - analysis_start_sec) / step_sec + 1e-9) + 1L
    starts <- analysis_start_sec + (seq_len(n_windows) - 1L) * step_sec
    starts <- starts[starts < analysis_end_sec - 1e-9]
    if (length(starts) == 0L) {
      starts <- analysis_start_sec
    }
    ends <- pmin(starts + window_length_sec, analysis_end_sec)
    windows <- data.frame(
      window_id = sprintf("W%03d", seq_along(starts)),
      window_start_sec = starts,
      window_end_sec = ends,
      window_length_sec = ends - starts,
      step_sec = step_sec,
      include_end = ends >= (analysis_end_sec - 1e-9),
      stringsAsFactors = FALSE
    )
  } else {
    if (is.null(multi_csv_df) || nrow(multi_csv_df) == 0L) {
      stop("複数区間CSVが指定されていません。")
    }
    req_cols <- c("window_start_sec", "window_end_sec")
    if (!all(req_cols %in% names(multi_csv_df))) {
      stop("複数区間CSVには window_start_sec, window_end_sec 列が必要です。")
    }
    wid <- if ("window_id" %in% names(multi_csv_df)) {
      as.character(multi_csv_df$window_id)
    } else {
      sprintf("W%03d", seq_len(nrow(multi_csv_df)))
    }
    ws <- as.numeric(multi_csv_df$window_start_sec)
    we <- as.numeric(multi_csv_df$window_end_sec)
    windows <- data.frame(
      window_id = wid,
      window_start_sec = ws,
      window_end_sec = we,
      window_length_sec = we - ws,
      step_sec = NA_real_,
      include_end = we >= (max(we, na.rm = TRUE) - 1e-9),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(record_start_sec) && !is.null(record_end_sec) &&
      is.finite(record_start_sec) && is.finite(record_end_sec)) {
    windows$out_of_range <- windows$window_start_sec < record_start_sec |
      windows$window_end_sec > record_end_sec
  } else {
    windows$out_of_range <- FALSE
  }

  windows
}

#' RR系列（generate_rr()/run_rr_pipeline()$rr）を時間窓へ分割する
#'
#' @param rr_df data.frame(ms, ...)。msは後側ピーク時刻(ms)。
#' @param window_start_sec, window_end_sec 窓の開始・終了秒
#' @param include_end 終端を含めるか（[start,end] vs [start,end)）
#' @return rr_dfの部分集合（該当行）
slice_rr_by_window <- function(rr_df, window_start_sec, window_end_sec, include_end = FALSE) {
  t_sec <- rr_df$ms / 1000
  idx <- if (isTRUE(include_end)) {
    t_sec >= window_start_sec & t_sec <= window_end_sec
  } else {
    t_sec >= window_start_sec & t_sec < window_end_sec
  }
  rr_df[idx, , drop = FALSE]
}

#' 等間隔化系列（resample_rr()の出力）を時間窓へ分割する
#'
#' @param resampled list(time_sec, value, ...)
#' @param window_start_sec, window_end_sec 窓の開始・終了秒
#' @param include_end 終端を含めるか
#' @return list(time_sec, value)
slice_resampled_by_window <- function(resampled, window_start_sec, window_end_sec, include_end = FALSE) {
  idx <- if (isTRUE(include_end)) {
    resampled$time_sec >= window_start_sec & resampled$time_sec <= window_end_sec
  } else {
    resampled$time_sec >= window_start_sec & resampled$time_sec < window_end_sec
  }
  list(time_sec = resampled$time_sec[idx], value = resampled$value[idx])
}

#' 全解析範囲でピーク検出・RR生成・RR補正・等間隔化を行い、時間窓ごとに
#' PSDとHRV指標を算出する統合関数（section 5 標準処理順序の実行主体）。
#'
#' 窓ごとにraw波形へ戻ってピーク検出をやり直すことはしない。
#' 窓ごとの失敗（RR不足・PSD不能等）はその窓のwarning_codeに反映し、
#' 他の窓の計算やアプリ全体には影響させない（section 9）。
#'
#' @param time_sec 経過秒ベクトル（記録全体）
#' @param ppg PPG振幅ベクトル（記録全体）
#' @param fs_hz サンプリング周波数(Hz)（生波形）
#' @param analysis_start_sec, analysis_end_sec 解析対象範囲（秒）
#' @param mode "legacy" または "recommended"
#' @param window_mode "single" | "fixed" | "multi_csv"
#' @param window_length_sec, step_sec fixedモード用（秒）
#' @param multi_csv_df multi_csvモード用（read_window_csv()の出力）
#' @param freq_bands list(lf_lower_hz, lf_upper_hz, hf_lower_hz, hf_upper_hz)
#' @param psd_config list(window_sec, overlap_ratio, nfft)
#' @param peak_params list(min_peak_distance_sec, min_peak_height)（recommendedのみ使用）
#' @param sd_multiplier 外れ値補正のSD倍率
#' @param resample_hz recommendedモードでの等間隔化周波数(Hz)
#' @param config load_app_config()の戻り値
#' @param file_name, analysis_id 結果CSVに記録する識別子
#' @return list(peaks, rr, resampled, windows, results_df, psd_by_window, outlier_method, resample_method)
run_hrv_analysis <- function(time_sec, ppg, fs_hz,
                              analysis_start_sec, analysis_end_sec,
                              mode = c("legacy", "recommended"),
                              window_mode = c("single", "fixed", "multi_csv"),
                              window_length_sec = NULL, step_sec = NULL, multi_csv_df = NULL,
                              freq_bands, psd_config = list(window_sec = NULL, overlap_ratio = 0.5, nfft = NULL),
                              peak_params = list(min_peak_distance_sec = NULL, min_peak_height = NULL),
                              sd_multiplier = 3, resample_hz = 4,
                              config, file_name = "", analysis_id = "") {
  mode <- match.arg(mode)
  window_mode <- match.arg(window_mode)

  record_start_sec <- min(time_sec, na.rm = TRUE)
  record_end_sec <- max(time_sec, na.rm = TRUE)

  if (!is.finite(analysis_start_sec) || !is.finite(analysis_end_sec) ||
      analysis_end_sec <= analysis_start_sec ||
      analysis_start_sec < record_start_sec - 1e-6 || analysis_end_sec > record_end_sec + 1e-6) {
    stop("開始・終了範囲が不正です。")
  }

  idx <- which(time_sec >= analysis_start_sec & time_sec <= analysis_end_sec)
  if (length(idx) < 3L) {
    stop("指定範囲のデータ点数が不足しています。")
  }
  ppg_sub <- ppg[idx]
  time_sub <- time_sec[idx]

  pk <- detect_peaks(ppg_sub, fs_hz, time_sec = time_sub, mode = mode,
                      min_peak_distance_sec = peak_params$min_peak_distance_sec,
                      min_peak_height = peak_params$min_peak_height)
  peaks <- pk$peaks
  if (nrow(peaks) < 2L) {
    stop("ピーク数が不足しているためRRを生成できません。")
  }

  rr_pipeline <- run_rr_pipeline(peaks, mode = mode, sd_multiplier = sd_multiplier,
                                  resample_hz = resample_hz)
  rr <- rr_pipeline$rr
  resampled <- rr_pipeline$resampled

  windows <- build_windows(
    analysis_start_sec, analysis_end_sec, window_mode = window_mode,
    window_length_sec = window_length_sec, step_sec = step_sec,
    multi_csv_df = multi_csv_df,
    record_start_sec = record_start_sec, record_end_sec = record_end_sec
  )

  result_rows <- vector("list", nrow(windows))
  psd_by_window <- list()

  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    peak_time_sec <- peaks$ms / 1000
    n_raw_in_window <- sum(time_sub >= w$window_start_sec &
      (if (w$include_end) time_sub <= w$window_end_sec else time_sub < w$window_end_sec))
    n_peaks_in_window <- sum(peak_time_sec >= w$window_start_sec &
      (if (w$include_end) peak_time_sec <= w$window_end_sec else peak_time_sec < w$window_end_sec))

    row_common <- list(
      file_name = file_name, analysis_id = analysis_id, analysis_mode = mode,
      window_id = w$window_id, window_start_sec = w$window_start_sec,
      window_end_sec = w$window_end_sec, window_length_sec = w$window_length_sec,
      step_sec = w$step_sec, sampling_rate_raw_hz = fs_hz,
      resampling_rate_rr_hz = resampled$resample_hz,
      n_raw_samples = n_raw_in_window, n_peaks = n_peaks_in_window,
      lf_lower_hz = freq_bands$lf_lower_hz, lf_upper_hz = freq_bands$lf_upper_hz,
      hf_lower_hz = freq_bands$hf_lower_hz, hf_upper_hz = freq_bands$hf_upper_hz
    )

    res <- tryCatch({
      rr_win <- slice_rr_by_window(rr, w$window_start_sec, w$window_end_sec, include_end = w$include_end)
      resampled_win <- slice_resampled_by_window(resampled, w$window_start_sec, w$window_end_sec, include_end = w$include_end)

      td <- time_domain_metrics(rr_win$RR_ms_corrected)
      artifact_count <- sum(rr_win$is_outlier, na.rm = TRUE)
      artifact_percent <- if (nrow(rr_win) > 0L) 100 * artifact_count / nrow(rr_win) else NA_real_

      psd_res <- analyze_window_psd(resampled_win$value, resampled$resample_hz, freq_bands,
                                     psd_config = psd_config, rr_unit = "sec")

      qc <- evaluate_window_qc(
        window_length_sec = w$window_length_sec, n_rr_used = td$n_rr_used,
        n_psd_points_lf = psd_res$n_psd_points_lf, n_psd_points_hf = psd_res$n_psd_points_hf,
        artifact_percent = artifact_percent, LF_power = psd_res$LF_power, HF_power = psd_res$HF_power,
        out_of_range = isTRUE(w$out_of_range), config = config
      )

      psd_by_window[[w$window_id]] <- list(freq = psd_res$freq, psd = psd_res$psd, psd_unit = psd_res$psd_unit)

      list(
        n_rr_raw = nrow(rr_win), n_rr_used = td$n_rr_used,
        mean_rr_ms = td$mean_rr_ms, mean_hr_bpm = td$mean_hr_bpm,
        sdnn_ms = td$sdnn_ms, rmssd_ms = td$rmssd_ms,
        LF_power = if (qc$is_valid) psd_res$LF_power else NA_real_,
        HF_power = if (qc$is_valid) psd_res$HF_power else NA_real_,
        LF_HF = if (qc$is_valid) psd_res$LF_HF else NA_real_,
        HF_norm = if (qc$is_valid) psd_res$HF_norm else NA_real_,
        LF_norm = if (qc$is_valid) psd_res$LF_norm else NA_real_,
        LF_percent = if (qc$is_valid) psd_res$LF_percent else NA_real_,
        HF_percent = if (qc$is_valid) psd_res$HF_percent else NA_real_,
        psd_method = psd_res$psd_method,
        artifact_count = artifact_count, artifact_percent = artifact_percent,
        warning_code = qc$warning_code, warning_message = qc$warning_message
      )
    }, error = function(e) {
      list(
        n_rr_raw = NA_integer_, n_rr_used = NA_integer_,
        mean_rr_ms = NA_real_, mean_hr_bpm = NA_real_, sdnn_ms = NA_real_, rmssd_ms = NA_real_,
        LF_power = NA_real_, HF_power = NA_real_, LF_HF = NA_real_, HF_norm = NA_real_, LF_norm = NA_real_,
        LF_percent = NA_real_, HF_percent = NA_real_, psd_method = NA_character_,
        artifact_count = NA_integer_, artifact_percent = NA_real_,
        warning_code = "E_WINDOW_FAILED",
        warning_message = paste0("この窓の解析に失敗しました: ", conditionMessage(e))
      )
    })

    result_rows[[i]] <- do.call(build_result_row, c(row_common, res))
  }

  list(
    peaks = peaks, rr = rr, resampled = resampled,
    windows = windows, results_df = combine_result_rows(result_rows),
    psd_by_window = psd_by_window,
    outlier_method = rr_pipeline$outlier_method, resample_method = rr_pipeline$resample_method
  )
}
