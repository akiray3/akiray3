# export.R
#
# 結果CSV・ピーク/RR CSV・解析条件CSV/JSONの出力ビルダー（section 8）。
# ここではデータ整形とファイル書き出しのみを行い、解析ロジックは持たない。

RESULT_CSV_COLUMNS <- c(
  "file_name", "analysis_id", "analysis_mode", "window_id",
  "window_start_sec", "window_end_sec", "window_length_sec", "step_sec",
  "sampling_rate_raw_hz", "resampling_rate_rr_hz",
  "n_raw_samples", "n_peaks", "n_rr_raw", "n_rr_used",
  "mean_rr_ms", "mean_hr_bpm", "sdnn_ms", "rmssd_ms",
  "LF_power", "HF_power", "LF_HF", "HF_norm", "LF_norm", "LF_percent", "HF_percent",
  "lf_lower_hz", "lf_upper_hz", "hf_lower_hz", "hf_upper_hz", "psd_method",
  "artifact_count", "artifact_percent", "warning_code", "warning_message"
)

#' 結果CSVの1行を構築する（section 8.1の列に対応）
#' @param ... RESULT_CSV_COLUMNS と同名の引数（未指定はNAになる）
#' @return 1行のdata.frame
build_result_row <- function(...) {
  args <- list(...)
  row <- as.list(rep(NA, length(RESULT_CSV_COLUMNS)))
  names(row) <- RESULT_CSV_COLUMNS
  unknown <- setdiff(names(args), RESULT_CSV_COLUMNS)
  if (length(unknown) > 0L) {
    stop(sprintf("build_result_row: 未知の列名です: %s", paste(unknown, collapse = ", ")))
  }
  row[names(args)] <- args
  as.data.frame(row, stringsAsFactors = FALSE)
}

#' 複数の結果行を1つのdata.frameに結合する
#' @param rows build_result_row() の戻り値のリスト
#' @return data.frame（列順はRESULT_CSV_COLUMNS）
combine_result_rows <- function(rows) {
  if (length(rows) == 0L) {
    empty <- as.data.frame(setNames(replicate(length(RESULT_CSV_COLUMNS), character(0), simplify = FALSE), RESULT_CSV_COLUMNS))
    return(empty)
  }
  out <- do.call(rbind, rows)
  out[, RESULT_CSV_COLUMNS, drop = FALSE]
}

#' ピーク・RR再現性確認用テーブルを作成する（section 8.2）
#'
#' @param peaks data.frame(Amplitude, Point, ms) 検出ピーク（解析範囲全体）
#' @param rr_df data.frame(Point, ms, RR_ms, RR_ms_corrected, is_outlier)（generate_rr/run_rr_pipelineの出力。RRは後側ピークに対応）
#' @return data.frame(peak_time_sec, sample_index, amplitude, raw_RR_ms, artifact_flag, corrected_RR_ms)
build_peak_rr_table <- function(peaks, rr_df) {
  base <- data.frame(
    sample_index = peaks$Point,
    peak_time_sec = peaks$ms / 1000,
    amplitude = peaks$Amplitude,
    stringsAsFactors = FALSE
  )
  rr_map <- data.frame(
    sample_index = rr_df$Point,
    raw_RR_ms = rr_df$RR_ms,
    artifact_flag = rr_df$is_outlier,
    corrected_RR_ms = rr_df$RR_ms_corrected,
    stringsAsFactors = FALSE
  )
  merged <- merge(base, rr_map, by = "sample_index", all.x = TRUE)
  merged <- merged[order(merged$sample_index), ]
  merged <- merged[, c("peak_time_sec", "sample_index", "amplitude",
                        "raw_RR_ms", "artifact_flag", "corrected_RR_ms")]
  rownames(merged) <- NULL
  merged
}

#' 解析条件一式を構築する（section 8.3）
#'
#' @return list（write_analysis_conditions_json/csvへ渡す）
build_analysis_conditions <- function(app_version, pulsewavetools_version,
                                       input_columns, sampling_rate_hz,
                                       peak_condition, outlier_condition,
                                       resampling_condition, psd_condition,
                                       freq_bands, run_datetime = Sys.time()) {
  list(
    app_version = app_version,
    pulsewavetools_version = pulsewavetools_version,
    input_columns = input_columns,
    sampling_rate_hz = sampling_rate_hz,
    peak_condition = peak_condition,
    outlier_condition = outlier_condition,
    resampling_condition = resampling_condition,
    psd_condition = psd_condition,
    freq_bands = freq_bands,
    run_datetime = format(run_datetime, "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' 解析条件をJSONとして書き出す
write_analysis_conditions_json <- function(conditions, path) {
  jsonlite::write_json(conditions, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(path)
}

#' 解析条件をフラットな1行CSVとして書き出す（ネスト項目はJSON文字列として格納）
write_analysis_conditions_csv <- function(conditions, path) {
  flat <- lapply(conditions, function(v) {
    if (is.list(v) || length(v) > 1L) {
      as.character(jsonlite::toJSON(v, auto_unbox = TRUE))
    } else if (is.null(v)) {
      NA_character_
    } else {
      v
    }
  })
  df <- as.data.frame(flat, stringsAsFactors = FALSE, check.names = FALSE)
  write_csv_utf8(df, path)
  invisible(path)
}

#' UTF-8（BOM付き）でCSVを書き出す（Excel等での日本語文字化け対策）
#'
#' @param df 書き出すdata.frame
#' @param path 出力パス
#' @param bom BOMを付与するか（既定TRUE）
write_csv_utf8 <- function(df, path, bom = TRUE) {
  if (isTRUE(bom)) {
    bin_con <- file(path, open = "wb")
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), bin_con)
    close(bin_con)
    con <- file(path, open = "a", encoding = "UTF-8")
  } else {
    con <- file(path, open = "w", encoding = "UTF-8")
  }
  on.exit(close(con))
  utils::write.csv(df, con, row.names = FALSE)
  invisible(path)
}
