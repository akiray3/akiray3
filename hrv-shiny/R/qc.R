# qc.R
#
# 品質管理（QC）判定。閾値は config/defaults.yml に集約し、ソースコード中に
# 散在させない（section 7）。窓ごとに warning_code / warning_message を返し、
# アプリ全体を停止させない（section 9）。

#' config/defaults.yml を読み込む
#' @param path 設定ファイルパス
#' @return list（yaml::read_yamlの結果）
load_app_config <- function(path = "config/defaults.yml") {
  cfg <- yaml::read_yaml(path)
  cfg
}

#' 窓1件分のQC判定を行う
#'
#' 判定するのはアプリが window ごとに算出した数値のみ。ここでは値を書き換えず、
#' warning_code / warning_message / is_valid（Eコードが1つでもあればFALSE）を返す。
#' 呼び出し側（export.R等）は is_valid が FALSE の窓についてLF/HF等をNAにする。
#'
#' @param window_length_sec 窓長（秒）
#' @param n_rr_used 窓内で使用された有効RR数
#' @param n_psd_points_lf, n_psd_points_hf LF/HF帯域内のPSD点数
#' @param artifact_percent 外れ値補正率(%)（NAなら判定対象外）
#' @param LF_power, HF_power 算出されたLF/HFパワー（NAなら未算出）
#' @param out_of_range 窓が記録範囲を超えているか
#' @param config load_app_config() の戻り値
#' @return list(warning_code, warning_message, is_valid)
evaluate_window_qc <- function(window_length_sec, n_rr_used, n_psd_points_lf, n_psd_points_hf,
                                artifact_percent, LF_power, HF_power, out_of_range = FALSE,
                                config) {
  wc <- config$warnings$codes
  codes <- character(0)
  messages <- character(0)

  add_code <- function(code) {
    codes <<- c(codes, code)
    msg <- wc[[code]]
    if (is.null(msg)) msg <- code
    messages <<- c(messages, msg)
  }

  if (isTRUE(out_of_range)) {
    add_code("E_WINDOW_OUT_OF_RANGE")
  }
  if (!is.finite(n_rr_used) || n_rr_used < config$quality$min_rr_count) {
    add_code("E_INSUFFICIENT_RR")
  }
  if (!is.finite(n_psd_points_lf) || n_psd_points_lf < config$quality$min_psd_points_per_band ||
      !is.finite(n_psd_points_hf) || n_psd_points_hf < config$quality$min_psd_points_per_band) {
    add_code("E_INSUFFICIENT_PSD_POINTS")
  }
  if (is.na(LF_power) || is.na(HF_power) || (is.finite(LF_power) && LF_power == 0) ||
      (is.finite(HF_power) && HF_power == 0)) {
    add_code("E_ZERO_OR_NA_BAND")
  }
  if (is.finite(artifact_percent) && artifact_percent > config$quality$max_artifact_percent) {
    add_code("W_HIGH_ARTIFACT")
  }
  if (is.finite(window_length_sec) &&
      window_length_sec <= max(unlist(config$windowing$short_window_thresholds_sec))) {
    add_code("W_SHORT_WINDOW")
  }

  if (length(codes) == 0L) {
    codes <- "OK"
    messages <- wc[["OK"]]
    if (is.null(messages)) messages <- "OK"
  }

  is_valid <- !any(grepl("^E_", codes))

  list(
    warning_code = paste(codes, collapse = ";"),
    warning_message = paste(messages, collapse = " / "),
    is_valid = is_valid
  )
}
