# peak_detection.R
#
# legacy / recommended 両モードのピーク検出を統一インターフェースで提供する。
# legacy は R/PulseWaveTools_legacy.R の findPulsePeaks_legacy() を
# そのまま呼び出す（minpeakdistance = 0.5 * samplingRate は変更不可）。
# recommended は最小ピーク距離を明示的な設定項目とし、120bpm超のRRも
# 検出できるよう既定値0.35秒（config/defaults.ymlのpeak_detection.recommended
# .min_peak_distance_sec）を用いる。

#' PPG波形からピークを検出する（legacy/recommended統一インターフェース）
#'
#' @param ppg PPG振幅ベクトル
#' @param samplingRate サンプリング周波数(Hz)
#' @param time_sec 各サンプルの経過秒（NULLならsamplingRateから等間隔生成）
#' @param mode "legacy" または "recommended"
#' @param min_peak_distance_sec recommendedモードでのみ使用。NULLなら既定0.35秒。
#' @param min_peak_height recommendedモードでのみ使用。NULLなら未指定。
#' @return list(peaks = data.frame(Amplitude, Point, ms), params = list(...))
detect_peaks <- function(ppg, samplingRate, time_sec = NULL,
                          mode = c("legacy", "recommended"),
                          min_peak_distance_sec = NULL,
                          min_peak_height = NULL) {
  mode <- match.arg(mode)

  if (length(ppg) < 3L) {
    stop("ピーク検出に必要なデータ点数が不足しています。")
  }
  if (!is.numeric(samplingRate) || is.na(samplingRate) || samplingRate <= 0) {
    stop("サンプリング周波数が不正です。")
  }

  time_ms <- if (!is.null(time_sec)) time_sec * 1000 else NULL

  if (mode == "legacy") {
    peaks <- findPulsePeaks_legacy(ppg, samplingRate, time_ms = time_ms)
    params <- list(
      mode = "legacy",
      min_peak_distance_sec = 0.5,
      min_peak_distance_note = "固定値（PulseWaveTools原仕様: 0.5 * samplingRate）。RR 500ms未満（120bpm超）は検出されない。",
      min_peak_height = NA_real_
    )
    return(list(peaks = peaks, params = params))
  }

  # recommended
  if (is.null(min_peak_distance_sec) || !is.finite(min_peak_distance_sec) || min_peak_distance_sec <= 0) {
    min_peak_distance_sec <- 0.35
  }

  fp_args <- list(
    x = as.numeric(ppg),
    minpeakdistance = min_peak_distance_sec * samplingRate,
    zero = "+",
    sortstr = FALSE
  )
  if (!is.null(min_peak_height) && is.finite(min_peak_height)) {
    fp_args$minpeakheight <- min_peak_height
  }

  pk <- do.call(pracma::findpeaks, fp_args)

  if (is.null(pk) || nrow(pk) == 0L) {
    peaks <- data.frame(Amplitude = numeric(0), Point = integer(0), ms = numeric(0))
  } else {
    peaks <- data.frame(Amplitude = pk[, 1], Point = as.integer(pk[, 2]))
    peaks <- peaks[order(peaks$Point), , drop = FALSE]
    peaks$ms <- if (is.null(time_ms)) {
      (peaks$Point - 1) / samplingRate * 1000
    } else {
      time_ms[peaks$Point]
    }
    rownames(peaks) <- NULL
  }

  params <- list(
    mode = "recommended",
    min_peak_distance_sec = min_peak_distance_sec,
    min_peak_distance_note = "ユーザー設定可能（既定0.35秒 = 約171bpmまで検出可）。",
    min_peak_height = if (is.null(min_peak_height)) NA_real_ else min_peak_height
  )

  list(peaks = peaks, params = params)
}
