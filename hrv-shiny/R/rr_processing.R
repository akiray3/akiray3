# rr_processing.R
#
# RR間隔生成・外れ値補正・等間隔化を、legacy/recommended共通の標準パイプライン
# （section 5: ピーク検出 → RR生成 → RR補正 → 等間隔化）として提供する。
#
# 重要：この標準パイプラインの処理順序（補正→等間隔化）は、
# R/PulseWaveTools_legacy.R の findHRV2_legacy()（原コードの順序:
# 等間隔化→補正、互換性検証専用）とは異なる。アプリ本体は常にこのファイルの
# run_rr_pipeline() を用いる。legacy/recommendedの違いは「各アルゴリズムの
# 中身」（外れ値検出方式・等間隔化方式）であり、「処理順序」ではない。

#' RR間隔を生成する（ピーク時刻差、後側ピークに対応づけ）
#'
#' legacy/recommendedで同一のアルゴリズムを用いる（doc section 4 findHRV）。
#' @param peaks data.frame(Amplitude, Point, ms)
#' @return data.frame(Point, ms, RR_ms)
generate_rr <- function(peaks) {
  findHRV_legacy(peaks)
}

#' RR外れ値補正（legacy: 逐次median±3SD補正 / recommended: 一括マスク+単回補間）
#'
#' @param rr_values RR間隔ベクトル(ms)
#' @param mode "legacy" または "recommended"
#' @param sd_multiplier SD倍率（既定3）
#' @return list(corrected, outlier_flag, artifact_percent, method)
correct_rr_outliers <- function(rr_values, mode = c("legacy", "recommended"), sd_multiplier = 3) {
  mode <- match.arg(mode)
  n <- length(rr_values)

  if (mode == "legacy") {
    res <- omitOutlier_legacy(rr_values, sd_multiplier = sd_multiplier)
    return(list(
      corrected = res$corrected,
      outlier_flag = res$outlier_flag,
      artifact_percent = res$artifact_percent,
      method = "legacy: 逐次 median±3SD 補正（順序依存、原PulseWaveTools再現）"
    ))
  }

  if (n == 0L) {
    return(list(corrected = numeric(0), outlier_flag = logical(0), artifact_percent = NA_real_,
                method = "recommended: 一括マスク後に単回spline補間"))
  }

  x <- as.numeric(rr_values)
  m <- stats::median(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  mask <- rep(FALSE, n)
  if (!is.na(s) && s > 0) {
    mask <- !is.na(x) & abs(x - m) > sd_multiplier * s
  }

  x_corrected <- x
  x_corrected[mask] <- NA_real_
  if (any(mask)) {
    if (sum(!is.na(x_corrected)) < 2L) {
      stop("spline補間に失敗しました（有効なRRが不足しています）。")
    }
    z <- zoo::zoo(x_corrected)
    x_corrected <- as.numeric(tryCatch(
      zoo::na.spline(z, na.rm = FALSE),
      error = function(e) stop("spline補間に失敗しました。")
    ))
  }

  list(
    corrected = x_corrected,
    outlier_flag = mask,
    artifact_percent = 100 * sum(mask) / n,
    method = "recommended: 一括マスク後に単回spline補間"
  )
}

#' RR系列を等間隔化する（legacy: 1ms格子経由1Hz / recommended: 直接spline）
#'
#' @param event_ms イベント時刻(ms)（後側ピーク時刻）
#' @param values イベント対応値（通常はRR_ms、補正後）
#' @param mode "legacy" または "recommended"
#' @param resample_hz recommendedモードでの再サンプリング周波数(Hz)。NULLなら既定4Hz。
#' @return list(time_sec, value, resample_hz, method)
resample_rr <- function(event_ms, values, mode = c("legacy", "recommended"), resample_hz = NULL) {
  mode <- match.arg(mode)

  if (mode == "legacy") {
    res <- resamplingEvent_legacy(event_ms, values)
    return(list(
      time_sec = res$time_sec,
      value = res$value,
      resample_hz = 1,
      method = "legacy: 1ms格子->zoo::na.spline->1Hz抽出（原PulseWaveTools再現）"
    ))
  }

  if (is.null(resample_hz) || !is.finite(resample_hz) || resample_hz <= 0) {
    resample_hz <- 4
  }
  if (length(event_ms) < 2L) {
    stop("イベント数が不足しているため補間できません。")
  }

  event_sec <- event_ms / 1000
  grid_sec <- seq(event_sec[1], event_sec[length(event_sec)], by = 1 / resample_hz)
  interp <- tryCatch(
    stats::spline(x = event_sec, y = values, xout = grid_sec, method = "natural"),
    error = function(e) NULL
  )
  if (is.null(interp)) {
    stop("spline補間に失敗しました。")
  }

  list(
    time_sec = interp$x,
    value = interp$y,
    resample_hz = resample_hz,
    method = "recommended: イベント時刻から直接spline補間（等間隔格子）"
  )
}

#' 時間領域HRV指標（RR補正後系列から算出、等間隔化前）
#'
#' @param rr_ms RR間隔ベクトル(ms)
#' @return list(mean_rr_ms, mean_hr_bpm, sdnn_ms, rmssd_ms, n_rr_used)
time_domain_metrics <- function(rr_ms) {
  x <- rr_ms[is.finite(rr_ms)]
  if (length(x) == 0L) {
    return(list(mean_rr_ms = NA_real_, mean_hr_bpm = NA_real_, sdnn_ms = NA_real_,
                rmssd_ms = NA_real_, n_rr_used = 0L))
  }
  mean_rr <- mean(x)
  list(
    mean_rr_ms = mean_rr,
    mean_hr_bpm = if (is.finite(mean_rr) && mean_rr > 0) 60000 / mean_rr else NA_real_,
    sdnn_ms = if (length(x) >= 2L) stats::sd(x) else NA_real_,
    rmssd_ms = if (length(x) >= 2L) sqrt(mean(diff(x)^2)) else NA_real_,
    n_rr_used = length(x)
  )
}

#' 標準RR処理パイプライン（ピーク検出後): RR生成 -> 外れ値補正 -> 等間隔化
#'
#' @param peaks data.frame(Amplitude, Point, ms)（解析範囲全体で一度だけ検出したもの）
#' @param mode "legacy" または "recommended"
#' @param sd_multiplier 外れ値補正のSD倍率
#' @param resample_hz recommendedモードの再サンプリング周波数(Hz)
#' @return list(rr, artifact_percent, outlier_method, resampled, resample_method)
run_rr_pipeline <- function(peaks, mode = c("legacy", "recommended"),
                             sd_multiplier = 3, resample_hz = NULL) {
  mode <- match.arg(mode)

  rr <- generate_rr(peaks)
  if (nrow(rr) < 2L) {
    stop("RR間隔が不足しているため解析できません（ピーク数を確認してください）。")
  }

  corr <- correct_rr_outliers(rr$RR_ms, mode = mode, sd_multiplier = sd_multiplier)
  rr$RR_ms_corrected <- corr$corrected
  rr$is_outlier <- corr$outlier_flag

  resampled <- resample_rr(rr$ms, rr$RR_ms_corrected, mode = mode, resample_hz = resample_hz)

  list(
    rr = rr,
    artifact_percent = corr$artifact_percent,
    outlier_method = corr$method,
    resampled = resampled,
    resample_method = resampled$method
  )
}
