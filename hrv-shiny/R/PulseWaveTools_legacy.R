# PulseWaveTools_legacy.R
#
# 引き継ぎ書 section 4 に記載された既存 PulseWaveTools のアルゴリズムを
# 忠実に再現したラッパー群。
#
# 注意（重要）：本セッションの環境およびリポジトリには PulseWaveTools
# パッケージの原本ソースが存在しなかった（添付されたのは引き継ぎ書のみ）。
# そのため、本ファイルは引き継ぎ書 section 4 に明記されたアルゴリズム仕様
# （findPulsePeaks の呼び出しコードは原文ママ、findHRV/resamplingEvent/
# omitOutlier/findHRV2 は文書中のアルゴリズム記述）を「legacy」として
# 実装したものである。実際の PulseWaveTools パッケージのソースが入手でき
# 次第、tests/testthat/test-legacy-compatibility.R で数値比較し、差異が
# あれば本ファイルを修正すること。原コードの問題点（omitOutlier の順序
# 依存性など）はここでは無条件に修正せず、忠実再現を優先する。
#
# 改良版（recommended モード）の実装は本ファイルに置かず、
# R/peak_detection.R, R/rr_processing.R に分離する。

#' ピーク検出（legacy: PulseWaveTools::findPulsePeaks 相当）
#'
#' @param dat 数値ベクトルまたは1列の行列/データフレーム（PPG振幅）
#' @param samplingRate サンプリング周波数 (Hz)
#' @param time_ms 各サンプルの時刻(ms)。NULL の場合は samplingRate から等間隔に生成。
#' @return data.frame(Amplitude, Point, ms) を Point 昇順に並べたもの
findPulsePeaks_legacy <- function(dat, samplingRate, time_ms = NULL) {
  if (is.null(dim(dat))) {
    dat <- matrix(dat, ncol = 1)
  }
  if (nrow(dat) < 3) {
    stop("findPulsePeaks_legacy: データ点数が不足しています。")
  }
  if (!is.numeric(samplingRate) || is.na(samplingRate) || samplingRate <= 0) {
    stop("findPulsePeaks_legacy: samplingRate が不正です。")
  }

  pk <- pracma::findpeaks(
    as.matrix(dat)[, 1],
    minpeakdistance = 0.5 * samplingRate,
    zero = "+",
    sortstr = FALSE
  )

  if (is.null(pk) || nrow(pk) == 0L) {
    return(data.frame(Amplitude = numeric(0), Point = integer(0), ms = numeric(0)))
  }

  out <- data.frame(
    Amplitude = pk[, 1],
    Point = as.integer(pk[, 2])
  )
  # Point順に並べる
  out <- out[order(out$Point), , drop = FALSE]

  if (is.null(time_ms)) {
    out$ms <- (out$Point - 1) / samplingRate * 1000
  } else {
    out$ms <- time_ms[out$Point]
  }

  rownames(out) <- NULL
  out
}

#' RR間隔算出（legacy: PulseWaveTools::findHRV 相当）
#'
#' ピーク時刻差からRR間隔(ms)を算出する。RRは後側のピーク時刻に対応づける。
#'
#' @param peaks findPulsePeaks_legacy() の出力（Point昇順）
#' @return data.frame(Point, ms, RR_ms)。先頭点はRRが定義できないため除外。
findHRV_legacy <- function(peaks) {
  if (nrow(peaks) < 2L) {
    return(data.frame(Point = integer(0), ms = numeric(0), RR_ms = numeric(0)))
  }
  ms <- peaks$ms
  rr <- diff(ms)
  data.frame(
    Point = peaks$Point[-1],
    ms = peaks$ms[-1],
    RR_ms = rr
  )
}

#' RRイベントの等間隔化（legacy: PulseWaveTools::resamplingEvent 相当）
#'
#' 原アルゴリズム：RRイベントを1ms格子に配置し zoo::na.spline() で補間した後、
#' 1Hzで間引いて抽出する。メモリ効率が悪いことを承知の上で忠実再現する。
#'
#' @param event_ms イベント時刻(ms)（後側ピーク時刻。findHRV_legacy()$ms）
#' @param values イベントに対応する値（通常はRR_ms）
#' @return list(time_sec, value) 1Hzで抽出した等間隔系列
resamplingEvent_legacy <- function(event_ms, values) {
  if (length(event_ms) != length(values)) {
    stop("resamplingEvent_legacy: event_ms と values の長さが一致しません。")
  }
  if (length(event_ms) < 2L) {
    stop("resamplingEvent_legacy: イベント数が不足しているため補間できません。")
  }

  grid_ms <- seq(floor(min(event_ms)), ceiling(max(event_ms)), by = 1)

  # 1ms格子上にイベント値を配置（それ以外はNA）
  grid_value <- rep(NA_real_, length(grid_ms))
  idx <- round(event_ms - grid_ms[1]) + 1L
  idx <- pmin(pmax(idx, 1L), length(grid_ms))
  grid_value[idx] <- values

  z <- zoo::zoo(grid_value, order.by = grid_ms)
  interp <- tryCatch(
    zoo::na.spline(z, na.rm = FALSE),
    error = function(e) NULL
  )
  if (is.null(interp)) {
    stop("resamplingEvent_legacy: spline補間に失敗しました。")
  }
  interp_value <- as.numeric(interp)

  # 1Hzで抽出（1ms格子なので1000点ごと）
  step <- 1000L
  sel <- seq(1L, length(grid_ms), by = step)
  list(
    time_sec = (grid_ms[sel] - grid_ms[1]) / 1000,
    value = interp_value[sel]
  )
}

#' RR外れ値補正（legacy: PulseWaveTools::omitOutlier 相当）
#'
#' 原アルゴリズムは median ± 3SD を超えるRRを逐次NA化・spline補完し、
#' ループ中に平均・SDを再計算するため補正順序に依存する（忠実再現）。
#'
#' @param rr_values RR間隔の数値ベクトル（ms または 他単位、順序を保持）
#' @param sd_multiplier SD倍率（既定3）
#' @return list(corrected, outlier_flag, artifact_percent)
omitOutlier_legacy <- function(rr_values, sd_multiplier = 3) {
  x <- as.numeric(rr_values)
  n <- length(x)
  if (n == 0L) {
    return(list(corrected = numeric(0), outlier_flag = logical(0), artifact_percent = NA_real_))
  }
  outlier_flag <- rep(FALSE, n)

  for (i in seq_len(n)) {
    if (is.na(x[i])) next
    m <- stats::median(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) next
    if (abs(x[i] - m) > sd_multiplier * s) {
      x[i] <- NA_real_
      outlier_flag[i] <- TRUE
      if (sum(!is.na(x)) >= 2L) {
        z <- zoo::zoo(x)
        x <- as.numeric(tryCatch(zoo::na.spline(z, na.rm = FALSE), error = function(e) z))
      }
    }
  }

  list(
    corrected = x,
    outlier_flag = outlier_flag,
    artifact_percent = 100 * sum(outlier_flag) / n
  )
}

#' legacy互換フロー一括実行（互換性検証専用）
#'
#' PPG → findHRV() → resamplingEvent() → omitOutlier() の順（原コードの順序）。
#' この順序は section 5 で定めるアプリ標準パイプライン（RR補正→等間隔化）とは
#' 異なる点に注意。findHRV2_legacy() はあくまで legacy 互換性検証
#' （tests/testthat/test-legacy-compatibility.R）専用であり、Shiny本体の
#' 解析には R/rr_processing.R の標準パイプラインを用いること。
#'
#' @param dat PPG振幅ベクトル
#' @param samplingRate サンプリング周波数(Hz)
#' @param time_ms 各サンプル時刻(ms)。NULLならsamplingRateから生成。
#' @param sd_multiplier omitOutlier の SD倍率
#' @return list(peaks, rr, resampled, corrected)
findHRV2_legacy <- function(dat, samplingRate, time_ms = NULL, sd_multiplier = 3) {
  peaks <- findPulsePeaks_legacy(dat, samplingRate, time_ms)
  rr <- findHRV_legacy(peaks)
  resampled <- resamplingEvent_legacy(rr$ms, rr$RR_ms)
  corrected <- omitOutlier_legacy(resampled$value, sd_multiplier = sd_multiplier)

  list(
    peaks = peaks,
    rr = rr,
    resampled = resampled,
    corrected = corrected
  )
}
