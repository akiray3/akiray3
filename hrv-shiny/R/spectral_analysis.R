# spectral_analysis.R
#
# HRV周波数解析：Welch法によるPSD推定と帯域積分によるLF/HF算出。
# PulseWaveToolsには周波数解析部分が含まれていないため（引き継ぎ書 section 6.2）、
# 本ファイルで明示的に実装する。
#
# 手順（section 6.2）：
#   1. 等間隔RR系列を秒単位へ変換する（rr_unit="sec"時。ms指定も可）
#   2. 窓内で平均を除去する
#   3. Welch法でPSDを求める
#   4. PSD単位を明示する（s^2/Hz または ms^2/Hz）
#   5. LF/HFはPSDを周波数について台形積分する
#
# 周波数境界0.15Hzは二重計上しない：LFは上限排他([0.04,0.15))、
# HFは下限含む上限含む([0.15,0.40])。

#' Hann窓
hann_window <- function(n) {
  if (n <= 1L) return(rep(1, n))
  0.5 - 0.5 * cos(2 * pi * (0:(n - 1)) / (n - 1))
}

#' 次の2のべき乗
next_pow2 <- function(n) {
  if (n <= 1L) return(1L)
  as.integer(2^ceiling(log2(n)))
}

#' Welch法によるPSD推定（片側スペクトル、density スケーリング）
#'
#' 窓長・オーバーラップ・FFT長は引数として固定管理し、呼び出し側で結果に記録する。
#'
#' @param x 数値ベクトル（既に平均除去済みを推奨）
#' @param fs サンプリング周波数(Hz)
#' @param nperseg セグメント長（サンプル数）。NULLならlength(x)（単一セグメント）。
#' @param overlap_ratio セグメント間オーバーラップ比率（0以上1未満）
#' @param nfft FFT長。NULLならnpersegの次の2べき乗（ゼロパディング）。
#' @param window "hann" または "none"
#' @param detrend "constant"（セグメント毎に平均除去） または "none"
#' @return list(freq, psd, nperseg, noverlap, nfft, n_segments, window, detrend)
welch_psd <- function(x, fs, nperseg = NULL, overlap_ratio = 0.5, nfft = NULL,
                       window = "hann", detrend = "none") {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 4L || !is.finite(fs) || fs <= 0) {
    stop("PSD推定に失敗しました（データ点数またはサンプリング周波数が不正です）。")
  }

  if (is.null(nperseg) || !is.finite(nperseg) || nperseg <= 0) {
    nperseg <- n
  }
  nperseg <- min(as.integer(round(nperseg)), n)
  if (nperseg < 4L) {
    stop("PSD推定に失敗しました（セグメント長が短すぎます）。")
  }

  if (is.null(nfft) || !is.finite(nfft) || nfft < nperseg) {
    nfft <- next_pow2(nperseg)
  }
  nfft <- as.integer(nfft)

  overlap_ratio <- min(max(overlap_ratio, 0), 0.95)
  noverlap <- as.integer(round(nperseg * overlap_ratio))
  noverlap <- min(max(noverlap, 0L), nperseg - 1L)
  step <- nperseg - noverlap
  if (step <= 0L) step <- nperseg

  starts <- seq(1L, n - nperseg + 1L, by = step)
  if (length(starts) == 0L) starts <- 1L

  win <- if (identical(window, "hann")) hann_window(nperseg) else rep(1, nperseg)
  win_norm <- sum(win^2) / nperseg

  n_freq <- nfft %/% 2L + 1L
  psd_acc <- rep(0, n_freq)
  n_seg <- 0L

  for (s in starts) {
    seg <- x[s:(s + nperseg - 1L)]
    if (identical(detrend, "constant")) {
      seg <- seg - mean(seg)
    }
    seg <- seg * win
    if (nfft > nperseg) {
      seg <- c(seg, rep(0, nfft - nperseg))
    }
    X <- stats::fft(seg)[1:n_freq]
    p <- (Mod(X)^2) / (fs * nperseg * win_norm)
    if (n_freq > 2L) {
      p[2:(n_freq - 1L)] <- p[2:(n_freq - 1L)] * 2
    }
    if (nfft %% 2L != 0L && n_freq > 1L) {
      p[n_freq] <- p[n_freq] * 2
    }
    psd_acc <- psd_acc + p
    n_seg <- n_seg + 1L
  }

  psd <- psd_acc / n_seg
  freq <- (0:(n_freq - 1L)) * fs / nfft

  list(freq = freq, psd = psd, nperseg = nperseg, noverlap = noverlap,
       nfft = nfft, n_segments = n_seg, window = window, detrend = detrend)
}

#' 周波数帯域のパワーを台形積分で算出する
#'
#' @param freq 周波数ベクトル(Hz)
#' @param psd PSDベクトル
#' @param lower 下限(Hz、含む)
#' @param upper 上限(Hz)
#' @param include_upper 上限を含むか（境界の二重計上を避けるため既定FALSE）
#' @return スカラー（点数不足ならNA）
band_power <- function(freq, psd, lower, upper, include_upper = FALSE) {
  idx <- if (include_upper) {
    freq >= lower & freq <= upper
  } else {
    freq >= lower & freq < upper
  }
  if (sum(idx) < 2L) return(NA_real_)
  pracma::trapz(freq[idx], psd[idx])
}

#' LF/HFとその派生指標を計算する（ゼロ除算はNAとし、Infを出さない）
#'
#' @return list(LF_power, HF_power, LF_HF, HF_norm, LF_norm, LF_percent, HF_percent)
compute_hrv_frequency_indices <- function(freq, psd, lf_lower, lf_upper, hf_lower, hf_upper) {
  LF_power <- band_power(freq, psd, lf_lower, lf_upper, include_upper = FALSE)
  HF_power <- band_power(freq, psd, hf_lower, hf_upper, include_upper = TRUE)

  safe_div <- function(a, b) {
    if (is.na(a) || is.na(b) || b == 0) return(NA_real_)
    a / b
  }

  LF_HF <- safe_div(LF_power, HF_power)
  denom <- if (is.na(LF_power) || is.na(HF_power)) NA_real_ else LF_power + HF_power
  HF_norm <- safe_div(HF_power, denom)
  LF_norm <- safe_div(LF_power, denom)
  LF_percent <- if (is.na(LF_norm)) NA_real_ else 100 * LF_norm
  HF_percent <- if (is.na(HF_norm)) NA_real_ else 100 * HF_norm

  list(
    LF_power = LF_power, HF_power = HF_power,
    LF_HF = LF_HF, HF_norm = HF_norm, LF_norm = LF_norm,
    LF_percent = LF_percent, HF_percent = HF_percent
  )
}

#' 1窓分のPSDおよびHRV周波数指標を算出する（section 6.2 手順の一括実行）
#'
#' @param rr_value_ms 等間隔化されたRR値(ms)（resample_rr()の出力value）
#' @param fs_resample 等間隔化のサンプリング周波数(Hz)
#' @param freq_bands list(lf_lower_hz, lf_upper_hz, hf_lower_hz, hf_upper_hz)
#' @param psd_config list(window_sec, overlap_ratio, nfft)。window_secがNULLなら全区間を単一セグメントとする。
#' @param rr_unit "sec"（既定、PSD単位はs^2/Hz）または"ms"（ms^2/Hz）
#' @return list(freq, psd, psd_unit, psd_method, n_psd_points_lf, n_psd_points_hf, LF_power, HF_power, LF_HF, HF_norm, LF_norm, LF_percent, HF_percent)
analyze_window_psd <- function(rr_value_ms, fs_resample, freq_bands,
                                psd_config = list(window_sec = NULL, overlap_ratio = 0.5, nfft = NULL),
                                rr_unit = c("sec", "ms")) {
  rr_unit <- match.arg(rr_unit)
  x <- as.numeric(rr_value_ms)
  x <- x[is.finite(x)]
  if (length(x) < 4L) {
    stop("PSD推定に失敗しました（等間隔化後のRR点数が不足しています）。")
  }

  if (rr_unit == "sec") {
    x_conv <- x / 1000
    psd_unit <- "s^2/Hz"
  } else {
    x_conv <- x
    psd_unit <- "ms^2/Hz"
  }
  x_demeaned <- x_conv - mean(x_conv)

  nperseg <- if (!is.null(psd_config$window_sec) && is.finite(psd_config$window_sec)) {
    round(psd_config$window_sec * fs_resample)
  } else {
    NULL
  }
  overlap_ratio <- if (!is.null(psd_config$overlap_ratio)) psd_config$overlap_ratio else 0.5
  nfft <- psd_config$nfft

  w <- welch_psd(x_demeaned, fs = fs_resample, nperseg = nperseg, overlap_ratio = overlap_ratio,
                 nfft = nfft, window = "hann", detrend = "none")

  idx <- compute_hrv_frequency_indices(
    w$freq, w$psd,
    lf_lower = freq_bands$lf_lower_hz, lf_upper = freq_bands$lf_upper_hz,
    hf_lower = freq_bands$hf_lower_hz, hf_upper = freq_bands$hf_upper_hz
  )

  n_psd_points_lf <- sum(w$freq >= freq_bands$lf_lower_hz & w$freq < freq_bands$lf_upper_hz)
  n_psd_points_hf <- sum(w$freq >= freq_bands$hf_lower_hz & w$freq <= freq_bands$hf_upper_hz)

  c(
    list(
      freq = w$freq, psd = w$psd, psd_unit = psd_unit,
      psd_method = sprintf(
        "welch(nperseg=%d,noverlap=%d,nfft=%d,window=%s,n_segments=%d)",
        w$nperseg, w$noverlap, w$nfft, w$window, w$n_segments
      ),
      n_psd_points_lf = n_psd_points_lf,
      n_psd_points_hf = n_psd_points_hf
    ),
    idx
  )
}
