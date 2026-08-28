# app.R — 耳朶容積脈波(PPG) HRV解析Shinyアプリ（単一ファイル構成）
#
# 管理を容易にするため、解析ロジック・UI・サーバーを1ファイルにまとめている。
# セクション見出し（## ==== ... ====）で区切って整理する。
#
# 入力ファイルの形式はShimmer出力（sep=行・ヘッダ・単位行つきTSV/CSV）に統一
# されているため、時刻列・PPG列はKNOWN_TIME_COLS/KNOWN_PPG_COLSから自動判定
# する（ユーザーに列を選ばせるUIは持たない）。解析パイプラインはrecommended
# （改良版）のみを実装する。

library(shiny)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

## ==== 設定読込 =========================================================

load_app_config <- function(path = "config/defaults.yml") yaml::read_yaml(path)

APP_CONFIG <- load_app_config(file.path(getwd(), "config", "defaults.yml"))
APP_VERSION <- APP_CONFIG$app$version %||% "0.1.0"

KNOWN_TIME_COLS <- c("Shimmer__TimestampSync_Unix_CAL", "Shimmer_TimestampSync_Unix_CAL", "Timestamp_Unix_CAL")
KNOWN_PPG_COLS <- c("Shimmer__PPG_A13_CAL", "Shimmer_PPG_A13_CAL", "PPG_A13_CAL")

## ==== データ読込（Shimmer CSV/TSV） ====================================
# sep=行/単位行の除外、区切り文字自動判定、ヘッダなし2列形式にも対応（section 2）。

detect_shimmer_format <- function(path, n_peek = 10L) {
  if (!file.exists(path)) stop("ファイルが見つかりません。")
  raw_lines <- readLines(path, n = n_peek, warn = FALSE, encoding = "UTF-8")
  raw_lines <- raw_lines[!is.na(raw_lines) & nzchar(trimws(raw_lines))]
  if (length(raw_lines) == 0L) stop("ファイルを解釈できません（空ファイル、または読み込めません）。")

  # Excel等で再保存されたファイルは "sep=," のように先頭行が二重引用符で
  # 囲まれることがあるため、判定前に前後の引用符を取り除く。
  strip_quotes <- function(x) sub('^"(.*)"$', "\\1", x)

  skip_lines <- 0L
  sep_declared <- NA_character_
  first_line <- strip_quotes(raw_lines[1])
  if (grepl("^sep=", first_line, ignore.case = TRUE)) {
    sep_char <- sub("^sep=", "", first_line, ignore.case = TRUE)
    if (nchar(sep_char) >= 1L) sep_declared <- substr(sep_char, 1, 1)
    skip_lines <- 1L
    raw_lines <- raw_lines[-1]
  }
  if (length(raw_lines) == 0L) stop("ファイルを解釈できません（ヘッダ行が見つかりません）。")

  candidate_delims <- c("\t", ",", ";")
  if (!is.na(sep_declared) && sep_declared %in% candidate_delims) {
    candidate_delims <- unique(c(sep_declared, candidate_delims))
  }

  header_line <- raw_lines[1]
  count_fields <- function(line, delim) length(strsplit(line, delim, fixed = TRUE)[[1]])
  field_counts <- vapply(candidate_delims, function(d) count_fields(header_line, d), integer(1))
  delim <- candidate_delims[which.max(field_counts)]
  if (max(field_counts) < 2L) stop("ファイルを解釈できません（タブ・カンマ・セミコロンいずれでも列を分割できません）。")

  is_numeric_token <- function(x) !is.na(suppressWarnings(as.numeric(x))) & nchar(x) > 0
  header_fields <- strip_quotes(trimws(strsplit(header_line, delim, fixed = TRUE)[[1]]))

  if (all(is_numeric_token(header_fields))) {
    # ヘッダなし2列（以上）形式：ヘッダ・単位行なしとみなす
    return(list(delim = delim, skip_lines = skip_lines, header = FALSE, has_unit_row = FALSE,
                col_names = paste0("V", seq_along(header_fields)), n_fields = length(header_fields)))
  }

  has_unit_row <- FALSE
  if (length(raw_lines) >= 2L) {
    unit_fields <- trimws(strsplit(raw_lines[2], delim, fixed = TRUE)[[1]])
    unit_nonnumeric <- length(unit_fields) == length(header_fields) && !all(is_numeric_token(unit_fields))
    if (unit_nonnumeric && length(raw_lines) >= 3L) {
      data_fields <- trimws(strsplit(raw_lines[3], delim, fixed = TRUE)[[1]])
      has_unit_row <- length(data_fields) == length(header_fields) && all(is_numeric_token(data_fields))
    } else if (unit_nonnumeric) {
      has_unit_row <- TRUE
    }
  }

  list(delim = delim, skip_lines = skip_lines, header = TRUE, has_unit_row = has_unit_row,
       col_names = header_fields, n_fields = length(header_fields))
}

read_shimmer_csv <- function(path) {
  fmt <- detect_shimmer_format(path)
  skip_total <- fmt$skip_lines + (if (isTRUE(fmt$header)) 1L else 0L) + (if (isTRUE(fmt$has_unit_row)) 1L else 0L)

  dt <- tryCatch(
    data.table::fread(file = path, sep = fmt$delim, skip = skip_total, header = FALSE,
                       data.table = TRUE, showProgress = FALSE, na.strings = c("", "NA", "NaN", "N/A")),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0L) stop("ファイルを解釈できません。区切り文字またはヘッダ構造を確認してください。")

  n_use <- min(ncol(dt), length(fmt$col_names))
  dt <- dt[, seq_len(n_use), with = FALSE]
  data.table::setnames(dt, fmt$col_names[seq_len(n_use)])
  list(data = dt, format = fmt)
}

guess_time_column <- function(col_names) {
  hit <- col_names[col_names %in% KNOWN_TIME_COLS]
  if (length(hit) > 0L) return(hit[1])
  hit2 <- col_names[grepl("timestamp", col_names, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[1])
  col_names[1]
}

guess_ppg_column <- function(col_names) {
  hit <- col_names[col_names %in% KNOWN_PPG_COLS]
  if (length(hit) > 0L) return(hit[1])
  hit2 <- col_names[grepl("ppg", col_names, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[1])
  col_names[min(2L, length(col_names))]
}

normalize_time_sec <- function(timestamp_ms) {
  timestamp_ms <- as.numeric(timestamp_ms)
  finite_idx <- which(is.finite(timestamp_ms))
  if (length(finite_idx) == 0L) stop("タイムスタンプに有効な値がありません。")
  t0_ms <- timestamp_ms[finite_idx[1]]
  # t0を先に引いてから秒に変換し、大きな数値の桁落ちを避ける
  list(time_sec = (timestamp_ms - t0_ms) / 1000, t0_ms = t0_ms)
}

estimate_sampling_rate <- function(time_sec) {
  d_pos <- diff(time_sec)
  d_pos <- d_pos[is.finite(d_pos) & d_pos > 0]
  if (length(d_pos) == 0L) return(list(fs_hz = NA_real_, dt_sec = NA_real_))
  dt_sec <- stats::median(d_pos)
  list(fs_hz = 1 / dt_sec, dt_sec = dt_sec)
}

diagnose_timestamps <- function(time_sec, large_gap_factor = 5) {
  d <- diff(time_sec)
  dt_pos <- d[is.finite(d) & d > 0]
  dt_median_sec <- if (length(dt_pos) > 0L) stats::median(dt_pos) else NA_real_
  non_finite_index <- which(!is.finite(time_sec))
  non_monotonic_index <- which(is.finite(d) & d <= 0) + 1L
  duplicated_index <- which(is.finite(d) & abs(d) < 1e-9) + 1L
  large_gap_index <- if (is.finite(dt_median_sec) && dt_median_sec > 0) {
    which(is.finite(d) & d > dt_median_sec * large_gap_factor) + 1L
  } else integer(0)

  list(n_samples = length(time_sec), n_non_finite = length(non_finite_index), non_finite_index = non_finite_index,
       n_non_monotonic = length(non_monotonic_index), non_monotonic_index = non_monotonic_index,
       n_duplicated = length(duplicated_index), duplicated_index = duplicated_index,
       n_large_gaps = length(large_gap_index), large_gap_index = large_gap_index,
       dt_median_sec = dt_median_sec, is_monotonic = length(non_monotonic_index) == 0L)
}

import_shimmer_file <- function(path, time_col = NULL, ppg_col = NULL, fs_override = NULL) {
  parsed <- read_shimmer_csv(path)
  dt <- parsed$data
  col_names <- names(dt)
  if (is.null(time_col)) time_col <- guess_time_column(col_names)
  if (is.null(ppg_col)) ppg_col <- guess_ppg_column(col_names)
  if (!(time_col %in% col_names)) stop(sprintf("時刻列 '%s' が見つかりません。", time_col))
  if (!(ppg_col %in% col_names)) stop(sprintf("PPG列 '%s' が見つかりません。", ppg_col))

  timestamp_raw <- suppressWarnings(as.numeric(dt[[time_col]]))
  ppg_raw <- suppressWarnings(as.numeric(dt[[ppg_col]]))
  if (all(is.na(timestamp_raw))) stop(sprintf("指定列 '%s' が数値ではありません。", time_col))
  if (all(is.na(ppg_raw))) stop(sprintf("指定列 '%s' が数値ではありません。", ppg_col))

  norm <- normalize_time_sec(timestamp_raw)
  fs_est <- estimate_sampling_rate(norm$time_sec)
  diag <- diagnose_timestamps(norm$time_sec)

  fs_used <- fs_est$fs_hz
  fs_source <- "estimated"
  if (!is.null(fs_override) && is.finite(fs_override) && fs_override > 0) {
    fs_used <- fs_override
    fs_source <- "manual"
  }

  warnings_jp <- character(0)
  if (!is.finite(fs_used)) warnings_jp <- c(warnings_jp, "サンプリング周波数を推定できません。手動で指定してください。")
  if (!diag$is_monotonic) warnings_jp <- c(warnings_jp, sprintf("タイムスタンプが単調増加ではありません（%d 箇所）。", diag$n_non_monotonic))
  if (diag$n_duplicated > 0L) warnings_jp <- c(warnings_jp, sprintf("タイムスタンプの重複が %d 箇所あります。", diag$n_duplicated))
  if (diag$n_large_gaps > 0L) warnings_jp <- c(warnings_jp, sprintf("サンプリング間隔の中央値の5倍を超える大きな欠落が %d 箇所あります。", diag$n_large_gaps))
  if (diag$n_non_finite > 0L) warnings_jp <- c(warnings_jp, sprintf("非有限のタイムスタンプが %d 箇所あります。", diag$n_non_finite))

  list(time_sec = norm$time_sec, ppg = ppg_raw, t0_ms = norm$t0_ms, fs_hz = fs_used,
       fs_estimated_hz = fs_est$fs_hz, dt_median_sec = fs_est$dt_sec, fs_source = fs_source,
       diagnostics = diag, format = parsed$format,
       columns = list(all = col_names, time_col = time_col, ppg_col = ppg_col),
       warnings = warnings_jp, n_samples = nrow(dt))
}

## ==== ピーク検出・RR処理 ================================================
# ピーク検出(pracma::findpeaks) → RR生成 → 外れ値補正（一括マスク後に単回
# spline補間） → 等間隔化（直接spline補間）の順で実行する標準パイプライン。

detect_peaks <- function(ppg, samplingRate, time_sec = NULL, min_peak_distance_sec = NULL, min_peak_height = NULL) {
  if (length(ppg) < 3L) stop("ピーク検出に必要なデータ点数が不足しています。")
  if (!is.numeric(samplingRate) || is.na(samplingRate) || samplingRate <= 0) stop("サンプリング周波数が不正です。")
  if (is.null(min_peak_distance_sec) || !is.finite(min_peak_distance_sec) || min_peak_distance_sec <= 0) {
    min_peak_distance_sec <- 0.35
  }

  time_ms <- if (!is.null(time_sec)) time_sec * 1000 else NULL
  fp_args <- list(x = as.numeric(ppg), minpeakdistance = min_peak_distance_sec * samplingRate, zero = "+", sortstr = FALSE)
  if (!is.null(min_peak_height) && is.finite(min_peak_height)) fp_args$minpeakheight <- min_peak_height
  pk <- do.call(pracma::findpeaks, fp_args)

  if (is.null(pk) || nrow(pk) == 0L) {
    peaks <- data.frame(Amplitude = numeric(0), Point = integer(0), ms = numeric(0))
  } else {
    peaks <- data.frame(Amplitude = pk[, 1], Point = as.integer(pk[, 2]))
    peaks <- peaks[order(peaks$Point), , drop = FALSE]
    peaks$ms <- if (is.null(time_ms)) (peaks$Point - 1) / samplingRate * 1000 else time_ms[peaks$Point]
    rownames(peaks) <- NULL
  }

  list(peaks = peaks, params = list(min_peak_distance_sec = min_peak_distance_sec,
                                     min_peak_height = min_peak_height %||% NA_real_))
}

generate_rr <- function(peaks) {
  if (nrow(peaks) < 2L) return(data.frame(Point = integer(0), ms = numeric(0), RR_ms = numeric(0)))
  # RRは後側のピーク時刻に対応づける
  data.frame(Point = peaks$Point[-1], ms = peaks$ms[-1], RR_ms = diff(peaks$ms))
}

correct_rr_outliers <- function(rr_values, sd_multiplier = 3) {
  x <- as.numeric(rr_values)
  n <- length(x)
  if (n == 0L) return(list(corrected = numeric(0), outlier_flag = logical(0), artifact_percent = NA_real_))

  m <- stats::median(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  mask <- if (!is.na(s) && s > 0) !is.na(x) & abs(x - m) > sd_multiplier * s else rep(FALSE, n)

  x_corrected <- x
  x_corrected[mask] <- NA_real_
  if (any(mask)) {
    if (sum(!is.na(x_corrected)) < 2L) stop("spline補間に失敗しました（有効なRRが不足しています）。")
    x_corrected <- as.numeric(tryCatch(zoo::na.spline(zoo::zoo(x_corrected), na.rm = FALSE),
                                        error = function(e) stop("spline補間に失敗しました。")))
  }
  list(corrected = x_corrected, outlier_flag = mask, artifact_percent = 100 * sum(mask) / n)
}

resample_rr <- function(event_ms, values, resample_hz = NULL) {
  if (is.null(resample_hz) || !is.finite(resample_hz) || resample_hz <= 0) resample_hz <- 4
  if (length(event_ms) < 2L) stop("イベント数が不足しているため補間できません。")

  event_sec <- event_ms / 1000
  grid_sec <- seq(event_sec[1], event_sec[length(event_sec)], by = 1 / resample_hz)
  interp <- tryCatch(stats::spline(x = event_sec, y = values, xout = grid_sec, method = "natural"), error = function(e) NULL)
  if (is.null(interp)) stop("spline補間に失敗しました。")
  list(time_sec = interp$x, value = interp$y, resample_hz = resample_hz)
}

time_domain_metrics <- function(rr_ms) {
  x <- rr_ms[is.finite(rr_ms)]
  if (length(x) == 0L) {
    return(list(mean_rr_ms = NA_real_, mean_hr_bpm = NA_real_, sdnn_ms = NA_real_, rmssd_ms = NA_real_, n_rr_used = 0L))
  }
  mean_rr <- mean(x)
  list(mean_rr_ms = mean_rr, mean_hr_bpm = if (is.finite(mean_rr) && mean_rr > 0) 60000 / mean_rr else NA_real_,
       sdnn_ms = if (length(x) >= 2L) stats::sd(x) else NA_real_,
       rmssd_ms = if (length(x) >= 2L) sqrt(mean(diff(x)^2)) else NA_real_, n_rr_used = length(x))
}

run_rr_pipeline <- function(peaks, sd_multiplier = 3, resample_hz = NULL) {
  rr <- generate_rr(peaks)
  if (nrow(rr) < 2L) stop("RR間隔が不足しているため解析できません（ピーク数を確認してください）。")

  corr <- correct_rr_outliers(rr$RR_ms, sd_multiplier = sd_multiplier)
  rr$RR_ms_corrected <- corr$corrected
  rr$is_outlier <- corr$outlier_flag
  resampled <- resample_rr(rr$ms, rr$RR_ms_corrected, resample_hz = resample_hz)

  list(rr = rr, artifact_percent = corr$artifact_percent, resampled = resampled)
}

## ==== 周波数解析（Welch PSD・帯域積分・HRV周波数指標） ==================
# PulseWaveToolsに周波数解析部分はないため独自実装（section 6.2）。
# 境界0.15Hzは二重計上しない：LFは上限排他[0.04,0.15)、HFは両端含む[0.15,0.40]。

hann_window <- function(n) if (n <= 1L) rep(1, n) else 0.5 - 0.5 * cos(2 * pi * (0:(n - 1)) / (n - 1))
next_pow2 <- function(n) if (n <= 1L) 1L else as.integer(2^ceiling(log2(n)))

welch_psd <- function(x, fs, nperseg = NULL, overlap_ratio = 0.5, nfft = NULL, window = "hann", detrend = "none") {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 4L || !is.finite(fs) || fs <= 0) stop("PSD推定に失敗しました（データ点数またはサンプリング周波数が不正です）。")

  if (is.null(nperseg) || !is.finite(nperseg) || nperseg <= 0) nperseg <- n
  nperseg <- min(as.integer(round(nperseg)), n)
  if (nperseg < 4L) stop("PSD推定に失敗しました（セグメント長が短すぎます）。")

  if (is.null(nfft) || !is.finite(nfft) || nfft < nperseg) nfft <- next_pow2(nperseg)
  nfft <- as.integer(nfft)

  overlap_ratio <- min(max(overlap_ratio, 0), 0.95)
  noverlap <- min(max(as.integer(round(nperseg * overlap_ratio)), 0L), nperseg - 1L)
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
    if (identical(detrend, "constant")) seg <- seg - mean(seg)
    seg <- seg * win
    if (nfft > nperseg) seg <- c(seg, rep(0, nfft - nperseg))
    X <- stats::fft(seg)[1:n_freq]
    p <- (Mod(X)^2) / (fs * nperseg * win_norm)
    if (n_freq > 2L) p[2:(n_freq - 1L)] <- p[2:(n_freq - 1L)] * 2   # 片側スペクトルへの倍化（DC/Nyquist以外）
    if (nfft %% 2L != 0L && n_freq > 1L) p[n_freq] <- p[n_freq] * 2 # nfft奇数はNyquist厳密には存在しないため最終binも倍化
    psd_acc <- psd_acc + p
    n_seg <- n_seg + 1L
  }

  list(freq = (0:(n_freq - 1L)) * fs / nfft, psd = psd_acc / n_seg, nperseg = nperseg, noverlap = noverlap,
       nfft = nfft, n_segments = n_seg, window = window, detrend = detrend)
}

band_power <- function(freq, psd, lower, upper, include_upper = FALSE) {
  idx <- if (include_upper) freq >= lower & freq <= upper else freq >= lower & freq < upper
  if (sum(idx) < 2L) return(NA_real_)
  pracma::trapz(freq[idx], psd[idx])
}

compute_hrv_frequency_indices <- function(freq, psd, lf_lower, lf_upper, hf_lower, hf_upper) {
  LF_power <- band_power(freq, psd, lf_lower, lf_upper, include_upper = FALSE)
  HF_power <- band_power(freq, psd, hf_lower, hf_upper, include_upper = TRUE)

  safe_div <- function(a, b) if (is.na(a) || is.na(b) || b == 0) NA_real_ else a / b  # 0除算はNA（Infにしない）
  LF_HF <- safe_div(LF_power, HF_power)
  denom <- if (is.na(LF_power) || is.na(HF_power)) NA_real_ else LF_power + HF_power
  HF_norm <- safe_div(HF_power, denom)
  LF_norm <- safe_div(LF_power, denom)

  list(LF_power = LF_power, HF_power = HF_power, LF_HF = LF_HF, HF_norm = HF_norm, LF_norm = LF_norm,
       LF_percent = if (is.na(LF_norm)) NA_real_ else 100 * LF_norm,
       HF_percent = if (is.na(HF_norm)) NA_real_ else 100 * HF_norm)
}

analyze_window_psd <- function(rr_value_ms, fs_resample, freq_bands,
                                psd_config = list(window_sec = NULL, overlap_ratio = 0.5, nfft = NULL),
                                rr_unit = c("sec", "ms")) {
  rr_unit <- match.arg(rr_unit)
  x <- as.numeric(rr_value_ms)
  x <- x[is.finite(x)]
  if (length(x) < 4L) stop("PSD推定に失敗しました（等間隔化後のRR点数が不足しています）。")

  if (rr_unit == "sec") { x_conv <- x / 1000; psd_unit <- "s^2/Hz" } else { x_conv <- x; psd_unit <- "ms^2/Hz" }
  x_demeaned <- x_conv - mean(x_conv)

  nperseg <- if (!is.null(psd_config$window_sec) && is.finite(psd_config$window_sec)) round(psd_config$window_sec * fs_resample) else NULL
  overlap_ratio <- if (!is.null(psd_config$overlap_ratio)) psd_config$overlap_ratio else 0.5

  w <- welch_psd(x_demeaned, fs = fs_resample, nperseg = nperseg, overlap_ratio = overlap_ratio,
                 nfft = psd_config$nfft, window = "hann", detrend = "none")
  idx <- compute_hrv_frequency_indices(w$freq, w$psd, freq_bands$lf_lower_hz, freq_bands$lf_upper_hz,
                                        freq_bands$hf_lower_hz, freq_bands$hf_upper_hz)

  c(list(freq = w$freq, psd = w$psd, psd_unit = psd_unit,
         psd_method = sprintf("welch(nperseg=%d,noverlap=%d,nfft=%d,window=%s,n_segments=%d)",
                               w$nperseg, w$noverlap, w$nfft, w$window, w$n_segments),
         n_psd_points_lf = sum(w$freq >= freq_bands$lf_lower_hz & w$freq < freq_bands$lf_upper_hz),
         n_psd_points_hf = sum(w$freq >= freq_bands$hf_lower_hz & w$freq <= freq_bands$hf_upper_hz)),
    idx)
}

## ==== QC（品質管理判定） ================================================
# 閾値はconfig/defaults.ymlに集約（section 7）。窓ごとにwarning_codeを返し
# アプリ全体は止めない（section 9）。Eコードが1つでもあればis_valid=FALSE。

evaluate_window_qc <- function(window_length_sec, n_rr_used, n_psd_points_lf, n_psd_points_hf,
                                artifact_percent, LF_power, HF_power, out_of_range = FALSE, config) {
  wc <- config$warnings$codes
  codes <- character(0)
  messages <- character(0)
  add_code <- function(code) {
    codes <<- c(codes, code)
    messages <<- c(messages, wc[[code]] %||% code)
  }

  if (isTRUE(out_of_range)) add_code("E_WINDOW_OUT_OF_RANGE")
  if (!is.finite(n_rr_used) || n_rr_used < config$quality$min_rr_count) add_code("E_INSUFFICIENT_RR")
  if (!is.finite(n_psd_points_lf) || n_psd_points_lf < config$quality$min_psd_points_per_band ||
      !is.finite(n_psd_points_hf) || n_psd_points_hf < config$quality$min_psd_points_per_band) add_code("E_INSUFFICIENT_PSD_POINTS")
  if (is.na(LF_power) || is.na(HF_power) || (is.finite(LF_power) && LF_power == 0) ||
      (is.finite(HF_power) && HF_power == 0)) add_code("E_ZERO_OR_NA_BAND")
  if (is.finite(artifact_percent) && artifact_percent > config$quality$max_artifact_percent) add_code("W_HIGH_ARTIFACT")
  if (is.finite(window_length_sec) && window_length_sec <= max(unlist(config$windowing$short_window_thresholds_sec))) add_code("W_SHORT_WINDOW")

  if (length(codes) == 0L) { codes <- "OK"; messages <- wc[["OK"]] %||% "OK" }
  list(warning_code = paste(codes, collapse = ";"), warning_message = paste(messages, collapse = " / "),
       is_valid = !any(grepl("^E_", codes)))
}

## ==== 出力（結果CSV・ピーク/RR CSV・解析条件CSV/JSON） ==================
# section 8。データ整形とファイル書き出しのみ（解析ロジックは持たない）。

RESULT_CSV_COLUMNS <- c(
  "file_name", "analysis_id", "analysis_mode", "window_id", "window_start_sec", "window_end_sec",
  "window_length_sec", "step_sec", "sampling_rate_raw_hz", "resampling_rate_rr_hz",
  "n_raw_samples", "n_peaks", "n_rr_raw", "n_rr_used", "mean_rr_ms", "mean_hr_bpm", "sdnn_ms", "rmssd_ms",
  "LF_power", "HF_power", "LF_HF", "HF_norm", "LF_norm", "LF_percent", "HF_percent",
  "lf_lower_hz", "lf_upper_hz", "hf_lower_hz", "hf_upper_hz", "psd_method",
  "artifact_count", "artifact_percent", "warning_code", "warning_message"
)

build_result_row <- function(...) {
  args <- list(...)
  row <- as.list(rep(NA, length(RESULT_CSV_COLUMNS)))
  names(row) <- RESULT_CSV_COLUMNS
  unknown <- setdiff(names(args), RESULT_CSV_COLUMNS)
  if (length(unknown) > 0L) stop(sprintf("build_result_row: 未知の列名です: %s", paste(unknown, collapse = ", ")))
  row[names(args)] <- args
  as.data.frame(row, stringsAsFactors = FALSE)
}

combine_result_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(as.data.frame(setNames(replicate(length(RESULT_CSV_COLUMNS), character(0), simplify = FALSE), RESULT_CSV_COLUMNS)))
  }
  do.call(rbind, rows)[, RESULT_CSV_COLUMNS, drop = FALSE]
}

build_peak_rr_table <- function(peaks, rr_df) {
  base <- data.frame(sample_index = peaks$Point, peak_time_sec = peaks$ms / 1000, amplitude = peaks$Amplitude,
                      stringsAsFactors = FALSE)
  rr_map <- data.frame(sample_index = rr_df$Point, raw_RR_ms = rr_df$RR_ms, artifact_flag = rr_df$is_outlier,
                        corrected_RR_ms = rr_df$RR_ms_corrected, stringsAsFactors = FALSE)
  merged <- merge(base, rr_map, by = "sample_index", all.x = TRUE)
  merged <- merged[order(merged$sample_index), c("peak_time_sec", "sample_index", "amplitude",
                                                   "raw_RR_ms", "artifact_flag", "corrected_RR_ms")]
  rownames(merged) <- NULL
  merged
}

build_analysis_conditions <- function(app_version, input_columns, sampling_rate_hz,
                                       peak_condition, outlier_condition, resampling_condition, psd_condition,
                                       freq_bands, run_datetime = Sys.time()) {
  list(app_version = app_version, input_columns = input_columns,
       sampling_rate_hz = sampling_rate_hz, peak_condition = peak_condition, outlier_condition = outlier_condition,
       resampling_condition = resampling_condition, psd_condition = psd_condition, freq_bands = freq_bands,
       run_datetime = format(run_datetime, "%Y-%m-%dT%H:%M:%S%z"))
}

write_analysis_conditions_json <- function(conditions, path) {
  jsonlite::write_json(conditions, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(path)
}

write_analysis_conditions_csv <- function(conditions, path) {
  flat <- lapply(conditions, function(v) {
    if (is.list(v) || length(v) > 1L) as.character(jsonlite::toJSON(v, auto_unbox = TRUE))
    else if (is.null(v)) NA_character_ else v
  })
  write_csv_utf8(as.data.frame(flat, stringsAsFactors = FALSE, check.names = FALSE), path)
  invisible(path)
}

write_csv_utf8 <- function(df, path, bom = TRUE) {
  if (isTRUE(bom)) {
    # writeChar()はテキスト接続では警告が出るため、BOMバイトはバイナリ接続で先に書く
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

## ==== 窓生成・窓分割・解析オーケストレーション ==========================
# section 5: 窓ごとに生波形へ戻ってピーク検出をやり直さない。全解析範囲で
# 一度検出したRR系列を窓へ分割する。区間規則は[start,end)、最終窓のみ終端を
# 含める。重複窓（スライディング窓）を許容する。

read_window_csv <- function(path) {
  dt <- tryCatch(data.table::fread(path, data.table = FALSE, showProgress = FALSE), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0L) stop("複数区間CSVを解釈できません。")
  names(dt) <- trimws(names(dt))
  if (!all(c("window_start_sec", "window_end_sec") %in% names(dt))) {
    stop("複数区間CSVには window_start_sec, window_end_sec 列が必要です。")
  }
  if (!("window_id" %in% names(dt))) dt$window_id <- sprintf("W%03d", seq_len(nrow(dt)))
  dt$window_start_sec <- as.numeric(dt$window_start_sec)
  dt$window_end_sec <- as.numeric(dt$window_end_sec)
  if (any(!is.finite(dt$window_start_sec)) || any(!is.finite(dt$window_end_sec)) ||
      any(dt$window_end_sec <= dt$window_start_sec)) stop("複数区間CSVの開始・終了範囲が不正です。")
  dt[, c("window_id", "window_start_sec", "window_end_sec")]
}

build_windows <- function(analysis_start_sec, analysis_end_sec, window_mode = c("single", "fixed", "multi_csv"),
                           window_length_sec = NULL, step_sec = NULL, multi_csv_df = NULL,
                           record_start_sec = NULL, record_end_sec = NULL) {
  window_mode <- match.arg(window_mode)
  if (!is.finite(analysis_start_sec) || !is.finite(analysis_end_sec) || analysis_end_sec <= analysis_start_sec) {
    stop("開始・終了範囲が不正です。")
  }

  if (window_mode == "single") {
    windows <- data.frame(window_id = "W001", window_start_sec = analysis_start_sec, window_end_sec = analysis_end_sec,
                           window_length_sec = analysis_end_sec - analysis_start_sec, step_sec = NA_real_,
                           include_end = TRUE, stringsAsFactors = FALSE)
  } else if (window_mode == "fixed") {
    if (is.null(window_length_sec) || !is.finite(window_length_sec) || window_length_sec <= 0) stop("窓長が不正です。")
    if (is.null(step_sec) || !is.finite(step_sec) || step_sec <= 0) step_sec <- window_length_sec
    # 窓数を整数演算で先に求める（seq()の浮動小数点誤差でstep*n==durationのとき
    # 極小の余分な窓が生まれるのを防ぐ）
    n_windows <- floor((analysis_end_sec - analysis_start_sec) / step_sec + 1e-9) + 1L
    starts <- analysis_start_sec + (seq_len(n_windows) - 1L) * step_sec
    starts <- starts[starts < analysis_end_sec - 1e-9]
    if (length(starts) == 0L) starts <- analysis_start_sec
    ends <- pmin(starts + window_length_sec, analysis_end_sec)
    windows <- data.frame(window_id = sprintf("W%03d", seq_along(starts)), window_start_sec = starts,
                           window_end_sec = ends, window_length_sec = ends - starts, step_sec = step_sec,
                           include_end = ends >= (analysis_end_sec - 1e-9), stringsAsFactors = FALSE)
  } else {
    if (is.null(multi_csv_df) || nrow(multi_csv_df) == 0L) stop("複数区間CSVが指定されていません。")
    if (!all(c("window_start_sec", "window_end_sec") %in% names(multi_csv_df))) {
      stop("複数区間CSVには window_start_sec, window_end_sec 列が必要です。")
    }
    wid <- if ("window_id" %in% names(multi_csv_df)) as.character(multi_csv_df$window_id) else sprintf("W%03d", seq_len(nrow(multi_csv_df)))
    ws <- as.numeric(multi_csv_df$window_start_sec)
    we <- as.numeric(multi_csv_df$window_end_sec)
    windows <- data.frame(window_id = wid, window_start_sec = ws, window_end_sec = we, window_length_sec = we - ws,
                           step_sec = NA_real_, include_end = we >= (max(we, na.rm = TRUE) - 1e-9), stringsAsFactors = FALSE)
  }

  windows$out_of_range <- if (!is.null(record_start_sec) && !is.null(record_end_sec) &&
                               is.finite(record_start_sec) && is.finite(record_end_sec)) {
    windows$window_start_sec < record_start_sec | windows$window_end_sec > record_end_sec
  } else FALSE
  windows
}

slice_rr_by_window <- function(rr_df, window_start_sec, window_end_sec, include_end = FALSE) {
  t_sec <- rr_df$ms / 1000
  idx <- if (isTRUE(include_end)) t_sec >= window_start_sec & t_sec <= window_end_sec else t_sec >= window_start_sec & t_sec < window_end_sec
  rr_df[idx, , drop = FALSE]
}

slice_resampled_by_window <- function(resampled, window_start_sec, window_end_sec, include_end = FALSE) {
  idx <- if (isTRUE(include_end)) resampled$time_sec >= window_start_sec & resampled$time_sec <= window_end_sec
         else resampled$time_sec >= window_start_sec & resampled$time_sec < window_end_sec
  list(time_sec = resampled$time_sec[idx], value = resampled$value[idx])
}

# 全解析範囲でピーク検出・RR生成・RR補正・等間隔化を一度だけ行い、時間窓ごとに
# PSDとHRV指標を算出する統合関数（section 5）。窓ごとの失敗はwarning_codeに
# 反映し、他の窓やアプリ全体には影響させない（section 9）。
run_hrv_analysis <- function(time_sec, ppg, fs_hz, analysis_start_sec, analysis_end_sec,
                              window_mode = c("single", "fixed", "multi_csv"),
                              window_length_sec = NULL, step_sec = NULL, multi_csv_df = NULL, freq_bands,
                              psd_config = list(window_sec = NULL, overlap_ratio = 0.5, nfft = NULL),
                              peak_params = list(min_peak_distance_sec = NULL, min_peak_height = NULL),
                              sd_multiplier = 3, resample_hz = 4, config, file_name = "", analysis_id = "") {
  window_mode <- match.arg(window_mode)
  record_start_sec <- min(time_sec, na.rm = TRUE)
  record_end_sec <- max(time_sec, na.rm = TRUE)
  if (!is.finite(analysis_start_sec) || !is.finite(analysis_end_sec) || analysis_end_sec <= analysis_start_sec ||
      analysis_start_sec < record_start_sec - 1e-6 || analysis_end_sec > record_end_sec + 1e-6) {
    stop("開始・終了範囲が不正です。")
  }

  idx <- which(time_sec >= analysis_start_sec & time_sec <= analysis_end_sec)
  if (length(idx) < 3L) stop("指定範囲のデータ点数が不足しています。")
  ppg_sub <- ppg[idx]
  time_sub <- time_sec[idx]

  pk <- detect_peaks(ppg_sub, fs_hz, time_sec = time_sub,
                      min_peak_distance_sec = peak_params$min_peak_distance_sec, min_peak_height = peak_params$min_peak_height)
  peaks <- pk$peaks
  if (nrow(peaks) < 2L) stop("ピーク数が不足しているためRRを生成できません。")

  rr_pipeline <- run_rr_pipeline(peaks, sd_multiplier = sd_multiplier, resample_hz = resample_hz)
  rr <- rr_pipeline$rr
  resampled <- rr_pipeline$resampled

  windows <- build_windows(analysis_start_sec, analysis_end_sec, window_mode = window_mode,
                            window_length_sec = window_length_sec, step_sec = step_sec, multi_csv_df = multi_csv_df,
                            record_start_sec = record_start_sec, record_end_sec = record_end_sec)

  result_rows <- vector("list", nrow(windows))
  psd_by_window <- list()
  peak_time_sec <- peaks$ms / 1000

  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    n_raw_in_window <- sum(time_sub >= w$window_start_sec & (if (w$include_end) time_sub <= w$window_end_sec else time_sub < w$window_end_sec))
    n_peaks_in_window <- sum(peak_time_sec >= w$window_start_sec & (if (w$include_end) peak_time_sec <= w$window_end_sec else peak_time_sec < w$window_end_sec))

    row_common <- list(file_name = file_name, analysis_id = analysis_id, analysis_mode = "recommended", window_id = w$window_id,
                        window_start_sec = w$window_start_sec, window_end_sec = w$window_end_sec,
                        window_length_sec = w$window_length_sec, step_sec = w$step_sec, sampling_rate_raw_hz = fs_hz,
                        resampling_rate_rr_hz = resampled$resample_hz, n_raw_samples = n_raw_in_window,
                        n_peaks = n_peaks_in_window, lf_lower_hz = freq_bands$lf_lower_hz, lf_upper_hz = freq_bands$lf_upper_hz,
                        hf_lower_hz = freq_bands$hf_lower_hz, hf_upper_hz = freq_bands$hf_upper_hz)

    res <- tryCatch({
      rr_win <- slice_rr_by_window(rr, w$window_start_sec, w$window_end_sec, include_end = w$include_end)
      resampled_win <- slice_resampled_by_window(resampled, w$window_start_sec, w$window_end_sec, include_end = w$include_end)
      td <- time_domain_metrics(rr_win$RR_ms_corrected)
      artifact_count <- sum(rr_win$is_outlier, na.rm = TRUE)
      artifact_percent <- if (nrow(rr_win) > 0L) 100 * artifact_count / nrow(rr_win) else NA_real_
      psd_res <- analyze_window_psd(resampled_win$value, resampled$resample_hz, freq_bands, psd_config = psd_config, rr_unit = "sec")
      qc <- evaluate_window_qc(w$window_length_sec, td$n_rr_used, psd_res$n_psd_points_lf, psd_res$n_psd_points_hf,
                                artifact_percent, psd_res$LF_power, psd_res$HF_power, isTRUE(w$out_of_range), config)
      psd_by_window[[w$window_id]] <- list(freq = psd_res$freq, psd = psd_res$psd, psd_unit = psd_res$psd_unit)

      na_if_invalid <- function(v) if (qc$is_valid) v else NA_real_
      list(n_rr_raw = nrow(rr_win), n_rr_used = td$n_rr_used, mean_rr_ms = td$mean_rr_ms, mean_hr_bpm = td$mean_hr_bpm,
           sdnn_ms = td$sdnn_ms, rmssd_ms = td$rmssd_ms, LF_power = na_if_invalid(psd_res$LF_power),
           HF_power = na_if_invalid(psd_res$HF_power), LF_HF = na_if_invalid(psd_res$LF_HF),
           HF_norm = na_if_invalid(psd_res$HF_norm), LF_norm = na_if_invalid(psd_res$LF_norm),
           LF_percent = na_if_invalid(psd_res$LF_percent), HF_percent = na_if_invalid(psd_res$HF_percent),
           psd_method = psd_res$psd_method, artifact_count = artifact_count, artifact_percent = artifact_percent,
           warning_code = qc$warning_code, warning_message = qc$warning_message)
    }, error = function(e) {
      list(n_rr_raw = NA_integer_, n_rr_used = NA_integer_, mean_rr_ms = NA_real_, mean_hr_bpm = NA_real_,
           sdnn_ms = NA_real_, rmssd_ms = NA_real_, LF_power = NA_real_, HF_power = NA_real_, LF_HF = NA_real_,
           HF_norm = NA_real_, LF_norm = NA_real_, LF_percent = NA_real_, HF_percent = NA_real_,
           psd_method = NA_character_, artifact_count = NA_integer_, artifact_percent = NA_real_,
           warning_code = "E_WINDOW_FAILED", warning_message = paste0("この窓の解析に失敗しました: ", conditionMessage(e)))
    })

    result_rows[[i]] <- do.call(build_result_row, c(row_common, res))
  }

  list(peaks = peaks, rr = rr, resampled = resampled, windows = windows, results_df = combine_result_rows(result_rows),
       psd_by_window = psd_by_window)
}

## ==== 表示専用ユーティリティ（解析結果には影響しない） ==================

decimate_for_display <- function(x, y, max_points) {
  n <- length(x)
  if (n <= max_points) return(list(x = x, y = y))
  idx <- unique(round(seq(1, n, length.out = max_points)))
  list(x = x[idx], y = y[idx])
}

## ==== UI =================================================================

ui <- fluidPage(
  titlePanel("耳朶容積脈波(PPG) HRV解析"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. データ入力"),
      fileInput("raw_file", "ローデータファイル (CSV/TSV)", accept = c(".csv", ".tsv", ".txt")),
      verbatimTextOutput("fs_info"),
      checkboxInput("fs_manual_toggle", "サンプリング周波数を手動で上書きする", value = FALSE),
      conditionalPanel("input.fs_manual_toggle == true",
        numericInput("fs_manual", "サンプリング周波数 (Hz)", value = 200, min = 1, max = 2000)),

      hr(), h4("2. 解析パラメータ"),
      numericInput("min_peak_distance_sec", "最小ピーク距離 (秒)", value = 0.35, min = 0.1, max = 2, step = 0.05),
      numericInput("resample_hz", "再サンプリング周波数 (Hz)", value = 4, min = 1, max = 20, step = 1),

      hr(), h4("3. 解析対象範囲"),
      fluidRow(column(6, numericInput("analysis_start", "開始秒", value = 0)),
               column(6, numericInput("analysis_end", "終了秒", value = 100))),
      actionButton("full_range_btn", "全範囲を使用"),

      hr(), h4("4. 窓設定"),
      radioButtons("window_mode", NULL, choices = c("単一区間" = "single", "固定窓長+ステップ" = "fixed", "複数区間CSV" = "multi_csv"), selected = "single"),
      conditionalPanel("input.window_mode == 'fixed'",
        numericInput("window_length_sec", "窓長 (秒)", value = 300, min = 1),
        numericInput("step_sec", "ステップ幅 (秒、既定=窓長)", value = 300, min = 1)),
      conditionalPanel("input.window_mode == 'multi_csv'",
        fileInput("window_csv", "複数区間CSV (window_start_sec, window_end_sec[, window_id])", accept = ".csv")),

      hr(), h4("5. 周波数帯設定"),
      fluidRow(column(6, numericInput("lf_lower", "LF下限(Hz)", value = 0.04, step = 0.01)),
               column(6, numericInput("lf_upper", "LF上限(Hz)", value = 0.15, step = 0.01))),
      fluidRow(column(6, numericInput("hf_lower", "HF下限(Hz)", value = 0.15, step = 0.01)),
               column(6, numericInput("hf_upper", "HF上限(Hz)", value = 0.40, step = 0.01))),

      hr(), actionButton("run_analysis", "解析実行", class = "btn-primary"),

      hr(), h4("6. ダウンロード"),
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
        tabPanel("PSD", uiOutput("psd_window_select_ui"), plotOutput("psd_plot", height = "320px")),
        tabPanel("結果表", DT::dataTableOutput("results_table")),
        tabPanel("警告・QC要約", verbatimTextOutput("qc_summary"))
      )
    )
  )
)

## ==== Server ==============================================================

server <- function(input, output, session) {
  imported <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)

  observeEvent(input$raw_file, {
    res <- tryCatch(import_shimmer_file(input$raw_file$datapath), error = function(e) {
      showNotification(paste("ファイルを読み込めません:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
    if (is.null(res)) return()
    imported(res)
    updateNumericInput(session, "analysis_start", value = 0)
    updateNumericInput(session, "analysis_end", value = max(res$time_sec, na.rm = TRUE))
    if (length(res$warnings) > 0L) showNotification(paste(res$warnings, collapse = "\n"), type = "warning", duration = 10)
  })

  output$fs_info <- renderText({
    req(imported())
    d <- imported()
    sprintf("検出列: 時刻=%s, PPG=%s\n推定サンプリング周波数: %.3f Hz (dt中央値=%.5f 秒)\n使用中のfs: %.3f Hz (%s)\nサンプル数: %d / 記録長: %.1f 秒",
            d$columns$time_col, d$columns$ppg_col,
            d$fs_estimated_hz %||% NA_real_, d$dt_median_sec %||% NA_real_, d$fs_hz, d$fs_source, d$n_samples, max(d$time_sec, na.rm = TRUE))
  })

  observeEvent(input$full_range_btn, {
    req(imported())
    updateNumericInput(session, "analysis_start", value = 0)
    updateNumericInput(session, "analysis_end", value = max(imported()$time_sec, na.rm = TRUE))
  })

  fs_used <- reactive({
    req(imported())
    if (isTRUE(input$fs_manual_toggle) && is.finite(input$fs_manual) && input$fs_manual > 0) input$fs_manual else imported()$fs_hz
  })

  observeEvent(input$run_analysis, {
    d <- imported()
    if (is.null(d)) { showNotification("先にローデータファイルをアップロードしてください。", type = "error"); return() }

    freq_bands <- list(lf_lower_hz = input$lf_lower, lf_upper_hz = input$lf_upper, hf_lower_hz = input$hf_lower, hf_upper_hz = input$hf_upper)
    if (!(freq_bands$lf_lower_hz < freq_bands$lf_upper_hz) || !(freq_bands$hf_lower_hz < freq_bands$hf_upper_hz)) {
      showNotification("周波数帯の下限・上限が不正です。", type = "error"); return()
    }

    multi_csv_df <- NULL
    if (input$window_mode == "multi_csv") {
      if (is.null(input$window_csv)) { showNotification("複数区間CSVをアップロードしてください。", type = "error"); return() }
      multi_csv_df <- tryCatch(read_window_csv(input$window_csv$datapath), error = function(e) {
        showNotification(paste("複数区間CSVを解釈できません:", conditionMessage(e)), type = "error"); NULL
      })
      if (is.null(multi_csv_df)) return()
    }

    res <- tryCatch(
      run_hrv_analysis(
        time_sec = d$time_sec, ppg = d$ppg, fs_hz = fs_used(),
        analysis_start_sec = input$analysis_start, analysis_end_sec = input$analysis_end,
        window_mode = input$window_mode,
        window_length_sec = input$window_length_sec, step_sec = input$step_sec,
        multi_csv_df = multi_csv_df, freq_bands = freq_bands,
        psd_config = list(window_sec = APP_CONFIG$psd$window_sec, overlap_ratio = APP_CONFIG$psd$overlap_ratio, nfft = APP_CONFIG$psd$nfft),
        peak_params = list(min_peak_distance_sec = input$min_peak_distance_sec, min_peak_height = NULL),
        sd_multiplier = APP_CONFIG$outlier$sd_multiplier,
        resample_hz = input$resample_hz %||% APP_CONFIG$resampling$default_hz,
        config = APP_CONFIG, file_name = input$raw_file$name %||% "", analysis_id = format(Sys.time(), "%Y%m%d%H%M%S")
      ),
      error = function(e) { showNotification(paste("解析に失敗しました:", conditionMessage(e)), type = "error", duration = NULL); NULL }
    )
    if (!is.null(res)) { analysis_result(res); showNotification("解析が完了しました。", type = "message") }
  })

  output$waveform_plot <- renderPlot({
    req(imported())
    d <- imported()
    disp <- decimate_for_display(d$time_sec, d$ppg, APP_CONFIG$display$waveform_decimation_max_points)
    plot(disp$x, disp$y, type = "l", col = "steelblue", xlab = "時間 (秒)", ylab = "PPG振幅",
         main = "PPG波形とピーク（表示は間引き、解析には全データを使用）")
    r <- analysis_result()
    if (!is.null(r) && nrow(r$peaks) > 0L) points(r$peaks$ms / 1000, r$peaks$Amplitude, col = "red", pch = 19, cex = 0.6)
  })

  output$tachogram_plot <- renderPlot({
    r <- analysis_result(); req(r)
    plot(r$rr$ms / 1000, r$rr$RR_ms_corrected, type = "o", pch = 16, cex = 0.4, col = "darkgreen",
         xlab = "時間 (秒)", ylab = "RR間隔 (ms、補正後)", main = "RR tachogram")
  })

  output$rr_correction_plot <- renderPlot({
    r <- analysis_result(); req(r)
    plot(r$rr$ms / 1000, r$rr$RR_ms, type = "l", col = "grey60", xlab = "時間 (秒)", ylab = "RR間隔 (ms)", main = "補正前後RR比較")
    lines(r$rr$ms / 1000, r$rr$RR_ms_corrected, col = "red")
    legend("topright", legend = c("補正前", "補正後"), col = c("grey60", "red"), lty = 1)
  })

  output$psd_window_select_ui <- renderUI({
    r <- analysis_result(); req(r)
    selectInput("psd_window_id", "表示する窓", choices = names(r$psd_by_window))
  })

  output$psd_plot <- renderPlot({
    r <- analysis_result(); req(r, input$psd_window_id)
    p <- r$psd_by_window[[input$psd_window_id]]; req(p)
    plot(p$freq, p$psd, type = "l", col = "purple", xlab = "周波数 (Hz)", ylab = sprintf("PSD (%s)", p$psd_unit),
         main = sprintf("PSD - %s", input$psd_window_id))
    abline(v = c(input$lf_lower, input$lf_upper, input$hf_lower, input$hf_upper), lty = 2, col = "grey50")
  })

  output$results_table <- DT::renderDataTable({
    r <- analysis_result(); req(r)
    DT::datatable(r$results_df, options = list(scrollX = TRUE, pageLength = 10))
  })

  output$qc_summary <- renderText({
    r <- analysis_result(); req(r)
    tab <- table(r$results_df$warning_code)
    paste(c("warning_code 集計:", paste(sprintf("  %s: %d 窓", names(tab), as.integer(tab)), collapse = "\n")), collapse = "\n")
  })

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
      r <- analysis_result(); d <- imported()
      validate(need(!is.null(r) && !is.null(d), "先に解析を実行してください。"))
      cond <- build_analysis_conditions(
        app_version = APP_VERSION,
        input_columns = d$columns, sampling_rate_hz = fs_used(),
        peak_condition = list(min_peak_distance_sec = input$min_peak_distance_sec),
        outlier_condition = list(sd_multiplier = APP_CONFIG$outlier$sd_multiplier),
        resampling_condition = list(resample_hz = r$resampled$resample_hz),
        psd_condition = APP_CONFIG$psd,
        freq_bands = list(lf_lower_hz = input$lf_lower, lf_upper_hz = input$lf_upper, hf_lower_hz = input$hf_lower, hf_upper_hz = input$hf_upper)
      )
      write_analysis_conditions_json(cond, file)
    }
  )
}

shinyApp(ui, server)
