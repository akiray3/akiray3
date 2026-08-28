# app.R
#
# 耳朶容積脈波(PPG) HRV解析Shinyアプリ。
#
# 本アプリは単一ファイル(app.R)構成とし、データ入出力・解析ロジック・
# UI・サーバーをすべてこのファイルにまとめている（管理を容易にするため、
# 機能ごとのRファイルへの分割はしていない）。ファイル内はセクション見出し
# （## ---- ... ----）で区切っている。
#
# 引き継ぎ書 section 4 に関する重要な注意：
# 本アプリを作成したセッションの環境には PulseWaveTools パッケージの
# 原本ソースも実データファイルも存在しなかった（添付は引き継ぎ書のみ）。
# そのため「legacy」セクションの各関数は、引き継ぎ書 section 4 に明記された
# アルゴリズム仕様（findPulsePeaks の呼び出しコードは原文ママ、findHRV/
# resamplingEvent/omitOutlier は文書中のアルゴリズム記述）に基づく再実装で
# ある。実データ・原本ソースが入手できた時点で、この節を数値比較のうえ
# 修正すること（詳細はREADME.md参照）。

library(shiny)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

## ==== 設定読込 =========================================================

#' config/defaults.yml を読み込む
#' @param path 設定ファイルパス
#' @return list（yaml::read_yamlの結果）
load_app_config <- function(path = "config/defaults.yml") {
  yaml::read_yaml(path)
}

APP_CONFIG <- load_app_config(file.path(getwd(), "config", "defaults.yml"))
APP_VERSION <- APP_CONFIG$app$version %||% "0.1.0"

KNOWN_TIME_COLS <- c(
  "Shimmer__TimestampSync_Unix_CAL",
  "Shimmer_TimestampSync_Unix_CAL",
  "Timestamp_Unix_CAL"
)
KNOWN_PPG_COLS <- c(
  "Shimmer__PPG_A13_CAL",
  "Shimmer_PPG_A13_CAL",
  "PPG_A13_CAL"
)
KNOWN_TIME_COL_HINT <- paste(KNOWN_TIME_COLS, collapse = ", ")
KNOWN_PPG_COL_HINT <- paste(KNOWN_PPG_COLS, collapse = ", ")

## ==== データ読込（Shimmer CSV/TSV） ====================================
#
# 引き継ぎ書 section 2 の要件に対応：
#   1. data.table::fread() を第一選択とする
#   2. 先頭の `sep=\t` 行を認識して除外する
#   3. 列名行と単位行を識別し、単位行はデータから除外する
#   4. タブ・カンマ・セミコロンを自動判定する
#   5. ヘッダありを標準、ヘッダなし2列形式も補助対応する
#   6. タイムスタンプはt0を引いてから秒に変換する（桁落ち回避）
#   7. fsはタイムスタンプ差分の中央値から推定する
#   8. 推定値は手動上書き可能（既定200Hzを機械的に適用しない）
#   9. 非単調・重複・大欠落・非有限値を検出する

#' ファイル冒頭を解析し、区切り文字・スキップ行数・ヘッダ構造を判定する
#'
#' @param path ファイルパス
#' @param n_peek 先読みする行数
#' @return list(delim, skip_lines, header, has_unit_row, col_names, n_fields)
detect_shimmer_format <- function(path, n_peek = 10L) {
  if (!file.exists(path)) {
    stop("ファイルが見つかりません。")
  }
  raw_lines <- readLines(path, n = n_peek, warn = FALSE, encoding = "UTF-8")
  raw_lines <- raw_lines[!is.na(raw_lines) & nzchar(trimws(raw_lines))]
  if (length(raw_lines) == 0L) {
    stop("ファイルを解釈できません（空ファイル、または読み込めません）。")
  }

  skip_lines <- 0L
  sep_declared <- NA_character_
  if (grepl("^sep=", raw_lines[1], ignore.case = TRUE)) {
    sep_char <- sub("^sep=", "", raw_lines[1], ignore.case = TRUE)
    if (nchar(sep_char) >= 1L) {
      sep_declared <- substr(sep_char, 1, 1)
    }
    skip_lines <- 1L
    raw_lines <- raw_lines[-1]
  }

  if (length(raw_lines) == 0L) {
    stop("ファイルを解釈できません（ヘッダ行が見つかりません）。")
  }

  candidate_delims <- c("\t", ",", ";")
  if (!is.na(sep_declared) && sep_declared %in% candidate_delims) {
    candidate_delims <- unique(c(sep_declared, candidate_delims))
  }

  header_line <- raw_lines[1]
  count_fields <- function(line, delim) length(strsplit(line, delim, fixed = TRUE)[[1]])
  field_counts <- vapply(candidate_delims, function(d) count_fields(header_line, d), integer(1))
  delim <- candidate_delims[which.max(field_counts)]
  if (max(field_counts) < 2L) {
    stop("ファイルを解釈できません（タブ・カンマ・セミコロンいずれでも列を分割できません）。")
  }

  is_numeric_token <- function(x) !is.na(suppressWarnings(as.numeric(x))) & nchar(x) > 0

  header_fields <- trimws(strsplit(header_line, delim, fixed = TRUE)[[1]])

  if (all(is_numeric_token(header_fields))) {
    # ヘッダなし2列（以上）形式
    return(list(
      delim = delim,
      skip_lines = skip_lines,
      header = FALSE,
      has_unit_row = FALSE,
      col_names = paste0("V", seq_along(header_fields)),
      n_fields = length(header_fields)
    ))
  }

  has_unit_row <- FALSE
  if (length(raw_lines) >= 2L) {
    unit_fields <- trimws(strsplit(raw_lines[2], delim, fixed = TRUE)[[1]])
    unit_len_ok <- length(unit_fields) == length(header_fields)
    unit_nonnumeric <- unit_len_ok && !all(is_numeric_token(unit_fields))

    if (unit_nonnumeric && length(raw_lines) >= 3L) {
      data_fields <- trimws(strsplit(raw_lines[3], delim, fixed = TRUE)[[1]])
      data_is_numeric <- length(data_fields) == length(header_fields) &&
        all(is_numeric_token(data_fields))
      has_unit_row <- data_is_numeric
    } else if (unit_nonnumeric) {
      has_unit_row <- TRUE
    }
  }

  list(
    delim = delim,
    skip_lines = skip_lines,
    header = TRUE,
    has_unit_row = has_unit_row,
    col_names = header_fields,
    n_fields = length(header_fields)
  )
}

#' Shimmer CSV/TSVを読み込む（列名は正規化済み文字列で保持）
#'
#' @param path ファイルパス
#' @return list(data = data.table, format = list)
read_shimmer_csv <- function(path) {
  fmt <- detect_shimmer_format(path)

  skip_total <- fmt$skip_lines +
    (if (isTRUE(fmt$header)) 1L else 0L) +
    (if (isTRUE(fmt$has_unit_row)) 1L else 0L)

  dt <- tryCatch(
    data.table::fread(
      file = path,
      sep = fmt$delim,
      skip = skip_total,
      header = FALSE,
      data.table = TRUE,
      showProgress = FALSE,
      na.strings = c("", "NA", "NaN", "N/A")
    ),
    error = function(e) NULL
  )

  if (is.null(dt) || nrow(dt) == 0L) {
    stop("ファイルを解釈できません。区切り文字またはヘッダ構造を確認してください。")
  }

  n_use <- min(ncol(dt), length(fmt$col_names))
  dt <- dt[, seq_len(n_use), with = FALSE]
  data.table::setnames(dt, fmt$col_names[seq_len(n_use)])

  list(data = dt, format = fmt)
}

#' 既知のShimmer列名から時刻列を推定する
guess_time_column <- function(col_names) {
  hit <- col_names[col_names %in% KNOWN_TIME_COLS]
  if (length(hit) > 0L) return(hit[1])
  hit2 <- col_names[grepl("timestamp", col_names, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[1])
  col_names[1]
}

#' 既知のShimmer列名からPPG列を推定する
guess_ppg_column <- function(col_names) {
  hit <- col_names[col_names %in% KNOWN_PPG_COLS]
  if (length(hit) > 0L) return(hit[1])
  hit2 <- col_names[grepl("ppg", col_names, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[1])
  col_names[min(2L, length(col_names))]
}

#' タイムスタンプ(ms)を正規化して経過秒に変換する（桁落ち回避のためt0を先に減算）
#'
#' @param timestamp_ms 数値ベクトル（Unixタイムスタンプ、ms単位）
#' @return list(time_sec, t0_ms)
normalize_time_sec <- function(timestamp_ms) {
  timestamp_ms <- as.numeric(timestamp_ms)
  finite_idx <- which(is.finite(timestamp_ms))
  if (length(finite_idx) == 0L) {
    stop("タイムスタンプに有効な値がありません。")
  }
  t0_ms <- timestamp_ms[finite_idx[1]]
  time_sec <- (timestamp_ms - t0_ms) / 1000
  list(time_sec = time_sec, t0_ms = t0_ms)
}

#' タイムスタンプ差分の中央値からサンプリング周波数を推定する
#'
#' @param time_sec 経過秒ベクトル
#' @return list(fs_hz, dt_sec)
estimate_sampling_rate <- function(time_sec) {
  d <- diff(time_sec)
  d_pos <- d[is.finite(d) & d > 0]
  if (length(d_pos) == 0L) {
    return(list(fs_hz = NA_real_, dt_sec = NA_real_))
  }
  dt_sec <- stats::median(d_pos)
  list(fs_hz = 1 / dt_sec, dt_sec = dt_sec)
}

#' タイムスタンプの品質診断（非単調・重複・大欠落・非有限値）
#'
#' @param time_sec 経過秒ベクトル
#' @param large_gap_factor 中央値dtの何倍を「大きな欠落」とみなすか
#' @return list(...) 各種フラグとサンプルインデックス
diagnose_timestamps <- function(time_sec, large_gap_factor = 5) {
  n <- length(time_sec)
  non_finite_index <- which(!is.finite(time_sec))

  d <- diff(time_sec)
  dt_pos <- d[is.finite(d) & d > 0]
  dt_median_sec <- if (length(dt_pos) > 0L) stats::median(dt_pos) else NA_real_

  non_monotonic_index <- which(is.finite(d) & d <= 0) + 1L
  duplicated_index <- which(is.finite(d) & abs(d) < 1e-9) + 1L
  large_gap_index <- if (is.finite(dt_median_sec) && dt_median_sec > 0) {
    which(is.finite(d) & d > dt_median_sec * large_gap_factor) + 1L
  } else {
    integer(0)
  }

  list(
    n_samples = n,
    n_non_finite = length(non_finite_index),
    non_finite_index = non_finite_index,
    n_non_monotonic = length(non_monotonic_index),
    non_monotonic_index = non_monotonic_index,
    n_duplicated = length(duplicated_index),
    duplicated_index = duplicated_index,
    n_large_gaps = length(large_gap_index),
    large_gap_index = large_gap_index,
    dt_median_sec = dt_median_sec,
    is_monotonic = length(non_monotonic_index) == 0L
  )
}

#' Shimmerファイルの読込から時刻正規化・fs推定・診断までを一括実行する
#'
#' @param path ファイルパス
#' @param time_col 時刻列名。NULLなら自動推定。
#' @param ppg_col PPG列名。NULLなら自動推定。
#' @param fs_override 手動指定するサンプリング周波数(Hz)。NULLなら推定値を使用。
#' @return list(time_sec, ppg, fs_hz, fs_source, diagnostics, format, columns, warnings)
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

  if (all(is.na(timestamp_raw))) {
    stop(sprintf("指定列 '%s' が数値ではありません。", time_col))
  }
  if (all(is.na(ppg_raw))) {
    stop(sprintf("指定列 '%s' が数値ではありません。", ppg_col))
  }

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
  if (!is.finite(fs_used)) {
    warnings_jp <- c(warnings_jp, "サンプリング周波数を推定できません。手動で指定してください。")
  }
  if (!diag$is_monotonic) {
    warnings_jp <- c(warnings_jp, sprintf(
      "タイムスタンプが単調増加ではありません（%d 箇所）。", diag$n_non_monotonic
    ))
  }
  if (diag$n_duplicated > 0L) {
    warnings_jp <- c(warnings_jp, sprintf("タイムスタンプの重複が %d 箇所あります。", diag$n_duplicated))
  }
  if (diag$n_large_gaps > 0L) {
    warnings_jp <- c(warnings_jp, sprintf(
      "サンプリング間隔の中央値の%g倍を超える大きな欠落が %d 箇所あります。", 5, diag$n_large_gaps
    ))
  }
  if (diag$n_non_finite > 0L) {
    warnings_jp <- c(warnings_jp, sprintf("非有限のタイムスタンプが %d 箇所あります。", diag$n_non_finite))
  }

  list(
    time_sec = norm$time_sec,
    ppg = ppg_raw,
    t0_ms = norm$t0_ms,
    fs_hz = fs_used,
    fs_estimated_hz = fs_est$fs_hz,
    dt_median_sec = fs_est$dt_sec,
    fs_source = fs_source,
    diagnostics = diag,
    format = parsed$format,
    columns = list(all = col_names, time_col = time_col, ppg_col = ppg_col),
    warnings = warnings_jp,
    n_samples = nrow(dt)
  )
}

## ==== PulseWaveTools legacy（原アルゴリズム仕様の再実装） ==============
#
# 引き継ぎ書 section 4 に記載された既存 PulseWaveTools のアルゴリズムを
# 忠実に再現したラッパー群。
#
# 注意（重要）：本アプリを作成したセッションの環境には PulseWaveTools
# パッケージの原本ソースが存在しなかった（添付されたのは引き継ぎ書のみ）。
# そのため、本セクションは引き継ぎ書 section 4 に明記されたアルゴリズム仕様
# （findPulsePeaks の呼び出しコードは原文ママ、findHRV/resamplingEvent/
# omitOutlier/findHRV2 は文書中のアルゴリズム記述）を「legacy」として
# 実装したものである。実際の PulseWaveTools パッケージのソースが入手でき
# 次第、tests/testthat/test-legacy-compatibility.R で数値比較し、差異が
# あれば本セクションを修正すること。原コードの問題点（omitOutlier の順序
# 依存性など）はここでは無条件に修正せず、忠実再現を優先する。
#
# 改良版（recommended モード）の実装は本セクションに置かず、
# 「ピーク検出（統一）」「RR処理（統一）」の各セクションに分離する。

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
#' この順序は「RR処理（統一）」セクションで定めるアプリ標準パイプライン
#' （RR補正→等間隔化）とは異なる点に注意。findHRV2_legacy() はあくまで
#' legacy互換性検証（tests/testthat/test-legacy-compatibility.R）専用であり、
#' Shiny本体の解析には run_rr_pipeline() の標準パイプラインを用いること。
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

## ==== ピーク検出（legacy/recommended 統一インターフェース） ============
#
# legacy は上記 findPulsePeaks_legacy() をそのまま呼び出す
# （minpeakdistance = 0.5 * samplingRate は変更不可）。
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

## ==== RR処理（統一）：生成・外れ値補正・等間隔化・時間領域指標 ==========
#
# legacy/recommended共通の標準パイプライン
# （section 5: ピーク検出 → RR生成 → RR補正 → 等間隔化）として提供する。
#
# 重要：この標準パイプラインの処理順序（補正→等間隔化）は、上記
# findHRV2_legacy()（原コードの順序: 等間隔化→補正、互換性検証専用）とは
# 異なる。アプリ本体は常にこのセクションの run_rr_pipeline() を用いる。
# legacy/recommendedの違いは「各アルゴリズムの中身」（外れ値検出方式・
# 等間隔化方式）であり、「処理順序」ではない。

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

## ==== 周波数解析（Welch PSD・帯域積分・HRV周波数指標） ==================
#
# PulseWaveToolsには周波数解析部分が含まれていないため（引き継ぎ書 section 6.2）、
# 本セクションで明示的に実装する。
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

## ==== QC（品質管理判定） ================================================
#
# 閾値は config/defaults.yml に集約し、ソースコード中に散在させない
# （section 7）。窓ごとに warning_code / warning_message を返し、
# アプリ全体を停止させない（section 9）。

#' 窓1件分のQC判定を行う
#'
#' 判定するのはアプリが window ごとに算出した数値のみ。ここでは値を書き換えず、
#' warning_code / warning_message / is_valid（Eコードが1つでもあればFALSE）を返す。
#' 呼び出し側（結果CSV構築処理）は is_valid が FALSE の窓についてLF/HF等をNAにする。
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

## ==== 出力（結果CSV・ピーク/RR CSV・解析条件CSV/JSON） ==================
#
# 結果CSV・ピーク/RR CSV・解析条件CSV/JSONの出力ビルダー（section 8）。
# ここではデータ整形とファイル書き出しのみを行う。

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

## ==== 窓生成・窓分割・解析オーケストレーション ==========================
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

## ==== 表示専用ユーティリティ（解析結果には影響しない） ==================

#' 大容量波形を表示用に間引く（解析には影響しない）
#'
#' @param x,y 同じ長さの数値ベクトル
#' @param max_points 表示上限点数
#' @return list(x, y)（max_points以下、等間隔インデックスで抽出）
decimate_for_display <- function(x, y, max_points) {
  n <- length(x)
  if (n <= max_points) {
    return(list(x = x, y = y))
  }
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

## ==== Server ==============================================================

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
