# import_shimmer.R
#
# Shimmer由来のPPGローデータ（タブ区切り、拡張子は.csvでも実体はTSV）の
# 読込・時刻正規化・サンプリング周波数推定を行う。
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
