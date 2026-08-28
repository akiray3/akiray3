# test-import.R
#
# 引き継ぎ書 section 11 の必須テスト 1-3 に対応。
#
# 注意：本セッションの環境には実データ
# （S5_260722_Session1_Shimmer__Calibrated_PC.csv）が存在しなかったため、
# 引き継ぎ書 section 2 のフォーマット仕様に厳密に従って生成した合成データ
# （tests/testthat/helper-synthetic.R）で検証する。実データが入手できた
# 時点で、このテストのfixtureを実データに差し替えて再実行すること。

test_that("Shimmer形式（sep=\\t行・単位行つき）を正しく読み込める", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_synthetic_shimmer_file(path, duration_sec = 5, fs_hz = 200, seed = 1)

  res <- import_shimmer_file(path)

  expect_equal(res$columns$time_col, "Shimmer__TimestampSync_Unix_CAL")
  expect_equal(res$columns$ppg_col, "Shimmer__PPG_A13_CAL")
  expect_true(res$format$header)
  expect_true(res$format$has_unit_row)
  expect_equal(res$format$delim, "\t")
  expect_equal(res$format$skip_lines, 1L)
  expect_equal(res$n_samples, 5 * 200 + 1)
  expect_equal(res$time_sec[1], 0)
})

test_that("sep=\\t行と単位行がデータに混入しない（先頭が有効な数値データである）", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_synthetic_shimmer_file(path, duration_sec = 2, fs_hz = 200, seed = 2)

  res <- import_shimmer_file(path)
  expect_true(is.numeric(res$ppg))
  expect_false(any(is.na(res$ppg)))
  expect_true(all(is.finite(res$time_sec)))
})

test_that("タイムスタンプ差の中央値からサンプリング周波数を再現可能に推定できる", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_synthetic_shimmer_file(path, duration_sec = 30, fs_hz = 256, seed = 3)

  res <- import_shimmer_file(path)
  expect_equal(res$fs_estimated_hz, 256, tolerance = 1e-6)
  expect_equal(res$fs_source, "estimated")

  # 再実行しても同じ推定値になる（再現性）
  res2 <- import_shimmer_file(path)
  expect_equal(res$fs_estimated_hz, res2$fs_estimated_hz)
})

test_that("fsの手動上書きが機能し、既定200Hzを機械的に適用しない", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_synthetic_shimmer_file(path, duration_sec = 5, fs_hz = 128, seed = 4)

  res_auto <- import_shimmer_file(path)
  expect_equal(res_auto$fs_hz, 128, tolerance = 1e-6)

  res_manual <- import_shimmer_file(path, fs_override = 500)
  expect_equal(res_manual$fs_hz, 500)
  expect_equal(res_manual$fs_source, "manual")
  # 推定値自体は上書きされない（画面表示用に保持）
  expect_equal(res_manual$fs_estimated_hz, 128, tolerance = 1e-6)
})

test_that("カンマ区切り・セミコロン区切りを自動判定できる", {
  path_comma <- tempfile(fileext = ".csv")
  on.exit(unlink(path_comma))
  writeLines(c("Time,PPG", "0,100", "5,110", "10,105"), path_comma)
  res_comma <- import_shimmer_file(path_comma)
  expect_equal(res_comma$format$delim, ",")

  path_semi <- tempfile(fileext = ".csv")
  on.exit(unlink(path_semi))
  writeLines(c("Time;PPG", "0;100", "5;110", "10;105"), path_semi)
  res_semi <- import_shimmer_file(path_semi)
  expect_equal(res_semi$format$delim, ";")
})

test_that("ヘッダなし2列形式を補助対応できる", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("1000.0,100.0", "1005.0,110.0", "1010.0,105.0"), path)

  res <- import_shimmer_file(path)
  expect_false(res$format$header)
  expect_equal(res$columns$all, c("V1", "V2"))
  expect_equal(res$fs_estimated_hz, 200, tolerance = 1e-6)
})

test_that("既知のShimmer列名が自動選択される", {
  expect_equal(guess_time_column(c("Shimmer__TimestampSync_Unix_CAL", "Shimmer__PPG_A13_CAL")),
               "Shimmer__TimestampSync_Unix_CAL")
  expect_equal(guess_ppg_column(c("Shimmer__TimestampSync_Unix_CAL", "Shimmer__PPG_A13_CAL")),
               "Shimmer__PPG_A13_CAL")
})

test_that("非単調・重複・大欠落・非有限タイムスタンプを検出できる", {
  # 非単調 + 重複を含む秒系列
  time_sec <- c(0, 0.005, 0.005, 0.015, 0.010, 5.0, 5.005)
  diag <- diagnose_timestamps(time_sec)

  expect_false(diag$is_monotonic)
  expect_gte(diag$n_duplicated, 1L)
  expect_gte(diag$n_non_monotonic, 1L)
  expect_gte(diag$n_large_gaps, 1L)

  diag_nonfinite <- diagnose_timestamps(c(0, 0.005, NA, Inf, 0.02))
  expect_equal(diag_nonfinite$n_non_finite, 2L)
})

test_that("解釈できないファイルはエラーメッセージを返す", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(character(0), path)
  expect_error(import_shimmer_file(path))
})

test_that("指定列が数値でない場合はエラーになる", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("Time,PPG", "a,b", "c,d"), path)
  expect_error(import_shimmer_file(path), "数値")
})
