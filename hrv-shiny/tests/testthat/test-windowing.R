# test-windowing.R
#
# 引き継ぎ書 section 11 の必須テスト 8-11 に対応。

test_that("単一区間窓は[start,end]全体を1窓として生成する", {
  w <- build_windows(10, 40, window_mode = "single")
  expect_equal(nrow(w), 1L)
  expect_equal(w$window_start_sec, 10)
  expect_equal(w$window_end_sec, 40)
  expect_true(w$include_end)
})

test_that("固定窓長+ステップは非重複窓の開始・終了時刻が正しい（[start,end)規則）", {
  w <- build_windows(0, 900, window_mode = "fixed", window_length_sec = 300, step_sec = 300)
  expect_equal(nrow(w), 3L)
  expect_equal(w$window_start_sec, c(0, 300, 600))
  expect_equal(w$window_end_sec, c(300, 600, 900))
  expect_equal(w$include_end, c(FALSE, FALSE, TRUE))  # 最終窓のみ終端を含める
})

test_that("スライディング窓（重複窓）を許容し、開始・終了時刻が正しい", {
  w <- build_windows(0, 100, window_mode = "fixed", window_length_sec = 30, step_sec = 10)
  expect_true(nrow(w) > 3L)
  # 隣接窓は重複する（step < window_length）
  expect_true(all(w$window_start_sec[-1] < w$window_end_sec[-nrow(w)]))
  expect_equal(w$window_start_sec, (seq_len(nrow(w)) - 1) * 10)
})

test_that("端数窓は記録範囲を超えず、最終窓のみ終端を含める", {
  w <- build_windows(0, 100, window_mode = "fixed", window_length_sec = 30, step_sec = 30)
  expect_equal(tail(w$window_end_sec, 1), 100)
  expect_true(all(w$window_end_sec <= 100))
  expect_equal(w$include_end, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("複数区間CSV（read_window_csv）から窓を生成できる", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c(
    "window_id,window_start_sec,window_end_sec",
    "A,0,60",
    "B,50,120"  # 意図的に重複させる
  ), path)

  df <- read_window_csv(path)
  w <- build_windows(0, 120, window_mode = "multi_csv", multi_csv_df = df)

  expect_equal(nrow(w), 2L)
  expect_equal(w$window_id, c("A", "B"))
  expect_equal(w$window_start_sec, c(0, 50))
  expect_equal(w$window_end_sec, c(60, 120))
})

test_that("窓が記録範囲を超える場合はout_of_rangeフラグが立つ", {
  w <- build_windows(0, 100, window_mode = "fixed", window_length_sec = 40, step_sec = 40,
                      record_start_sec = 0, record_end_sec = 90)
  expect_true(any(w$out_of_range))
})

test_that("開始・終了範囲が不正な場合はエラーになる", {
  expect_error(build_windows(50, 10, window_mode = "single"))
  expect_error(build_windows(NA, 10, window_mode = "single"))
})

test_that("slice_rr_by_window()は[start,end)規則で切り出し、最終窓のみ終端を含める", {
  rr_df <- data.frame(ms = c(1000, 2000, 3000, 4000, 5000), RR_ms = rep(1000, 5))

  # [1,4) 秒 -> 1000,2000,3000msが該当（4000msは含まない）
  sliced <- slice_rr_by_window(rr_df, 1, 4, include_end = FALSE)
  expect_equal(sliced$ms, c(1000, 2000, 3000))

  # 終端を含める場合は4000msも含む
  sliced_end <- slice_rr_by_window(rr_df, 1, 4, include_end = TRUE)
  expect_equal(sliced_end$ms, c(1000, 2000, 3000, 4000))
})

test_that("slice_resampled_by_window()も同じ規則で動作する", {
  resampled <- list(time_sec = c(0, 1, 2, 3, 4), value = c(10, 20, 30, 40, 50))
  sliced <- slice_resampled_by_window(resampled, 1, 4, include_end = FALSE)
  expect_equal(sliced$time_sec, c(1, 2, 3))

  sliced_end <- slice_resampled_by_window(resampled, 1, 4, include_end = TRUE)
  expect_equal(sliced_end$time_sec, c(1, 2, 3, 4))
})

test_that("表示用間引き（decimate_for_display）は解析結果に影響しない", {
  set.seed(1)
  fs <- 200
  t <- seq(0, 60, by = 1 / fs)
  hr_bpm <- 70 + 5 * sin(2 * pi * 0.1 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  # decimate_for_display はapp.Rの表示専用処理であり、解析（run_hrv_analysis）は
  # 常に間引いていない全データを使用する（本関数呼び出しの有無で結果は変わらない）
  disp <- decimate_for_display(t, ppg, max_points = 500)
  expect_true(length(disp$x) <= 500)

  res <- run_hrv_analysis(
    t, ppg, fs, 0, 60, mode = "legacy", window_mode = "single",
    freq_bands = TEST_BANDS, config = TEST_CONFIG, file_name = "x", analysis_id = "y"
  )
  # 間引き後データを解析対象として使っていないことを、生波形サンプル数で確認する
  expect_equal(res$results_df$n_raw_samples[1], length(t))
})

test_that("CSVダウンロード値（write_csv_utf8で書き出した結果）は画面表(results_df)と一致する", {
  set.seed(2)
  fs <- 200
  t <- seq(0, 120, by = 1 / fs)
  hr_bpm <- 70 + 5 * sin(2 * pi * 0.1 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  res <- run_hrv_analysis(
    t, ppg, fs, 0, 120, mode = "legacy", window_mode = "fixed",
    window_length_sec = 60, step_sec = 60,
    freq_bands = TEST_BANDS, config = TEST_CONFIG, file_name = "x", analysis_id = "y"
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_csv_utf8(res$results_df, path)
  reloaded <- utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

  expect_equal(nrow(reloaded), nrow(res$results_df))
  expect_equal(reloaded$LF_power, res$results_df$LF_power, tolerance = 1e-10)
  expect_equal(reloaded$HF_power, res$results_df$HF_power, tolerance = 1e-10)
  expect_equal(reloaded$warning_code, res$results_df$warning_code)
})

test_that("解析範囲全体でピークが検出できない場合は分かりやすいエラーメッセージになる（アプリはクラッシュせずエラーを返す）", {
  fs <- 200
  t <- seq(0, 10, by = 1 / fs)
  ppg <- rep(0, length(t))  # ピークが検出できない平坦な信号

  expect_error(
    run_hrv_analysis(t, ppg, fs, 0, 10, mode = "legacy", window_mode = "single",
                      freq_bands = TEST_BANDS, config = TEST_CONFIG),
    "ピーク数が不足"
  )
})

test_that("一部の窓だけRR不足で失敗しても、他の窓は正常に計算され全体は落ちない", {
  set.seed(3)
  fs <- 200
  t <- seq(0, 125, by = 1 / fs)  # 60,60,5秒 の3窓になる（最終窓はRR不足になりやすい）
  hr_bpm <- 75 + 3 * sin(2 * pi * 0.15 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  res <- run_hrv_analysis(
    t, ppg, fs, 0, 125, mode = "legacy", window_mode = "fixed",
    window_length_sec = 60, step_sec = 60,
    freq_bands = TEST_BANDS, config = TEST_CONFIG, file_name = "x", analysis_id = "y"
  )

  expect_equal(nrow(res$results_df), 3L)
  # 60秒窓は短窓警告つきだが値自体は算出される（アプリを止めない）
  expect_false(any(is.na(res$results_df$LF_power[1:2])))
  expect_true(grepl("E_INSUFFICIENT_RR|E_INSUFFICIENT_PSD_POINTS|E_WINDOW_FAILED",
                     res$results_df$warning_code[3]))
  expect_true(is.na(res$results_df$LF_power[3]))
})

test_that("極端に短い窓（30秒）でも結果行が生成され、短窓警告が付与される", {
  set.seed(4)
  fs <- 200
  t <- seq(0, 60, by = 1 / fs)
  hr_bpm <- 75 + 3 * sin(2 * pi * 0.15 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  res <- run_hrv_analysis(
    t, ppg, fs, 0, 60, mode = "legacy", window_mode = "fixed",
    window_length_sec = 30, step_sec = 30,
    freq_bands = TEST_BANDS, config = TEST_CONFIG, file_name = "x", analysis_id = "y"
  )

  expect_equal(nrow(res$results_df), 2L)
  expect_true(all(grepl("W_SHORT_WINDOW", res$results_df$warning_code)))
})
