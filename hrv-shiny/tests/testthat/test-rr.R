# test-rr.R
#
# RR生成・外れ値補正・等間隔化・時間領域指標の検証。

make_peaks_from_times <- function(times_sec, fs = 200, amplitude = 1) {
  data.frame(
    Amplitude = rep(amplitude, length(times_sec)),
    Point = round(times_sec * fs) + 1L,
    ms = times_sec * 1000
  )
}

test_that("generate_rr()はピーク時刻差からRRを算出し後側ピークに対応づける", {
  peaks <- make_peaks_from_times(c(0, 0.8, 1.6, 2.4))
  rr <- generate_rr(peaks)

  expect_equal(nrow(rr), 3L)
  expect_equal(rr$RR_ms, c(800, 800, 800))
  expect_equal(rr$ms, c(800, 1600, 2400))  # 後側ピークの時刻
})

test_that("correct_rr_outliers()のrecommendedモードは一括マスク後に単回補間する", {
  set.seed(11)
  rr <- rnorm(30, mean = 800, sd = 8)
  rr[15] <- 1600  # 明確な外れ値（正常RRのおよそ2倍）
  res <- correct_rr_outliers(rr, mode = "recommended", sd_multiplier = 3)

  expect_true(res$outlier_flag[15])
  expect_equal(sum(res$outlier_flag), 1L)
  expect_false(is.na(res$corrected[15]))
  expect_equal(res$artifact_percent, 100 * 1 / 30)
})

test_that("correct_rr_outliers()のlegacyモードは逐次median±3SD補正を行う（原アルゴリズム再現）", {
  set.seed(11)
  rr <- rnorm(30, mean = 800, sd = 8)
  rr[15] <- 1600
  res <- correct_rr_outliers(rr, mode = "legacy", sd_multiplier = 3)

  expect_true(res$outlier_flag[15])
  expect_false(is.na(res$corrected[15]))
})

test_that("外れ値がない場合は補正率0で系列が変化しない", {
  rr <- c(800, 810, 795, 805, 790, 800)
  res_leg <- correct_rr_outliers(rr, mode = "legacy")
  res_rec <- correct_rr_outliers(rr, mode = "recommended")

  expect_equal(res_leg$artifact_percent, 0)
  expect_equal(res_rec$artifact_percent, 0)
  expect_equal(res_leg$corrected, rr)
  expect_equal(res_rec$corrected, rr)
})

test_that("resample_rr()のlegacyモードは1Hzで等間隔系列を返す", {
  event_ms <- seq(0, 60000, by = 800)
  values <- rep(800, length(event_ms))
  res <- resample_rr(event_ms, values, mode = "legacy")

  expect_equal(res$resample_hz, 1)
  expect_true(all(diff(res$time_sec) - 1 < 1e-6))
  expect_true(all(abs(res$value - 800) < 1e-6))
})

test_that("resample_rr()のrecommendedモードは指定Hzで等間隔系列を返す", {
  event_ms <- seq(0, 60000, by = 800)
  values <- rep(800, length(event_ms))
  res <- resample_rr(event_ms, values, mode = "recommended", resample_hz = 4)

  expect_equal(res$resample_hz, 4)
  expect_true(all(abs(diff(res$time_sec) - 0.25) < 1e-6))
  expect_true(all(abs(res$value - 800) < 1e-6))
})

test_that("resample_rr()はイベント不足でエラーになる（spline補間不能）", {
  expect_error(resample_rr(c(0), c(800), mode = "recommended"))
})

test_that("time_domain_metrics()はSDNN/RMSSD/平均HRを正しく算出する", {
  rr <- c(800, 810, 790, 800)
  m <- time_domain_metrics(rr)

  expect_equal(m$mean_rr_ms, mean(rr))
  expect_equal(m$mean_hr_bpm, 60000 / mean(rr))
  expect_equal(m$sdnn_ms, sd(rr))
  expect_equal(m$rmssd_ms, sqrt(mean(diff(rr)^2)))
  expect_equal(m$n_rr_used, 4L)
})

test_that("time_domain_metrics()は空・NAのみでも落ちずNAを返す", {
  m <- time_domain_metrics(numeric(0))
  expect_true(is.na(m$mean_rr_ms))
  expect_equal(m$n_rr_used, 0L)

  m2 <- time_domain_metrics(c(NA_real_, NA_real_))
  expect_true(is.na(m2$mean_rr_ms))
})

test_that("run_rr_pipeline()はRR生成->補正->等間隔化の順で標準パイプラインを実行する", {
  set.seed(5)
  peaks <- make_peaks_from_times(cumsum(c(0, rep(0.8, 40))) + rnorm(41, sd = 0.005))
  res <- run_rr_pipeline(peaks, mode = "legacy")

  expect_true(is.data.frame(res$rr))
  expect_true("RR_ms_corrected" %in% names(res$rr))
  expect_true(is.list(res$resampled))
  expect_equal(res$resampled$resample_hz, 1)
})

test_that("RR間隔が2未満だとエラーになる（アプリが落ちない設計の前提となるエラー処理）", {
  peaks <- make_peaks_from_times(c(0))
  expect_error(run_rr_pipeline(peaks, mode = "legacy"), "RR間隔")
})
