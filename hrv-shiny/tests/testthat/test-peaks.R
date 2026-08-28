# test-peaks.R
#
# ピーク検出（legacy: PulseWaveTools再現 / recommended: 改良版）の検証。

make_clean_pulse_train <- function(fs = 200, duration_sec = 20, hr_bpm = 72, seed = 1) {
  set.seed(seed)
  t <- seq(0, duration_sec, by = 1 / fs)
  f_hz <- hr_bpm / 60
  ppg <- sin(2 * pi * f_hz * t) + 0.3 * sin(2 * 2 * pi * f_hz * t - pi / 4)
  list(time_sec = t, ppg = ppg, fs = fs, hr_bpm = hr_bpm)
}

test_that("legacyモードは既知のクリーンな脈波から妥当な数のピークを検出する", {
  sim <- make_clean_pulse_train(hr_bpm = 72, duration_sec = 20)
  res <- detect_peaks(sim$ppg, sim$fs, time_sec = sim$time_sec, mode = "legacy")

  expected_n <- floor(20 * 72 / 60)
  expect_true(abs(nrow(res$peaks) - expected_n) <= 2)
  expect_true(all(c("Amplitude", "Point", "ms") %in% names(res$peaks)))
  expect_true(all(diff(res$peaks$Point) > 0))  # Point昇順
  expect_equal(res$params$min_peak_distance_sec, 0.5)
})

test_that("legacyモードは0.5*fsのminpeakdistanceを変更しない（120bpm超は検出できない）", {
  # 150bpm（RR=400ms<500ms）の高心拍データ
  sim <- make_clean_pulse_train(hr_bpm = 150, duration_sec = 20)
  res <- detect_peaks(sim$ppg, sim$fs, time_sec = sim$time_sec, mode = "legacy")

  expected_n_true <- floor(20 * 150 / 60)  # 真のピーク数
  # minpeakdistance=0.5*fs（500ms）により、真のピーク数より明らかに少なく検出される
  expect_true(nrow(res$peaks) < expected_n_true * 0.7)
})

test_that("recommendedモードは最小ピーク距離を設定でき、120bpm超も検出できる", {
  sim <- make_clean_pulse_train(hr_bpm = 150, duration_sec = 20)
  res <- detect_peaks(sim$ppg, sim$fs, time_sec = sim$time_sec, mode = "recommended",
                       min_peak_distance_sec = 0.35)

  expected_n <- floor(20 * 150 / 60)
  expect_true(abs(nrow(res$peaks) - expected_n) <= 3)
  expect_equal(res$params$min_peak_distance_sec, 0.35)
})

test_that("recommendedモードの既定最小ピーク距離は0.35秒である", {
  sim <- make_clean_pulse_train(hr_bpm = 80, duration_sec = 10)
  res <- detect_peaks(sim$ppg, sim$fs, time_sec = sim$time_sec, mode = "recommended")
  expect_equal(res$params$min_peak_distance_sec, 0.35)
})

test_that("データ点数不足やfs不正はエラーになる", {
  expect_error(detect_peaks(c(1, 2), 200, mode = "legacy"))
  expect_error(detect_peaks(rnorm(100), NA, mode = "legacy"))
  expect_error(detect_peaks(rnorm(100), -1, mode = "recommended"))
})
