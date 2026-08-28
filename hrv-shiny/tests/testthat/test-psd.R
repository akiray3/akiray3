# test-psd.R
#
# 引き継ぎ書 section 11 の必須テスト 5-8 に対応。

test_that("band_power()は境界0.15Hzを二重計上しない（LF排他・HF包含）", {
  freq <- c(0.10, 0.14, 0.15, 0.16, 0.30)
  psd <- c(1, 1, 1, 1, 1)

  lf <- band_power(freq, psd, 0.04, 0.15, include_upper = FALSE)
  hf <- band_power(freq, psd, 0.15, 0.40, include_upper = TRUE)

  # LFは0.15を含まない -> {0.10,0.14}のみ、HFは0.15を含む -> {0.15,0.16,0.30}
  expect_equal(lf, pracma::trapz(c(0.10, 0.14), c(1, 1)))
  expect_equal(hf, pracma::trapz(c(0.15, 0.16, 0.30), c(1, 1, 1)))
})

test_that("band_power()は点数不足でNAを返す（Infにしない）", {
  freq <- c(0.10)
  psd <- c(1)
  expect_true(is.na(band_power(freq, psd, 0.04, 0.15)))
})

test_that("既知周波数の合成RR系列で、LF帯とHF帯のパワーが期待方向になる", {
  fs <- 4
  t <- seq(0, 300, by = 1 / fs)
  # HF成分(0.28Hz)を支配的に、LF成分(0.08Hz)を弱く混合
  rr_ms <- 800 + 20 * sin(2 * pi * 0.28 * t) + 2 * sin(2 * pi * 0.08 * t)

  res <- analyze_window_psd(rr_ms, fs, TEST_BANDS, rr_unit = "sec")
  expect_true(res$HF_power > res$LF_power)

  # 逆にLF成分(0.08Hz)を支配的にした場合はLF_power > HF_power
  rr_ms2 <- 800 + 20 * sin(2 * pi * 0.08 * t) + 2 * sin(2 * pi * 0.28 * t)
  res2 <- analyze_window_psd(rr_ms2, fs, TEST_BANDS, rr_unit = "sec")
  expect_true(res2$LF_power > res2$HF_power)
})

test_that("LF/HF, HF_norm, LF_normの数式が正しい", {
  fs <- 4
  t <- seq(0, 300, by = 1 / fs)
  rr_ms <- 800 + 15 * sin(2 * pi * 0.25 * t) + 5 * sin(2 * pi * 0.08 * t)
  res <- analyze_window_psd(rr_ms, fs, TEST_BANDS, rr_unit = "sec")

  expect_equal(res$LF_HF, res$LF_power / res$HF_power)
  expect_equal(res$HF_norm, res$HF_power / (res$LF_power + res$HF_power))
  expect_equal(res$LF_norm, res$LF_power / (res$LF_power + res$HF_power))
  expect_equal(res$LF_percent, 100 * res$LF_norm)
  expect_equal(res$HF_percent, 100 * res$HF_norm)
})

test_that("HF_norm + LF_normは数値誤差範囲で1になる", {
  fs <- 4
  t <- seq(0, 300, by = 1 / fs)
  rr_ms <- 800 + 10 * sin(2 * pi * 0.2 * t) + rnorm(length(t), sd = 0.5)
  res <- analyze_window_psd(rr_ms, fs, TEST_BANDS, rr_unit = "sec")
  expect_equal(res$HF_norm + res$LF_norm, 1, tolerance = 1e-9)
})

test_that("PSD推定に失敗する場合（点数不足）はエラーを投げ、呼び出し側でwarning_codeへ変換できる", {
  expect_error(analyze_window_psd(c(1, 2), 4, TEST_BANDS))
})

test_that("compute_hrv_frequency_indices()はLFまたはHFが0のときNAを返しInfを出さない（0除算対策）", {
  freq <- seq(0, 1, by = 0.01)
  psd <- rep(0, length(freq))  # 全帯域パワー0
  idx <- compute_hrv_frequency_indices(freq, psd, 0.04, 0.15, 0.15, 0.40)

  expect_equal(idx$LF_power, 0)
  expect_equal(idx$HF_power, 0)
  expect_true(is.na(idx$LF_HF))
  expect_true(is.na(idx$HF_norm))
  expect_true(is.na(idx$LF_norm))
  expect_false(is.infinite(idx$LF_HF))
})

test_that("welch_psd()は短い系列でも（セグメント長を自動調整して）落ちない", {
  x <- rnorm(20)
  res <- welch_psd(x, fs = 4)
  expect_true(length(res$freq) > 0)
  expect_true(all(is.finite(res$psd)))
})

test_that("welch_psd()は点数不足で明示的にエラーを返す", {
  expect_error(welch_psd(c(1, 2), fs = 4))
})

test_that("PSD単位はrr_unitに応じて明示される", {
  fs <- 4
  t <- seq(0, 100, by = 1 / fs)
  rr_ms <- 800 + 5 * sin(2 * pi * 0.2 * t)

  res_sec <- analyze_window_psd(rr_ms, fs, TEST_BANDS, rr_unit = "sec")
  res_ms <- analyze_window_psd(rr_ms, fs, TEST_BANDS, rr_unit = "ms")

  expect_equal(res_sec$psd_unit, "s^2/Hz")
  expect_equal(res_ms$psd_unit, "ms^2/Hz")
  # ms版はsec版の(1000^2)倍のスケール
  expect_equal(res_ms$LF_power / res_sec$LF_power, 1e6, tolerance = 1e-6)
})
