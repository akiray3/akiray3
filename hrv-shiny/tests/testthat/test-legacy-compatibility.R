# test-legacy-compatibility.R
#
# 引き継ぎ書 section 11 の必須テスト 4, 12 に対応。
#
# 重要な注意（正直な限界）：
# 本セッションの環境およびリポジトリには PulseWaveTools パッケージの
# 原本ソースが存在しなかった（添付されたのは引き継ぎ書のみ）。そのため、
# 「元のPulseWaveToolsパッケージの出力と数値が一致する」ことを直接検証する
# golden-data回帰試験はここでは実施できない。
#
# 代わりに、本ファイルでは以下を検証する：
#   (a) R/PulseWaveTools_legacy.R が引き継ぎ書 section 4 に明記された
#       アルゴリズム仕様（findPulsePeaksの呼び出しコードは原文ママ、
#       findHRV/resamplingEvent/omitOutlierのアルゴリズム記述）どおりに
#       動作すること（仕様適合性）
#   (b) findHRV2_legacy()（原コードの処理順序: RR生成→等間隔化→外れ値補正）が
#       section 5 のアプリ標準パイプライン（RR生成→外れ値補正→等間隔化）と
#       意図的に異なる順序で動くこと（順序の取り違えがないことの確認）
#   (c) recommendedモードの呼び出しがlegacy関数の実行結果・状態に副作用を
#       及ぼさないこと（legacyコードパスの隔離）
#
# 実際のPulseWaveToolsソースが入手できた時点で、(a)をgolden dataとの
# 数値比較に置き換えること。

test_that("[要再検証] findPulsePeaks_legacyはminpeakdistance=0.5*samplingRateを仕様どおり用いる", {
  fs <- 200
  t <- seq(0, 10, by = 1 / fs)
  ppg <- sin(2 * pi * 1 * t)  # 60bpm相当のクリーンな信号

  peaks <- findPulsePeaks_legacy(ppg, fs)
  expect_true(all(diff(peaks$Point) >= 0.5 * fs))
  expect_true(all(c("Amplitude", "Point", "ms") %in% names(peaks)))
})

test_that("[要再検証] findHRV_legacyはRRを後側ピークの時刻に対応づける", {
  peaks <- data.frame(Amplitude = c(1, 1, 1), Point = c(1L, 201L, 401L), ms = c(0, 1000, 2000))
  rr <- findHRV_legacy(peaks)
  expect_equal(rr$ms, c(1000, 2000))
  expect_equal(rr$RR_ms, c(1000, 1000))
})

test_that("[要再検証] omitOutlier_legacyは逐次median±3SDでNA化しna.splineで補完する", {
  set.seed(21)
  rr <- rnorm(30, mean = 800, sd = 8)
  rr[10] <- 1700
  res <- omitOutlier_legacy(rr, sd_multiplier = 3)

  expect_true(res$outlier_flag[10])
  expect_false(is.na(res$corrected[10]))
  expect_equal(res$artifact_percent, 100 * sum(res$outlier_flag) / length(rr))
})

test_that("[要再検証] resamplingEvent_legacyは1ms格子経由で1Hz系列を返す", {
  event_ms <- seq(0, 30000, by = 800)
  values <- rep(800, length(event_ms))
  res <- resamplingEvent_legacy(event_ms, values)

  expect_true(all(abs(diff(res$time_sec) - 1) < 1e-9))
  expect_true(all(abs(res$value - 800) < 1e-6))
})

test_that("findHRV2_legacy()は原コードの順序（RR生成->等間隔化->外れ値補正）で実行される", {
  set.seed(22)
  fs <- 200
  t <- seq(0, 60, by = 1 / fs)
  hr_bpm <- 75 + 3 * sin(2 * pi * 0.1 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  legacy_flow <- findHRV2_legacy(ppg, fs, time_ms = t * 1000)

  # findHRV2_legacyは「等間隔化された1Hz系列」に対してomitOutlierを適用する
  # （run_rr_pipeline()のRR系列に直接適用するのとは対象が異なる）
  expect_equal(length(legacy_flow$corrected$corrected), length(legacy_flow$resampled$value))

  # section 5 のアプリ標準パイプラインは、外れ値補正をRR系列（等間隔化前）に
  # 適用する点で異なる。同じピークから出発しても、両者は同じ関数ではない。
  standard_flow <- run_rr_pipeline(legacy_flow$peaks, mode = "legacy")
  expect_false(identical(length(standard_flow$rr$RR_ms_corrected), length(legacy_flow$corrected$corrected)))
})

test_that("recommendedモードの実行がlegacy関数の出力に副作用を及ぼさない（コードパスの隔離）", {
  set.seed(23)
  fs <- 200
  t <- seq(0, 60, by = 1 / fs)
  hr_bpm <- 75 + 3 * sin(2 * pi * 0.12 * t)
  phase <- cumsum(hr_bpm / 60 / fs) * 2 * pi
  ppg <- sin(phase) + 0.02 * rnorm(length(t))

  peaks_before <- detect_peaks(ppg, fs, time_sec = t, mode = "legacy")$peaks
  rr_before <- run_rr_pipeline(peaks_before, mode = "legacy")

  # recommendedモードを一通り実行する（legacy関数の内部状態に触れない設計を確認）
  peaks_rec <- detect_peaks(ppg, fs, time_sec = t, mode = "recommended", min_peak_distance_sec = 0.3)$peaks
  invisible(run_rr_pipeline(peaks_rec, mode = "recommended", resample_hz = 4))

  peaks_after <- detect_peaks(ppg, fs, time_sec = t, mode = "legacy")$peaks
  rr_after <- run_rr_pipeline(peaks_after, mode = "legacy")

  expect_identical(peaks_before, peaks_after)
  expect_identical(rr_before$rr$RR_ms_corrected, rr_after$rr$RR_ms_corrected)
})

test_that("legacyとrecommendedは同一入力から異なる（が共に妥当な）ピーク数を返しうる", {
  set.seed(24)
  fs <- 200
  t <- seq(0, 30, by = 1 / fs)
  hr_bpm <- 140  # 高心拍（legacyでは検出漏れが生じうる）
  phase <- cumsum(rep(hr_bpm / 60 / fs, length(t))) * 2 * pi
  ppg <- sin(phase) + 0.3 * sin(2 * phase)

  n_legacy <- nrow(detect_peaks(ppg, fs, time_sec = t, mode = "legacy")$peaks)
  n_recommended <- nrow(detect_peaks(ppg, fs, time_sec = t, mode = "recommended",
                                      min_peak_distance_sec = 0.3)$peaks)

  expected_true_n <- floor(30 * hr_bpm / 60)
  expect_true(n_recommended >= n_legacy)
  expect_true(abs(n_recommended - expected_true_n) <= 3)
})
