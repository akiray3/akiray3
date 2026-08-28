# helper-synthetic.R
#
# テスト用の合成Shimmer形式PPGデータを生成するヘルパー。
# 実データ（S5_260722_Session1_Shimmer__Calibrated_PC.csv）は本セッションの
# 環境に存在しなかったため、引き継ぎ書 section 2 のフォーマット仕様
# （sep=\t行、ヘッダ行、単位行、タブ区切り、Unix ms タイムスタンプ）に
# 厳密に従って合成データを生成し、パーサ・解析パイプラインの検証に用いる。

#' 合成PPG波形を生成する（既知のHR変動を含む脈波形状）
#'
#' @param duration_sec 記録長(秒)
#' @param fs_hz サンプリング周波数(Hz)
#' @param base_hr_bpm 平均心拍数
#' @param lf_amp_bpm LF帯（0.1Hz付近）のHR変動振幅(bpm)
#' @param hf_amp_bpm HF帯（0.25Hz付近）のHR変動振幅(bpm)
#' @param noise_sd 観測ノイズのSD
#' @param seed 乱数シード
#' @return list(time_sec, ppg)
generate_synthetic_ppg <- function(duration_sec = 300, fs_hz = 200,
                                    base_hr_bpm = 72, lf_amp_bpm = 4, hf_amp_bpm = 3,
                                    noise_sd = 0.02, seed = 1) {
  set.seed(seed)
  t <- seq(0, duration_sec, by = 1 / fs_hz)
  hr_bpm <- base_hr_bpm + lf_amp_bpm * sin(2 * pi * 0.1 * t) + hf_amp_bpm * sin(2 * pi * 0.25 * t)
  inst_freq_hz <- hr_bpm / 60
  phase <- 2 * pi * cumsum(inst_freq_hz) / fs_hz

  # 脈波らしい非対称波形（基本波+高調波）を単純な合成で近似
  ppg <- sin(phase) + 0.35 * sin(2 * phase - pi / 3) + 0.1 * sin(3 * phase)
  ppg <- ppg + rnorm(length(t), sd = noise_sd)

  list(time_sec = t, ppg = ppg)
}

#' 合成PPG波形をShimmer形式ファイル（sep=\t行、ヘッダ、単位行、タブ区切り）として書き出す
#'
#' @param path 出力パス
#' @param t0_ms 先頭サンプルのUnixタイムスタンプ(ms)
#' @param ... generate_synthetic_ppg() へ渡す引数
write_synthetic_shimmer_file <- function(path, t0_ms = 1784681699011.98, ...) {
  sim <- generate_synthetic_ppg(...)
  timestamp_ms <- t0_ms + sim$time_sec * 1000

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines("sep=\t", con)
  writeLines("Shimmer__TimestampSync_Unix_CAL\tShimmer__PPG_A13_CAL", con)
  writeLines("ms\tmV", con)
  body <- paste(formatC(timestamp_ms, format = "f", digits = 8),
                formatC(sim$ppg * 1000 + 1500, format = "f", digits = 6),
                sep = "\t")
  writeLines(body, con)
  invisible(path)
}
