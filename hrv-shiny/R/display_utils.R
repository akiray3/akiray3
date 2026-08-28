# display_utils.R
#
# 表示専用のユーティリティ。解析結果には一切影響しない（間引きは表示のみ）。

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
