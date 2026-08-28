# setup.R（testthatが自動実行）
#
# 本アプリは単一ファイル(app.R)構成のため、テストもapp.Rを直接source()して
# 解析関数群（ui/serverの定義も含む。shinyApp()はオブジェクトを作るだけで
# 実行はされないため、source()しても安全）を読み込む。
# 実行例: (hrv-shiny/ で) Rscript -e 'testthat::test_dir("tests/testthat")'

find_project_root <- function(start = getwd(), markers = c("app.R", "config"), max_up = 6) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (all(file.exists(file.path(d, markers)))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("プロジェクトルート（app.R, config/ を含むディレクトリ）が見つかりません。")
}

project_root <- find_project_root()
# app.R内部はgetwd()基準で設定ファイルを読み込むため、プロジェクトルートに揃える。
setwd(project_root)
source(file.path(project_root, "app.R"), encoding = "UTF-8")

TEST_CONFIG <- load_app_config(file.path(project_root, "config", "defaults.yml"))
TEST_BANDS <- list(
  lf_lower_hz = TEST_CONFIG$frequency_bands$lf_lower_hz,
  lf_upper_hz = TEST_CONFIG$frequency_bands$lf_upper_hz,
  hf_lower_hz = TEST_CONFIG$frequency_bands$hf_lower_hz,
  hf_upper_hz = TEST_CONFIG$frequency_bands$hf_upper_hz
)
