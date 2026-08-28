# setup.R（testthatが自動実行）
#
# 本プロジェクトはRパッケージではないため、テスト実行時の作業ディレクトリ
# （hrv-shiny/ プロジェクトルート）を起点にR/以下のソースを読み込む。
# 実行例: (hrv-shiny/ で) Rscript -e 'testthat::test_dir("tests/testthat")'

find_project_root <- function(start = getwd(), markers = c("R", "config"), max_up = 6) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (all(file.exists(file.path(d, markers)))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("プロジェクトルート（R/, config/ を含むディレクトリ）が見つかりません。")
}

project_root <- find_project_root()
r_files <- list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0L) {
  stop("R/ ソースが見つかりません。hrv-shiny/ をカレントディレクトリにしてテストを実行してください。")
}
for (f in r_files) {
  source(f, encoding = "UTF-8")
}

TEST_CONFIG <- load_app_config(file.path(project_root, "config", "defaults.yml"))
TEST_BANDS <- list(
  lf_lower_hz = TEST_CONFIG$frequency_bands$lf_lower_hz,
  lf_upper_hz = TEST_CONFIG$frequency_bands$lf_upper_hz,
  hf_lower_hz = TEST_CONFIG$frequency_bands$hf_lower_hz,
  hf_upper_hz = TEST_CONFIG$frequency_bands$hf_upper_hz
)
