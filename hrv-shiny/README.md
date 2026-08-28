# 耳朶容積脈波(PPG) HRV解析 Shinyアプリ

Shimmerで記録した耳朶容積脈波（PPG）のローデータをアップロードし、指定した解析範囲・時間窓ごとに
心拍変動（HRV）の周波数指標（LF power, HF power, LF/HF, HF_norm, LF_norm）と補助的な時間領域指標・
QC情報を算出するR Shinyアプリです。

本アプリは改良版（recommended）の解析パイプラインのみを実装しています。
PulseWaveTools原本アルゴリズムを忠実再現する「legacyモード」は削除済みです。
入力ファイルの形式はShimmer出力（sep=行・ヘッダ・単位行つきTSV/CSV）に統一されているため、
時刻列・PPG列はアップロード時に自動判定し、ユーザーが列を選び直すUIは持ちません
（検出結果はサイドバーの推定サンプリング周波数欄に表示されます）。

- **サンプルデータ** (`sample_data/synthetic_shimmer_sample.csv`) は実データではなく、
  Shimmer形式仕様に厳密に従って生成した合成データです
  （`tests/testthat/helper-synthetic.R` で生成）。

## セットアップ

```bash
cd hrv-shiny
Rscript -e 'renv::restore(lockfile = "renv-lock/renv.lock")'   # renv.lockに記録された依存関係を復元
```

**注意：このディレクトリで `renv::init()` や `renv::activate()` は実行しないでください。**
`renv::restore(lockfile = ...)` は明示的にlockfileパスを渡す限り、renvプロジェクトを
初期化しなくても動作します。`renv::init()` を実行すると `renv/activate.R` が作られ、
`.Rprofile` に `source("renv/activate.R")` が追記されますが、この状態でデプロイすると
下記「shinyapps.io等へのデプロイ」で説明する`subscript out of bounds`エラーが再発します。
誤って実行してしまった場合は、`renv/` ディレクトリを削除し、`.Rprofile` 先頭に追加された
`source("renv/activate.R")` の行を削除してください。

Docker等でクリーンな環境から動かす場合は、Ubuntu系であれば以下でも依存パッケージを揃えられます。

```bash
apt-get install -y r-base-core r-cran-shiny r-cran-dt r-cran-pracma r-cran-zoo \
  r-cran-data.table r-cran-yaml r-cran-jsonlite r-cran-testthat r-cran-renv
```

## 起動方法

```bash
cd hrv-shiny
Rscript -e 'shiny::runApp(".", port=3838, host="0.0.0.0")'
```

ブラウザで `http://localhost:3838` を開き、`sample_data/synthetic_shimmer_sample.csv`
（または実データ）をアップロードして動作を確認できます。

## shinyapps.io等へのデプロイ

`rsconnect::deployApp()` はアプリのディレクトリ直下（プロジェクトルート）に
`renv.lock` または `renv/activate.R` があると、自動的に「renvモード」でデプロイし、
デプロイ元マシンの実際のRライブラリと `renv.lock` の内容が完全一致していることを
要求します。ズレがあると `Error in FUN(X[[i]], ...) : subscript out of bounds` の
ような分かりにくいエラーで失敗します（"library and lockfile are out of sync"）。

この自動判定は `.rscignore` でファイルをバンドル対象から除外しても止まりません
（`.rscignore` はアップロードするファイルの絞り込みであり、renvプロジェクトか
どうかの判定はそれとは別にファイルの「存在」を見ているため）。そのため本アプリでは：

- `renv.lock` をプロジェクトルートに置かず **`renv-lock/renv.lock`** に配置しています。
- **このディレクトリで `renv::init()` を実行しないでください**（`renv/activate.R` が
  作られると同じ問題が再発します。上記「セットアップ」の注意も参照）。

これによりrsconnectはrenvプロジェクトと判定せず、`app.R` を直接スキャンして
ローカルにインストール済みのパッケージから依存関係を自動検出します。デプロイ前に、
最低限以下のパッケージをデプロイ元のRにインストールしておいてください。

```r
install.packages(c("shiny", "DT", "data.table", "pracma", "zoo", "yaml", "jsonlite"))
```

お使いのrsconnectが `dependencyResolution` 引数に対応している場合（比較的新しい
バージョン）は、`rsconnect::deployApp(dependencyResolution = "library")` と明示的に
指定する方法でも同じ回避が可能です。`renv-lock/renv.lock` はローカル開発時の
再現性確保（`renv::restore(lockfile = "renv-lock/renv.lock")`）のために残しています。

`rsconnect/` ディレクトリ（デプロイ先のアカウント・アプリID等）は `.gitignore` 済みで、
リポジトリには含まれません。各自の環境にのみ存在するローカルな状態です。

## テスト実行

```bash
cd hrv-shiny
Rscript -e 'testthat::test_dir("tests/testthat")'
```

実装時点の実行結果：**`FAIL 0 | WARN 0 | SKIP 0 | PASS 129`**（5ファイル、合計129アサーション）。

| テストファイル | 内容 |
|---|---|
| `test-import.R` | Shimmer形式読込、sep行/単位行除外、区切り文字自動判定、fs推定、タイムスタンプ診断 |
| `test-peaks.R` | ピーク検出（最小ピーク距離設定） |
| `test-rr.R` | RR生成、外れ値補正（一括マスク+単回補間）、等間隔化、時間領域指標 |
| `test-psd.R` | Welch PSD、帯域積分、LF/HF/LF_HF/HF_norm/LF_norm、境界0.15Hzの非重複、0除算対策 |
| `test-windowing.R` | 単一/固定/複数区間窓、重複窓、`[start,end)`規則、表示間引きの非干渉、CSV往復一致、窓単位の障害耐性 |

## ディレクトリ構成

単一ファイル(`app.R`)構成です。管理を容易にするため、機能ごとのRファイル分割はせず、
`app.R` 内をセクション見出し（`## ==== ... ====`）で区切って整理しています
（設定読込 → データ読込 → ピーク検出・RR処理 → 周波数解析 →
QC → 出力 → 窓生成/オーケストレーション → 表示ユーティリティ → UI → Server）。

```text
hrv-shiny/
├── app.R                        # 解析ロジック・UI・サーバーを1ファイルにまとめたもの
├── tests/testthat/              # app.Rをsource()して関数群を直接テストする
├── config/defaults.yml          # 閾値・既定値の集約設定
├── sample_data/                 # 合成デモデータ（実データではない）
├── renv-lock/renv.lock          # ルート直下に置くとrsconnectのrenvモードが誤検出されるため隔離
├── .rscignore                   # shinyapps.io等へのデプロイ時にtests/等を除外
└── README.md
```

## 出力ファイル

- **結果+QC CSV**：section 8.1 の列（`file_name`〜`warning_message`）を1窓1行で出力。ゼロ除算はNA（Infを出さない）。
- **ピーク/RR CSV**：peak time, sample index, amplitude, raw RR, artifact flag, corrected RR（再現性確認用）。
- **解析条件 JSON**：アプリバージョン、入力列、サンプリング周波数、各種条件、周波数帯、実行日時。

## 既知の制約 / 未実装（Phase 3）

- 複数ファイルの一括処理は未実装です。
- 配布用コンテナ化・shinylive化は未実装です。
- LF/HFやHF_norm等は「HRV周波数指標」「正規化成分」として出力しており、
  「交感神経活動」「副交感神経活動量」等の生理学的断定はUI・出力のいずれにも含めていません。
