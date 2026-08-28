# 耳朶容積脈波(PPG) HRV解析 Shinyアプリ

Shimmerで記録した耳朶容積脈波（PPG）のローデータをアップロードし、指定した解析範囲・時間窓ごとに
心拍変動（HRV）の周波数指標（LF power, HF power, LF/HF, HF_norm, LF_norm）と補助的な時間領域指標・
QC情報を算出するR Shinyアプリです。

このアプリは `Claude_Code_HRV_Shiny_______.md`（引き継ぎ書）の仕様に基づいて実装されています。

## 重要な注意（実装時の制約・限界）

このリポジトリを作成したセッションの環境には、以下がいずれも**存在しませんでした**：

1. 実データファイル `S5_260722_Session1_Shimmer__Calibrated_PC.csv`
2. `PulseWaveTools` パッケージの原本ソースコード

添付されていたのは引き継ぎ書（Markdown）のみでした。そのため：

- **`app.R` の「PulseWaveTools legacy」セクション**（`findPulsePeaks_legacy()` 等）は、
  引き継ぎ書 section 4 に明記されたアルゴリズム仕様（`findPulsePeaks()` の呼び出しコードは
  原文ママ、`findHRV()` / `resamplingEvent()` / `omitOutlier()` はアルゴリズムの文章記述）に
  基づいて再実装したものです。実際のPulseWaveToolsパッケージのソースとバイト単位で
  一致することは保証されません。
- **`tests/testthat/test-legacy-compatibility.R`** は、原PulseWaveToolsの出力とのgolden-data
  数値比較ではなく、「仕様どおりに実装されているか」「legacy/recommendedのコードパスが
  分離されているか」を検証するテストになっています。
- **サンプルデータ** (`sample_data/synthetic_shimmer_sample.csv`) は実データではなく、
  引き継ぎ書 section 2 のフォーマット仕様に厳密に従って生成した合成データです
  （`tests/testthat/helper-synthetic.R` で生成）。

**実データとPulseWaveToolsの原本ソースが入手できた時点で、必ず以下を行ってください：**

1. `app.R` の「PulseWaveTools legacy」セクションの各関数の出力を、原本の対応関数の出力と実データで比較する。
2. 差異があれば同セクションを修正する（recommendedモード側は変更しない）。
3. `tests/testthat/test-legacy-compatibility.R` に実データ・原本出力を使ったgolden-data回帰試験を追加する。
4. `sample_data/` を実データ（または実データから作った匿名化サンプル）に差し替える。

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

実装時点の実行結果：**`FAIL 0 | WARN 0 | SKIP 0 | PASS 153`**（6ファイル、合計153アサーション）。

| テストファイル | 内容 |
|---|---|
| `test-import.R` | Shimmer形式読込、sep行/単位行除外、区切り文字自動判定、fs推定、タイムスタンプ診断 |
| `test-peaks.R` | legacy/recommendedピーク検出（0.5×fs固定 vs 可変最小距離） |
| `test-rr.R` | RR生成、外れ値補正（逐次 vs 一括）、等間隔化（1Hz vs 可変Hz）、時間領域指標 |
| `test-psd.R` | Welch PSD、帯域積分、LF/HF/LF_HF/HF_norm/LF_norm、境界0.15Hzの非重複、0除算対策 |
| `test-windowing.R` | 単一/固定/複数区間窓、重複窓、`[start,end)`規則、表示間引きの非干渉、CSV往復一致、窓単位の障害耐性 |
| `test-legacy-compatibility.R` | legacy仕様適合性、原コード順序の再現、recommendedとのコードパス隔離（**要実データ再検証**） |

## legacyモードとrecommendedモードの差分

| 項目 | legacy | recommended |
|---|---|---|
| ピーク検出の最小距離 | `0.5 * samplingRate`（固定、原PulseWaveTools仕様） | 設定可能（既定0.35秒 = 約171bpmまで検出可） |
| 外れ値補正 | 逐次 `median±3SD` 補正＋都度spline補間（順序依存、原コード再現） | 一括マスク後に単回spline補間（順序非依存） |
| 等間隔化 | RRイベントを1ms格子に配置→`zoo::na.spline()`→1Hzで間引き抽出 | イベント時刻から直接spline補間（既定4Hz、設定可能） |
| アプリ標準パイプラインの処理順序 | RR生成 → 外れ値補正 → 等間隔化（`run_rr_pipeline()`、section 5準拠） | 同左（順序は共通、中身のアルゴリズムのみ異なる） |

`app.R` 内の `findHRV2_legacy()` のみ、原コードの処理順序
（RR生成 → 等間隔化 → 外れ値補正）を再現しており、互換性検証専用です。
Shinyアプリ本体は常に `run_rr_pipeline()`（RR生成→補正→等間隔化）を使用します。

## ディレクトリ構成

単一ファイル(`app.R`)構成です。管理を容易にするため、機能ごとのRファイル分割はせず、
`app.R` 内をセクション見出し（`## ==== ... ====`）で区切って整理しています
（設定読込 → データ読込 → PulseWaveTools legacy → ピーク検出 → RR処理 → 周波数解析 →
QC → 出力 → 窓生成/オーケストレーション → 表示ユーティリティ → UI → Server）。

```text
hrv-shiny/
├── app.R                        # 解析ロジック・UI・サーバーを1ファイルにまとめたもの
├── tests/testthat/              # app.Rをsource()して関数群を直接テストする
├── config/defaults.yml          # 閾値・既定値の集約設定
├── sample_data/                 # 合成デモデータ（実データではない、上記注意参照）
├── renv-lock/renv.lock          # ルート直下に置くとrsconnectのrenvモードが誤検出されるため隔離
├── .rscignore                   # shinyapps.io等へのデプロイ時にtests/等を除外
└── README.md
```

## 出力ファイル

- **結果+QC CSV**：section 8.1 の列（`file_name`〜`warning_message`）を1窓1行で出力。ゼロ除算はNA（Infを出さない）。
- **ピーク/RR CSV**：peak time, sample index, amplitude, raw RR, artifact flag, corrected RR（再現性確認用）。
- **解析条件 JSON**：アプリバージョン、PulseWaveTools系譜（本リポジトリでの実装経緯を明記）、
  入力列、サンプリング周波数、各種条件、周波数帯、実行日時。

## 既知の制約 / 未実装（Phase 3）

- 複数ファイルの一括処理は未実装です。
- 過去解析結果とのgolden-data比較は、実データ・原PulseWaveToolsソースが無いため未実装です
  （上記「重要な注意」参照）。
- 配布用コンテナ化・shinylive化は未実装です。
- LF/HFやHF_norm等は「HRV周波数指標」「正規化成分」として出力しており、
  「交感神経活動」「副交感神経活動量」等の生理学的断定はUI・出力のいずれにも含めていません。
