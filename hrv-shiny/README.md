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
Rscript -e 'renv::restore()'   # renv.lockに記録された依存関係を復元
```

Docker等でクリーンな環境から動かす場合は、Ubuntu系であれば以下でも依存パッケージを揃えられます。

```bash
apt-get install -y r-base-core r-cran-shiny r-cran-dt r-cran-pracma r-cran-zoo \
  r-cran-data.table r-cran-yaml r-cran-jsonlite r-cran-testthat r-cran-renv
```

## デプロイ時のトラブルシューティング

`rsconnect::deployApp()`（shinyapps.ioへのデプロイ）で以下のようなエラーが出る場合：

```
ℹ Capturing R dependencies from renv.lock
Error in FUN(X[[i]], ...) : subscript out of bounds
In addition: Warning messages:
1: In packageDescription(name, lib.loc = lib_dir, encoding = "UTF-8") :
  no package 'shiny' was found
...
```

これは`app.R`やコードの不具合ではなく、**`rsconnect::deployApp()`を実行しているRセッションの
ライブラリに、`renv.lock`記載のパッケージが実際にはインストールされていない**ことが原因です。
`renv.lock`はプロジェクトの依存関係の「記録」に過ぎず、それだけでは対象パッケージは
インストールされません。`renv`が有効化されないまま（＝プロジェクト専用ライブラリではなく
グローバルライブラリを見に行った状態で）`packageDescription()`が対象パッケージを見つけられず、
rsconnect内部の依存グラフ構築処理が`subscript out of bounds`で落ちます。

**対処手順**：

1. `hrv-shiny/`ディレクトリをカレントにしてRセッションを開始する（`.Rprofile`が読み込まれる状態）。
2. 同じセッションで依存関係を復元する。
   ```r
   renv::restore()
   ```
3. 復元が完了していることを確認する。
   ```r
   renv::status()
   ```
4. **同じRセッションのまま**（別セッションで開き直さない）`rsconnect::deployApp()`を実行する。
   セッションを開き直すとライブラリの有効化状態がリセットされる場合があるため。

なお本リポジトリには`.rscignore`を追加し、`tests/`（`testthat`に依存）をデプロイ用バンドルから
除外しています。テストの実行自体には引き続き`testthat`のインストールが必要です
（下記「テスト実行」参照）。

## 起動方法

```bash
cd hrv-shiny
Rscript -e 'shiny::runApp(".", port=3838, host="0.0.0.0")'
```

ブラウザで `http://localhost:3838` を開き、`sample_data/synthetic_shimmer_sample.csv`
（または実データ）をアップロードして動作を確認できます。

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
├── renv.lock
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
