プログラム一覧
==============

1. `jpl_csv_kokei`  
   黄経一覧 CSV 出力  
   (1899-2100)
2. `jpl_csv_sekki_24`  
   二十四節気一覧 CSV 出力  
   (1899-2100)
3. `jpl_csv_moon`  
   月の朔弦望一覧 CSV 出力  
   (1899-2100)
4. `jpl_csv_zassetsu`  
   雑節一覧 CSV 出力  
   (1900-2099; `kokei.csv`, `sekki_24.csv` が必要)
5. `jpl_csv_holiday`  
   祝日一覧 CSV 出力  
   (1900-2099; `sekki_24.csv` が必要)
6. `jpl_csv_etc`  
   その他一覧 CSV 出力  
   (1900-2099; `moon.csv` が必要)
7. `jpl_csv_oc`  
   旧暦一覧 CSV 出力  
   (1900-2099; `sekki_24.csv`, `moon.csv` が必要)
8. `jpl_csv_calendar`  
   カレンダー一覧 CSV 出力  
   (1900-2099; 上記すべての CSV ファイルが必要)
9. `jpl_csv_calendar_j`  
   カレンダー一覧 CSV 出力 (`jpl_csv_calendar` のコード部分を日本語化したもの)  
   (1900-2099; 上記すべての CSV ファイルが必要)
10. `jpl_cal`  
   カレンダー（指定の日付のカレンダー情報を出力）  
   (1900-2099; 上記すべての CSV ファイルが必要)

* 当ディレクトリ直下のファイル名が `jpl_` で始まらない `.f95` ファイルは各種モジュールである。
* 上記 1〜7 を実行して作成される CSV ファイルは `csv` ディレクトリ配下に配置される。
* `csv` ディレクトリ以外のサブディレクトリについては、後述の「その他のサブディレクトリについて」を参照。

準備
====

* うるう年ファイル `LEAP_SEC.txt` を最新のものにしておく。  
  （参考「[うるう年 - NICT](http://jjy.nict.go.jp/QandA/data/leapsec.html "日本標準時プロジェクト　Information of Leap second")」）
* DUT1 ファイル `DUT1.txt` を最新のものにしておく。  
  （参考「[DUT1 - NICT](http://jjy.nict.go.jp/QandA/data/dut1.html "日本標準時プロジェクト　Announcement of DUT1")」）
* 当ディレクトリ直下の `HOLIDAY.txt` は祝日マスタである。変更があれば、都度更新する。  
  （左から「祝日ID, 月, 日, 区分, 有効年（開始）, 有効年（終了）, 名称」である）  
  + 祝日IDの `90`, `91` は固定。祝日を追加する場合、祝日IDには `90` 未満を設定する。
  + 月日固定の祝日はそのままその数字を月と日に設定する。
  + 日が変動する祝日は、日に `0` を設定する。
  + 区分には、月日固定の祝日なら `0`, 第2月曜日の祝日なら `2`, 第3月曜日の祝日なら `3`, 春／秋分なら `4`, 振替休日なら `8`, 国民の休日なら `9` を設定する。
  + 有効年（開始／終了）は、その祝日が有効となる開始と終了の西暦年を設定する。
  + 名称は日本語のみで10文字以内であること。半角文字は設定しないこと。（1文字 3byte を想定）

ビルド方法
==========

プログラム一覧の 10 個のプログラムは次のようにそれぞれのプログラムに対応した Makefile を指定してビルドする。（以下は `jpl_csv_kokei` をビルドする例）

``` text
$ make -f Makefile_csv_kokei
```

ビルドをやり直す場合は、その前に `clean` する。

``` text
$ make -f Makefile_csv_kokei clean
```

全てをまとめてビルドしたければ、別途 Makefile を作成してもよい。

実行方法
========

プログラム一覧の 8〜10 は、 1〜7 を順番に実行（CSV ファイルを作成）してから実行すること。

``` text
$ ./jpl_csv_kokei
$ ./jpl_csv_sekki_24
$ ./jpl_csv_moon
$ ./jpl_csv_zassetsu
$ ./jpl_csv_holiday
$ ./jpl_csv_etc
$ ./jpl_csv_oc
$ ./jpl_csv_calendar
$ ./jpl_csv_calendar_j
$ ./jpl_cal [YYYYMMDD]
```

既に該当の CSV ファイルが存在する場合は実行（上書き）できないようしている。  
（CSV ファイルを作成し直す場合は、該当の CSV ファイルを手動で削除すること）

その他のサブディレクトリについて
================================

`csv` 以外の以下のディレクトリはテスト＆確認用に作成したもので、それぞれ独立し、各ディレクトリ配下でビルド＆実行できるようになっている。（各ディレクトリ内の `README.md` 参照）

* `conv_time` （各種時刻換算）
* `conv_coord` （赤道・黄道座標変換（テスト））
* `greenwich_time` （グリニジ視恒星時 GAST 等の計算）
* `bpn_rotation` （バイアス・歳差・章動適用）
* `nutation_model` （章動の計算）
* `jpl_read_430` （JPLEPH(JPL の DE430 バイナリデータ)読み込み）
* `jpl_calc_430` （JPLEPH(JPL の DE430 バイナリデータ)読み込み、座標（位置・速度）を計算）
* `apparen_sun_moon_jpl` （太陽・月の視位置計算）
* `apparen_sun_moon_jcg` （太陽・月の視位置計算（海保・略算式版））

