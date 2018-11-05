ビルド方法
==========

`make`

（やり直す場合は、 `make clean` をしてから）

準備
====

* JPL 天文暦バイナリデータ `JPLEPH` を実行ファイルと同じディレクトリ内に配置。  
  （参照「[JPL 天文暦データのバイナリ化！](https://www.mk-mode.com/octopress/2016/04/18/merging-jpl-data/ "JPL 天文暦データのバイナリ化！")」）
* うるう年ファイル `LEAP_SEC.txt` は適宜最新のものに更新すること。

実行方法
========

`./apparent_sun_moon_jpl [YYYYMMDDHHMMSSUUUUUU]`

* JST（日本標準時）は「年・月・日・時・分・秒・マイクロ秒」を20桁で指定する。
* JST（日本標準時）を指定しない場合は、システム日時を JST とみなす。

