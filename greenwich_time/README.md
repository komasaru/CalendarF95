ビルド方法
==========

`make`

（やり直す場合は、 `make clean` をしてから）

準備
====

* うるう年ファイル `LEAP_SEC.txt`, DUT1 ファイル `DUT1.txt` は適宜最新のものに更新すること。

実行方法
========

`./greenwich_time [YYYYMMDDHHMMSSUUUUUU]`

* TT（地球時）は「年・月・日・時・分・秒・マイクロ秒」を20桁で指定する。
* TT（地球時）を指定しない場合は、システム日時を TT とみなす。

