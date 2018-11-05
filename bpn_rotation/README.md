ビルド方法
==========

`make`

（やり直す場合は、 `make clean` をしてから）

実行方法
========

`./bpn_rotation X Y Z [YYYYMMDDHHMMSSUUUUUU]`

* `X`, `Y`, `Z` は GCRS 座標(x, y, z) を指定する。（省略不可）
* TT（地球時）は「年・月・日・時・分・秒・マイクロ秒」を20桁で指定する。
* TT（地球時）を指定しない場合は、システム日時を TT とみなす。

