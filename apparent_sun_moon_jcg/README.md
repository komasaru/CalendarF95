注意
====

* **2022年（令和4年）版から、当リポジトリのメンテナンスを行わないことにしました。**  
  **今後は、 [C++ 版](https://github.com/komasaru/ephemeris_jcg "komasaru/ephemeris_jcg: C++ source code to calculate ephemerises by JCG method.")のみをメンテナンスします。**

ビルド方法
==========

`make`

（やり直す場合は、 `make clean` をしてから）

準備
====

1. text ディレクトリ内に各年の係数ファイル（テキストファイル）を配置する。  
   （「[天文・暦情報](https://www1.kaiho.mlit.go.jp/KOHO/index.html "天文・暦情報")」より取得）
2. 扱いやすくするために、別途作成の Ruby スクリプトを実行して、整形する。（同一ディレクトリ内にテキストファイルが生成される）

実行方法
========

`./apparent_sun_moon_jcg [YYYYMMDDHHMMSS]`

* UTC（協定世界時）は「年・月・日・時・分・秒」を14桁で指定する。
* UTC（協定世界時）を指定しない場合は、システム日時を UTC とみなす。

