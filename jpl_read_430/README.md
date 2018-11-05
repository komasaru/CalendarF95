ビルド方法
==========

`make`

（やり直す場合は、 `make clean` をしてから）

準備
====

* JPL 天文暦バイナリデータ `JPLEPH` を実行ファイルと同じディレクトリ内に配置。  
  （参照「[JPL 天文暦データのバイナリ化！](https://www.mk-mode.com/octopress/2016/04/18/merging-jpl-data/ "JPL 天文暦データのバイナリ化！")」）

実行方法
========

`./jpl_read_430 [A [JD]]`

* `A` は天体番号。(`1`〜`13`; 省略可）  
  （省略時は `11` とみなす)
   1. 水星            (Mercury)
   2. 金星            (Venus)
   3. 地球 - 月の重心 (Earth-Moon barycenter)
   4. 火星            (Mars)
   5. 木星            (Jupiter)
   6. 土星            (Saturn)
   7. 天王星          (Uranus)
   8. 海王星          (Neptune)
   9. 冥王星          (Pluto)
  10. 月（地心）      (Moon (geocentric))
  11. 太陽            (Sun)
  12. 地球の章動(1980年IAUモデル) (Earth Nutations in longitude and obliquity(IAU 1980 model))
  13. 月の秤動        (Lunar mantle libration)
* `JD` はユリウス日(Julian Day)。（省略可）  
  （省略時はシステム時刻を UTC とみなしたユリウス日）

