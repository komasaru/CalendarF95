DB calendar データ生成手順
==========================

1. DB 生成
  1-1. Database (with `db_script/create_db.sql`)
  1-2. Table    (with `db_script/create_tbl.sql`)
2. マスタ生成
  2-1. サーバ or ダンプファイルからインポート
3. カレンダ CSV 生成（3-1 〜 3-8 は DB インポート用）
  3-1.  `jpl_csv_kokei`       (1899-2100)
  3-2.  `jpl_csv_sekki_24`    (1899-2100)
  3-3.  `jpl_csv_moon`        (1899-2100)
  3-4.  `jpl_csv_zassetsu`    (1900-2099; required `kokei.csv`, `sekki_24.csv`)
  3-5.  `jpl_csv_holiday`     (1900-2099; required `sekki_24.csv`)
  3-6.  `jpl_csv_etc`         (1900-2099; required `moon.csv`)
  3-7.  `jpl_csv_oc`          (1900-2099; required `sekki_24.csv`, `moon.csv`)
  3-8.  `jpl_csv_calendar`    (1900-2099; required all of the above csv files)
  3-9.  `jpl_csv_calendar_j`  (1900-2099; `jpl_csv_calendar` のコード部分を日本語化したもの)

4. DB インポート（CSV ロード）
  4-1.  `imp_kokei.sh`        (-> `dat_kokeis`)
  4-2.  `imp_sekki_24.sh`     (-> `dat_sekki24s`)
  4-3.  `imp_moon.sh`         (-> `dat_moons`)
  4-4.  `imp_zassetsu.sh`     (-> `dat_zassetsu`)
  4-5.  `imp_holiday.sh`      (-> `dat_holidays`)
  4-6.  `imp_etc.sh`          (-> `dat_etcs`)
  4-7.  `imp_oc.sh`           (-> `dat_old_calendars`)
  4-8.  `imp_calendar.sh`     (-> `dat_calendars`)

  5-12. `jpl_cal`

