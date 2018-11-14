!*******************************************************************************
! 日・月の出・南中・入時刻、日・月の出・入方位角、日・月の南中高度の計算
!
!   date          name            version
!   2018.11.11    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数 : 99999999 [+-]999.99 [+-]999.99 [+]9999.99
!        第1: 日付 [必須]
!             計算対象の日付(グレゴリオ暦)を半角8桁数字で指定
!        第2: 緯度 [必須]
!             緯度を 度 で指定（度・分・秒は度に換算して指定すること）
!             (北緯はプラス、南緯はマイナス。桁数は特に制限なし)
!        第3: 経度 [必須]
!             経度を 度 で指定（度・分・秒は度に換算して指定すること）
!             (東経はプラス、西経はマイナス。桁数は特に制限なし)
!        第4: 標高 [必須]
!             標高をメートルで指定(マイナス値は指定不可)
!             (桁数は特に制限なし)
!*******************************************************************************
!
program sun_moon
  use const, only : SP, DP, SEC_DAY
  use time
  use delta_t
  use calc
  implicit none
  type(t_time) :: jst, utc
  type(t_time) :: tm_sr, tm_ss, tm_sm, tm_mr, tm_ms, tm_mm
  integer(SP)  :: utc_tai
  real(DP)     :: lat, lon, ht, dut1, dt, dt_d, dip, day_p
  real(DP)     :: ang_sr, ang_ss, ang_mr, ang_ms, ht_sm, ht_mm

  ! コマンドライン引数取得
  call get_arg(jst, lat, lon, ht)

  ! 初期処理
  call jst2utc(jst, utc)               ! JST -> UTC
  call utc2utc_tai(utc, utc_tai)       ! UTC -> UTC - TAI
  call utc2dut1(utc, dut1)             ! UTC -> DUT1
  call utc2dt(utc, utc_tai, dut1, dt)  ! UTC -> delta T
  dt_d = dt / SEC_DAY                  ! ΔTを日換算
  dip = 0.0353333_DP * sqrt(ht)        ! 地平線伏角
  day_p = day_progress(jst)            ! 2000年1月1日力学時正午からの経過日数(日)

  ! 各種計算
  call calc_sun_sr( 0, lon, lat, day_p, dt_d, dip, tm_sr, ang_sr)  ! 日の出
  call calc_sun_sr( 1, lon, lat, day_p, dt_d, dip, tm_ss, ang_ss)  ! 日の入
  call calc_sun_m(     lon, lat, day_p, dt_d, dip, tm_sm, ht_sm )  ! 日の南中
  call calc_moon_sr(0, lon, lat, day_p, dt_d, dip, tm_mr, ang_mr)  ! 月の出
  call calc_moon_sr(1, lon, lat, day_p, dt_d, dip, tm_ms, ang_ms)  ! 月の入
  call calc_moon_m(    lon, lat, day_p, dt_d, dip, tm_mm, ht_mm )  ! 月の南中

  ! 結果出力
  call display

  stop
contains
  ! コマンドライン引数取得
  !
  ! :param(out) type(t_time) jst: 日付
  ! :param(out) real(8)      lat: 緯度
  ! :param(out) real(8)      lon: 経度
  ! :param(out) real(8)       ht: 標高
  subroutine get_arg(jst, lat, lon ,ht)
    implicit none
    type(t_time), intent(out) :: jst
    real(DP),     intent(out) :: lat, lon ,ht
    character(32) :: buf
    integer(SP)   :: dt(8), ios, y, m, d

    if (iargc() /= 4) then
      print *, "Arguments: YYYYMMDD LATITUDE LONGITUDE HEIGHT"
      stop
    end if
    call getarg(1, buf)
    read (buf, '(I4I2I2)', iostat=ios) y, m, d
    if (ios /= 0) then
      print *, "DATE is invalid!"
      stop
    end if
    jst = t_time(y, m, d, 0, 0, 0)
    call getarg(2, buf)
    read (buf, *, iostat=ios) lat
    if (ios /= 0) then
      print *, "LATITUDE is invalid!"
      stop
    end if
    call getarg(3, buf)
    read (buf, *, iostat=ios) lon
    if (ios /= 0) then
      print *, "LONGITUDE is invalid!"
      stop
    end if
    call getarg(4, buf)
    read (buf, *, iostat=ios) ht
    if (ios /= 0) then
      print *, "HEIGHT is invalid!"
      stop
    end if
    if (ht < 0.0_DP) then
      print *, "HEIGHT is invalid!"
      stop
    end if
  end subroutine get_arg

  ! 結果出力
  subroutine display
    implicit none
    character(1) :: s_lat, s_lon

    s_lat = "N"
    s_lon = "E"
    if (lat < 0.0_DP) then
      s_lat = "S"
      lat = dsign(lat, 1.0_DP)
    end if
    if (lon < 0.0_DP) then
      s_lon = "W"
      lon = dsign(lon, 1.0_DP)
    end if
    print '("[", I4, "-", I0.2, "-", I0.2, "JST ", &
        & F0.4, A, X, F0.4, A, X, F0.2, "m]")', &
        & jst%year, jst%month, jst%day, lat, s_lat, lon, s_lon, ht
    print '("日の出 ", I0.2, ":", I0.2, ":", I0.2, &
        & " (方位角 ", F6.2, ")")', &
        & tm_sr%hour, tm_sr%minute, tm_sr%second, ang_sr
    print '("日南中 ", I0.2, ":", I0.2, ":", I0.2, &
        & " (　高度 ", F6.2, ")")', &
        & tm_sm%hour, tm_sm%minute, tm_sm%second, ht_sm
    print '("日の入 ", I0.2, ":", I0.2, ":", I0.2, &
        & " (方位角 ", F6.2, ")")', &
        & tm_ss%hour, tm_ss%minute, tm_ss%second, ang_ss
    if (ang_mr < 0.0_DP) then
      print '(A)', "月の出 --:--:-- (方位角 ---.--)"
    else
      print '("月の出 ", I0.2, ":", I0.2, ":", I0.2, &
          & " (方位角 ", F6.2, ")")', &
          & tm_mr%hour, tm_mr%minute, tm_mr%second, ang_mr
    end if
    if (ht_mm < 0.0_DP) then
      print '(A)', "月南中 --:--:-- (　高度 ---.--)"
    else
      print '("月南中 ", I0.2, ":", I0.2, ":", I0.2, &
          & " (　高度 ", F6.2, ")")', &
          & tm_mm%hour, tm_mm%minute, tm_mm%second, ht_mm
    end if
    if (ang_ms < 0.0_DP) then
      print '(A)', "月の入 --:--:-- (方位角 ---.--)"
    else
      print '("月の入 ", I0.2, ":", I0.2, ":", I0.2, &
          & " (方位角 ", F6.2, ")")', &
          & tm_ms%hour, tm_ms%minute, tm_ms%second, ang_ms
    end if
  end subroutine display
end program sun_moon

