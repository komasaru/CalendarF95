!*******************************************************************************
! Modules for calculation
!
!   date          name            version
!   2018.11.11    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module calc
  use const, only : SP, DP, JST_UTC, EPS, PI_180, A_REF
  use time,  only : t_time
  implicit none
  private
  public :: day_progress, &
          & calc_sun_sr, calc_sun_m, calc_moon_sr, calc_moon_m

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! 2000年1月1日力学時正午からの経過日数計算
  !
  ! :param(in) type(t_time) jst: JST
  ! :return    real(8)    day_p: 2000.0(2000年1月1日力学時正午)からの
  !                              経過日数 (日)
  function day_progress(jst) result(day_p)
    implicit none
    type(t_time), intent(in) :: jst
    real(DP) :: day_p
    integer(SP) :: y, m, d

    y = jst%year - 2000
    m = jst%month
    d = jst%day

    ! 1月,2月は前年の13月,14月とする
    if (m < 3) then
      y = y - 1
      m = m + 12
    end if
    day_p = 365.0_DP * y + 30.0_DP * m + d &
        & - 33.5_DP - JST_UTC / 24.0_DP &
        & + int(3 * (m + 1) / 5.0) &
        & + int(y / 4.0_DP)
  end function day_progress

  ! 計算（日の出／入）
  !
  ! :param(in)  integer(4)    kbn: 0: 日の出, : 日の入
  ! :param(in)  real(8)       lon: 経度
  ! :param(in)  real(8)       lat: 緯度
  ! :param(in)  real(8)     day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in)  real(8)      dt_d: ΔT(日換算)
  ! :param(in)  real(8)       dip: 地平線伏角
  ! :param(out) type(t_time) time: 日の出時刻
  ! :param(out) real(8)       ang: 日の出方位角
  subroutine calc_sun_sr(kbn, lon, lat, day_p, dt_d, dip, time, ang)
    implicit none
    integer(SP),  intent(in)  :: kbn
    real(DP),     intent(in)  :: lon, lat, day_p, dt_d, dip
    type(t_time), intent(out) :: time
    real(DP),     intent(out) :: ang
    real(DP) :: num, jy, kokei

    num = time_sun(kbn, lon, lat, day_p, dt_d, dip)
    call conv_time(num * 24.0_DP, time)
    jy = (day_p + num + dt_d) / 365.25_DP  ! 経過ユリウス年(日)
    kokei = lon_sun(jy)                    ! 黄経(太陽)
    ang = calc_angle(lon, lat, kokei, 0.0_DP, num, jy)
  end subroutine calc_sun_sr

  ! 計算（日の南中）
  !
  ! :param(in)  real(8)       lon: 経度
  ! :param(in)  real(8)       lat: 緯度
  ! :param(in)  real(8)     day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in)  real(8)      dt_d: ΔT(日換算)
  ! :param(in)  real(8)       dip: 地平線伏角
  ! :param(out) type(t_time) time: 日の南中時刻
  ! :param(out) real(8)        ht: 日の南中高度
  subroutine calc_sun_m(lon, lat, day_p, dt_d, dip, time, ht)
    implicit none
    real(DP),     intent(in)  :: lon, lat, day_p, dt_d, dip
    type(t_time), intent(out) :: time
    real(DP),     intent(out) :: ht
    real(DP) :: num, jy, kokei

    num = time_sun(2, lon, lat, day_p, dt_d, dip)
    call conv_time(num * 24.0_DP, time)
    jy = (day_p + num + dt_d) / 365.25_DP  ! 経過ユリウス年(日)
    kokei = lon_sun(jy)                    ! 黄経(太陽)
    ht = calc_height(lon, lat, kokei, 0.0_DP, num, jy)
  end subroutine calc_sun_m

  ! 計算（月の出／入）
  !
  ! :param(in)  integer(4)    kbn: 0: 月の出, 1: 月の入
  ! :param(in)  real(8)       lon: 経度
  ! :param(in)  real(8)       lat: 緯度
  ! :param(in)  real(8)     day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in)  real(8)      dt_d: ΔT(日換算)
  ! :param(in)  real(8)       dip: 地平線伏角
  ! :param(out) type(t_time) time: 月の出時刻
  ! :param(out) real(8)       ang: 月の出方位角
  subroutine calc_moon_sr(kbn, lon, lat, day_p, dt_d, dip, time, ang)
    implicit none
    integer(SP),  intent(in)  :: kbn
    real(DP),     intent(in)  :: lon, lat, day_p, dt_d, dip
    type(t_time), intent(out) :: time
    real(DP),     intent(out) :: ang
    real(DP) :: num, jy, kokei, koi

    num = time_moon(kbn, lon, lat, day_p, dt_d, dip)
    if (num < 0.0_DP) then
      ! 月の出／入がない場合はマイナスを設定
      time = t_time(0, 0, 0, -1, -1, -1)
      ang  = -1.0_DP
    else
      call conv_time(num * 24.0_DP, time)
      jy = (day_p + num + dt_d) / 365.25_DP  ! 経過ユリウス年(日)
      kokei = lon_moon(jy)  ! 黄経(月)
      koi   = lat_moon(jy)  ! 黄緯(月)
      ang = calc_angle(lon, lat, kokei, koi, num, jy)
    end if
  end subroutine calc_moon_sr

  ! 計算（月の南中）
  !
  ! :param(in)  real(8)       lon: 経度
  ! :param(in)  real(8)       lat: 緯度
  ! :param(in)  real(8)     day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in)  real(8)      dt_d: ΔT(日換算)
  ! :param(in)  real(8)       dip: 地平線伏角
  ! :param(out) type(t_time) time: 月の南中時刻
  ! :param(out) real(8)        ht: 月の南中高度
  subroutine calc_moon_m(lon, lat, day_p, dt_d, dip, time, ht)
    implicit none
    real(DP),     intent(in)  :: lon, lat, day_p, dt_d, dip
    type(t_time), intent(out) :: time
    real(DP),     intent(out) :: ht
    real(DP) :: num, jy, kokei, koi

    num = time_moon(2, lon, lat, day_p, dt_d, dip)
    if (num < 0.0_DP) then
      ! 月の南中がない場合はマイナスを設定
      time = t_time(0, 0, 0, -1, -1, -1)
      ht   = -1.0_DP
    else
      call conv_time(num * 24.0_DP, time)
      jy = (day_p + num + dt_d) / 365.25_DP  ! 経過ユリウス年(日)
      kokei = lon_moon(jy)                   ! 黄経(月)
      koi   = lat_moon(jy)                   ! 黄緯(月)
      ht = calc_height(lon, lat, kokei, koi, num, jy)
    end if
  end subroutine calc_moon_m

  ! 日の出/日の入/日の南中計算計算
  !
  ! :param(in) integer(4) kbn: 0: 日の出, 1: 日の入, 2: 日の南中
  ! :param(in) real(8)    lon: 経度
  ! :param(in) real(8)    lat: 緯度
  ! :param(in) real(8)  day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in) real(8)   dt_d: ΔT(日換算)
  ! :param(in) real(8)    dip: 地平線伏角
  ! :return    real(8)     tm: 出入時刻
  function time_sun(kbn, lon, lat, day_p, dt_d, dip) result(tm)
    implicit none
    integer(SP), intent(in) :: kbn
    real(DP),    intent(in) :: lon, lat, day_p, dt_d, dip
    real(DP) :: tm
    real(DP) :: jy, kokei, dist, sekkei, sekii
    real(DP) :: r, diff, ht, tm_sd, hang_diff, rev

    rev = 1.0_DP  ! 補正値初期値
    tm  = 0.5_DP  ! 逐次計算時刻(日)初期設定

    ! 逐次計算
    do while (abs(rev) > EPS)
      jy    = (day_p + tm + dt_d) / 365.25_DP       ! tm の経過ユリウス年
      kokei = lon_sun(jy)                           ! 太陽の黄経
      dist  = dist_sun(jy)                          ! 太陽の距離
      call ko2se(kokei, 0.0_DP, jy, sekkei, sekii)  ! 黄道 -> 赤道変換
      r     = 0.266994_DP / dist                    ! 太陽の視半径
      diff  = 0.0024428_DP / dist                   ! 太陽の視差
      ht    = -r - A_REF - dip + diff               ! 太陽の出入高度
      tm_sd = time_sidereal(lon, jy, tm)            ! 恒星時
      hang_diff = hour_ang_diff(lat, sekkei, sekii, tm_sd, ht, kbn)  ! 時角差
      rev = hang_diff / 360.0_DP                    ! 仮定時刻に対する補正値
      tm  = tm + rev
    end do
  end function time_sun

  ! 月の出/月の入/月の南中計算
  !
  ! :param(in) integer(4) kbn: 0: 日の出, 1: 日の入, 2: 日の南中
  ! :param(in) real(8)    lon: 経度
  ! :param(in) real(8)    lat: 緯度
  ! :param(in) real(8)  day_p: 2000年1月1日力学時正午からの経過日数(日)
  ! :param(in) real(8)   dt_d: ΔT(日換算)
  ! :param(in) real(8)    dip: 地平線伏角
  ! :return    real(8)     tm: 出入時刻
  function time_moon(kbn, lon, lat, day_p, dt_d, dip) result(tm)
    implicit none
    integer(SP), intent(in) :: kbn
    real(DP),    intent(in) :: lon, lat, day_p, dt_d, dip
    real(DP)    :: tm
    real(DP)    :: jy, kokei, koi, sekkei, sekii
    real(DP)    :: diff, ht, tm_sd, hang_diff, rev

    rev = 1.0_DP  ! 補正値初期値
    tm  = 0.5_DP  ! 逐次計算時刻(日)初期設定

    ! 逐次計算
    do while (abs(rev) > EPS)
      jy    = (day_p + tm + dt_d) / 365.25_DP    ! tm の経過ユリウス年
      kokei = lon_moon(jy)                       ! 月の黄経
      koi   = lat_moon(jy)                       ! 月の黄緯
      call ko2se(kokei, koi, jy, sekkei, sekii)  ! 黄道 -> 赤道変換
      ! 南中のときは計算しない
      if (kbn/= 2) then
        diff = diff_moon(jy)                     ! 月の視差
        ht   = diff - dip - A_REF                ! 月の出入高度
      end if
      tm_sd = time_sidereal(lon, jy, tm)         ! 恒星時
      hang_diff = hour_ang_diff(lat, sekkei, sekii, tm_sd, ht, kbn)  ! 時角差
      rev = hang_diff / 347.8_DP                 ! 仮定時刻に対する補正値
      tm  = tm + rev
    end do
    ! 月の出/月の入りがない場合は -1.0 とする
    if (tm < 0.0_DP .or. tm >= 1.0_DP) tm = -1.0_DP
  end function time_moon

  ! 月の視差計算
  !
  ! :param  real(8)   jy: 経過ユリウス年
  ! :return real(8) diff: 視差
  function diff_moon(jy) result(diff)
    implicit none
    real(DP), intent(in) :: jy
    real(DP) :: diff

    diff = 0.0003_DP * sin(PI_180 * norm_ang(227.0_DP  +  4412.0_DP   * jy)) &
       & + 0.0004_DP * sin(PI_180 * norm_ang(194.0_DP  +  3773.4_DP   * jy)) &
       & + 0.0005_DP * sin(PI_180 * norm_ang(329.0_DP  +  8545.4_DP   * jy)) &
       & + 0.0009_DP * sin(PI_180 * norm_ang(100.0_DP  + 13677.3_DP   * jy)) &
       & + 0.0028_DP * sin(PI_180 * norm_ang(  0.0_DP  +  9543.98_DP  * jy)) &
       & + 0.0078_DP * sin(PI_180 * norm_ang(325.7_DP  +  8905.34_DP  * jy)) &
       & + 0.0095_DP * sin(PI_180 * norm_ang(190.7_DP  +  4133.35_DP  * jy)) &
       & + 0.0518_DP * sin(PI_180 * norm_ang(224.98_DP +  4771.989_DP * jy)) &
       & + 0.9507_DP * sin(PI_180 * norm_ang(90.0_DP))
  end function diff_moon

  ! 太陽の黄経 λsun(jy) を計算する
  !
  ! :param(in)  real(8)  jy: 経過ユリウス年
  ! :return     real(8) lon: 黄経
  function lon_sun(jy) result(lon)
    implicit none
    real(DP), intent(in) :: jy
    real(DP) :: lon

    lon = 0.0003_DP * sin(PI_180 * norm_ang(329.7_DP  +   44.43_DP  * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(352.5_DP  + 1079.97_DP  * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang( 21.1_DP  +  720.02_DP  * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(157.3_DP  +  299.30_DP  * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(234.9_DP  +  315.56_DP  * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(291.2_DP  +   22.81_DP  * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(207.4_DP  +    1.50_DP  * jy)) &
      & + 0.0006_DP * sin(PI_180 * norm_ang( 29.8_DP  +  337.18_DP  * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(206.8_DP  +   30.35_DP  * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(153.3_DP  +   90.38_DP  * jy)) &
      & + 0.0008_DP * sin(PI_180 * norm_ang(132.5_DP  +  659.29_DP  * jy)) &
      & + 0.0013_DP * sin(PI_180 * norm_ang( 81.4_DP  +  225.18_DP  * jy)) &
      & + 0.0015_DP * sin(PI_180 * norm_ang(343.2_DP  +  450.37_DP  * jy)) &
      & + 0.0018_DP * sin(PI_180 * norm_ang(251.3_DP  +    0.20_DP  * jy)) &
      & + 0.0018_DP * sin(PI_180 * norm_ang(297.8_DP  + 4452.67_DP  * jy)) &
      & + 0.0020_DP * sin(PI_180 * norm_ang(247.1_DP  +  329.64_DP  * jy)) &
      & + 0.0048_DP * sin(PI_180 * norm_ang(234.95_DP +   19.341_DP * jy)) &
      & + 0.0200_DP * sin(PI_180 * norm_ang(355.05_DP +  719.981_DP * jy)) &
      & + (1.9146_DP - 0.00005_DP * jy) &
        & * sin(PI_180 * norm_ang(357.538_DP + 359.991_DP * jy)) &
      & + norm_ang(280.4603_DP + 360.00769_DP * jy)
  end function lon_sun

  ! 太陽の距離 r(jy) を計算する
  !
  ! :param(in) real(8)   jy: 経過ユリウス年
  ! :return    real(8) dist: 距離
  function dist_sun(jy) result(dist)
    implicit none
    real(DP), intent(in) :: jy
    real(DP) :: dist

    dist = 0.000007_DP * sin(PI_180 * norm_ang(156.0_DP +  329.6_DP  * jy)) &
       & + 0.000007_DP * sin(PI_180 * norm_ang(254.0_DP +  450.4_DP  * jy)) &
       & + 0.000013_DP * sin(PI_180 * norm_ang( 27.8_DP + 4452.67_DP * jy)) &
       & + 0.000030_DP * sin(PI_180 * norm_ang( 90.0_DP))                   &
       & + 0.000091_DP * sin(PI_180 * norm_ang(265.1_DP +  719.98_DP * jy)) &
       & + (0.007256_DP - 0.0000002_DP * jy) &
         & * sin(PI_180 * norm_ang(267.54_DP + 359.991_DP * jy))
    dist = 10.0_DP ** dist
  end function dist_sun

  ! 月の黄経 λmoon(jy) を計算する
  !
  ! :param(in) real(8)  jy: 経過ユリウス年
  ! :return    real(8) lon: 黄経
  function lon_moon(jy) result(lon)
    implicit none
    real(DP), intent(in) :: jy
    real(DP) :: lon
    real(DP) :: am

    am  = 0.0006_DP * sin(PI_180 * norm_ang( 54.0_DP   + 19.3_DP       * jy)) &
      & + 0.0006_DP * sin(PI_180 * norm_ang( 71.0_DP   +  0.2_DP       * jy)) &
      & + 0.0020_DP * sin(PI_180 * norm_ang( 55.0_DP   + 19.34_DP      * jy)) &
      & + 0.0040_DP * sin(PI_180 * norm_ang(119.5_DP   +  1.33_DP      * jy))
    lon = 0.0003_DP * sin(PI_180 * norm_ang(280.0_DP   + 23221.3_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(161.0_DP   +    40.7_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(311.0_DP   +  5492.0_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(147.0_DP   + 18089.3_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang( 66.0_DP   +  3494.7_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang( 83.0_DP   +  3814.0_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang( 20.0_DP   +   720.0_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang( 71.0_DP   +  9584.7_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(278.0_DP   +   120.1_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(313.0_DP   +   398.7_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(332.0_DP   +  5091.3_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(114.0_DP   + 17450.7_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(181.0_DP   + 19088.0_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(247.0_DP   + 22582.7_DP    * jy)) &
      & + 0.0006_DP * sin(PI_180 * norm_ang(128.0_DP   +  1118.7_DP    * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(216.0_DP   +   278.6_DP    * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(275.0_DP   +  4853.3_DP    * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(140.0_DP   +  4052.0_DP    * jy)) &
      & + 0.0008_DP * sin(PI_180 * norm_ang(204.0_DP   +  7906.7_DP    * jy)) &
      & + 0.0008_DP * sin(PI_180 * norm_ang(188.0_DP   + 14037.3_DP    * jy)) &
      & + 0.0009_DP * sin(PI_180 * norm_ang(218.0_DP   +  8586.0_DP    * jy)) &
      & + 0.0011_DP * sin(PI_180 * norm_ang(276.5_DP   + 19208.02_DP   * jy)) &
      & + 0.0012_DP * sin(PI_180 * norm_ang(339.0_DP   + 12678.71_DP   * jy)) &
      & + 0.0016_DP * sin(PI_180 * norm_ang(242.2_DP   + 18569.38_DP   * jy)) &
      & + 0.0018_DP * sin(PI_180 * norm_ang(  4.1_DP   +  4013.29_DP   * jy)) &
      & + 0.0020_DP * sin(PI_180 * norm_ang( 55.0_DP   +    19.34_DP   * jy)) &
      & + 0.0021_DP * sin(PI_180 * norm_ang(105.6_DP   +  3413.37_DP   * jy)) &
      & + 0.0021_DP * sin(PI_180 * norm_ang(175.1_DP   +   719.98_DP   * jy)) &
      & + 0.0021_DP * sin(PI_180 * norm_ang( 87.5_DP   +  9903.97_DP   * jy)) &
      & + 0.0022_DP * sin(PI_180 * norm_ang(240.6_DP   +  8185.36_DP   * jy)) &
      & + 0.0024_DP * sin(PI_180 * norm_ang(252.8_DP   +  9224.66_DP   * jy)) &
      & + 0.0024_DP * sin(PI_180 * norm_ang(211.9_DP   +   988.63_DP   * jy)) &
      & + 0.0026_DP * sin(PI_180 * norm_ang(107.2_DP   + 13797.39_DP   * jy)) &
      & + 0.0027_DP * sin(PI_180 * norm_ang(272.5_DP   +  9183.99_DP   * jy)) &
      & + 0.0037_DP * sin(PI_180 * norm_ang(349.1_DP   +  5410.62_DP   * jy)) &
      & + 0.0039_DP * sin(PI_180 * norm_ang(111.3_DP   + 17810.68_DP   * jy)) &
      & + 0.0040_DP * sin(PI_180 * norm_ang(119.5_DP   +     1.33_DP   * jy)) &
      & + 0.0040_DP * sin(PI_180 * norm_ang(145.6_DP   + 18449.32_DP   * jy)) &
      & + 0.0040_DP * sin(PI_180 * norm_ang( 13.2_DP   + 13317.34_DP   * jy)) &
      & + 0.0048_DP * sin(PI_180 * norm_ang(235.0_DP   +    19.34_DP   * jy)) &
      & + 0.0050_DP * sin(PI_180 * norm_ang(295.4_DP   +  4812.66_DP   * jy)) &
      & + 0.0052_DP * sin(PI_180 * norm_ang(197.2_DP   +   319.32_DP   * jy)) &
      & + 0.0068_DP * sin(PI_180 * norm_ang( 53.2_DP   +  9265.33_DP   * jy)) &
      & + 0.0079_DP * sin(PI_180 * norm_ang(278.2_DP   +  4493.34_DP   * jy)) &
      & + 0.0085_DP * sin(PI_180 * norm_ang(201.5_DP   +  8266.71_DP   * jy)) &
      & + 0.0100_DP * sin(PI_180 * norm_ang( 44.89_DP  + 14315.966_DP  * jy)) &
      & + 0.0107_DP * sin(PI_180 * norm_ang(336.44_DP  + 13038.696_DP  * jy)) &
      & + 0.0110_DP * sin(PI_180 * norm_ang(231.59_DP  +  4892.052_DP  * jy)) &
      & + 0.0125_DP * sin(PI_180 * norm_ang(141.51_DP  + 14436.029_DP  * jy)) &
      & + 0.0153_DP * sin(PI_180 * norm_ang(130.84_DP  +   758.698_DP  * jy)) &
      & + 0.0305_DP * sin(PI_180 * norm_ang(312.49_DP  +  5131.979_DP  * jy)) &
      & + 0.0348_DP * sin(PI_180 * norm_ang(117.84_DP  +  4452.671_DP  * jy)) &
      & + 0.0410_DP * sin(PI_180 * norm_ang(137.43_DP  +  4411.998_DP  * jy)) &
      & + 0.0459_DP * sin(PI_180 * norm_ang(238.18_DP  +  8545.352_DP  * jy)) &
      & + 0.0533_DP * sin(PI_180 * norm_ang( 10.66_DP  + 13677.331_DP  * jy)) &
      & + 0.0572_DP * sin(PI_180 * norm_ang(103.21_DP  +  3773.363_DP  * jy)) &
      & + 0.0588_DP * sin(PI_180 * norm_ang(214.22_DP  +   638.635_DP  * jy)) &
      & + 0.1143_DP * sin(PI_180 * norm_ang(  6.546_DP +  9664.0404_DP * jy)) &
      & + 0.1856_DP * sin(PI_180 * norm_ang(177.525_DP +   359.9905_DP * jy)) &
      & + 0.2136_DP * sin(PI_180 * norm_ang(269.926_DP +  9543.9773_DP * jy)) &
      & + 0.6583_DP * sin(PI_180 * norm_ang(235.700_DP +  8905.3422_DP * jy)) &
      & + 1.2740_DP * sin(PI_180 * norm_ang(100.738_DP +  4133.3536_DP * jy)) &
      & + 6.2887_DP * sin(PI_180 * norm_ang(134.961_DP +  4771.9886_DP * jy + am)) &
      & + norm_ang(218.3161_DP + 4812.67881_DP * jy)
  end function lon_moon

  ! 月の黄緯 βmoon(jy) を計算する
  !
  ! :param(in) real(8)  jy: 経過ユリウス年
  ! :return    real(8) lat:  黄緯
  function lat_moon(jy) result(lat)
    implicit none
    real(DP), intent(in) :: jy
    real(DP) :: lat
    real(DP) :: bm

    bm  = 0.0005_DP * sin(PI_180 * norm_ang(307.0_DP   + 19.4_DP       * jy)) &
      & + 0.0026_DP * sin(PI_180 * norm_ang( 55.0_DP   + 19.34_DP      * jy)) &
      & + 0.0040_DP * sin(PI_180 * norm_ang(119.5_DP   +  1.33_DP      * jy)) &
      & + 0.0043_DP * sin(PI_180 * norm_ang(322.1_DP   + 19.36_DP      * jy)) &
      & + 0.0267_DP * sin(PI_180 * norm_ang(234.95_DP  + 19.341_DP     * jy))
    lat = 0.0003_DP * sin(PI_180 * norm_ang(234.0_DP   + 19268.0_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(146.0_DP   +  3353.3_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(107.0_DP   + 18149.4_DP    * jy)) &
      & + 0.0003_DP * sin(PI_180 * norm_ang(205.0_DP   + 22642.7_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(147.0_DP   + 14097.4_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang( 13.0_DP   +  9325.4_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang( 81.0_DP   + 10242.6_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(238.0_DP   + 23281.3_DP    * jy)) &
      & + 0.0004_DP * sin(PI_180 * norm_ang(311.0_DP   +  9483.9_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(239.0_DP   +  4193.4_DP    * jy)) &
      & + 0.0005_DP * sin(PI_180 * norm_ang(280.0_DP   +  8485.3_DP    * jy)) &
      & + 0.0006_DP * sin(PI_180 * norm_ang( 52.0_DP   + 13617.3_DP    * jy)) &
      & + 0.0006_DP * sin(PI_180 * norm_ang(224.0_DP   +  5590.7_DP    * jy)) &
      & + 0.0007_DP * sin(PI_180 * norm_ang(294.0_DP   + 13098.7_DP    * jy)) &
      & + 0.0008_DP * sin(PI_180 * norm_ang(326.0_DP   +  9724.1_DP    * jy)) &
      & + 0.0008_DP * sin(PI_180 * norm_ang( 70.0_DP   + 17870.7_DP    * jy)) &
      & + 0.0010_DP * sin(PI_180 * norm_ang( 18.0_DP   + 12978.66_DP   * jy)) &
      & + 0.0011_DP * sin(PI_180 * norm_ang(138.3_DP   + 19147.99_DP   * jy)) &
      & + 0.0012_DP * sin(PI_180 * norm_ang(148.2_DP   +  4851.36_DP   * jy)) &
      & + 0.0012_DP * sin(PI_180 * norm_ang( 38.4_DP   +  4812.68_DP   * jy)) &
      & + 0.0013_DP * sin(PI_180 * norm_ang(155.4_DP   +   379.35_DP   * jy)) &
      & + 0.0013_DP * sin(PI_180 * norm_ang( 95.8_DP   +  4472.03_DP   * jy)) &
      & + 0.0014_DP * sin(PI_180 * norm_ang(219.2_DP   +   299.96_DP   * jy)) &
      & + 0.0015_DP * sin(PI_180 * norm_ang( 45.8_DP   +  9964.00_DP   * jy)) &
      & + 0.0015_DP * sin(PI_180 * norm_ang(211.1_DP   +  9284.69_DP   * jy)) &
      & + 0.0016_DP * sin(PI_180 * norm_ang(135.7_DP   +   420.02_DP   * jy)) &
      & + 0.0017_DP * sin(PI_180 * norm_ang( 99.8_DP   + 14496.06_DP   * jy)) &
      & + 0.0018_DP * sin(PI_180 * norm_ang(270.8_DP   +  5192.01_DP   * jy)) &
      & + 0.0018_DP * sin(PI_180 * norm_ang(243.3_DP   +  8206.68_DP   * jy)) &
      & + 0.0019_DP * sin(PI_180 * norm_ang(230.7_DP   +  9244.02_DP   * jy)) &
      & + 0.0021_DP * sin(PI_180 * norm_ang(170.1_DP   +  1058.66_DP   * jy)) &
      & + 0.0022_DP * sin(PI_180 * norm_ang(331.4_DP   + 13377.37_DP   * jy)) &
      & + 0.0025_DP * sin(PI_180 * norm_ang(196.5_DP   +  8605.38_DP   * jy)) &
      & + 0.0034_DP * sin(PI_180 * norm_ang(319.9_DP   +  4433.31_DP   * jy)) &
      & + 0.0042_DP * sin(PI_180 * norm_ang(103.9_DP   + 18509.35_DP   * jy)) &
      & + 0.0043_DP * sin(PI_180 * norm_ang(307.6_DP   +  5470.66_DP   * jy)) &
      & + 0.0082_DP * sin(PI_180 * norm_ang(144.9_DP   +  3713.33_DP   * jy)) &
      & + 0.0088_DP * sin(PI_180 * norm_ang(176.7_DP   +  4711.96_DP   * jy)) &
      & + 0.0093_DP * sin(PI_180 * norm_ang(277.4_DP   +  8845.31_DP   * jy)) &
      & + 0.0172_DP * sin(PI_180 * norm_ang(  3.18_DP  + 14375.997_DP  * jy)) &
      & + 0.0326_DP * sin(PI_180 * norm_ang(328.96_DP  + 13737.362_DP  * jy)) &
      & + 0.0463_DP * sin(PI_180 * norm_ang(172.55_DP  +   698.667_DP  * jy)) &
      & + 0.0554_DP * sin(PI_180 * norm_ang(194.01_DP  +  8965.374_DP  * jy)) &
      & + 0.1732_DP * sin(PI_180 * norm_ang(142.427_DP +  4073.3220_DP * jy)) &
      & + 0.2777_DP * sin(PI_180 * norm_ang(138.311_DP +    60.0316_DP * jy)) &
      & + 0.2806_DP * sin(PI_180 * norm_ang(228.235_DP +  9604.0088_DP * jy)) &
      & + 5.1282_DP * sin(PI_180 * norm_ang( 93.273_DP +  4832.0202_DP * jy + bm))
  end function lat_moon

  ! 角度の正規化 (引数の範囲を 0≦θ＜360 にする)
  !
  ! :param(in) real(8) ang_src: 角度(正規化前)
  ! :return    real(8)     ang: 角度(正規化後)
  function norm_ang(ang_src) result(ang)
    implicit none
    real(DP), intent(in) :: ang_src
    real(DP) :: ang

    ang = ang_src - 360.0_DP * int(ang_src / 360.0_DP)
  end function norm_ang

  ! 観測地点の恒星時Θ(度)の計算
  !
  ! :param(in)  real(8) lon: 経度
  ! :param(in)  real(8)  jy: 経過ユリウス年
  ! :param(in)  real(8)   t: 時刻 ( 0.xxxx日 )
  ! :return     real(8)  tm: 観測地点の恒星時Θ(度)
  function time_sidereal(lon, jy, t) result(tm)
    implicit none
    real(DP), intent(in) :: lon, jy, t
    real(DP) :: tm

    tm = 325.4606_DP &
     & + 360.007700536_DP * jy &
     & + 0.00000003879_DP * jy * jy &
     & + 360.0_DP * t &
     & + lon
    tm = norm_ang(tm)
  end function time_sidereal

  ! 出入点(k)の時角(tk)と天体の時角(t)との差(dt=tk-t)を計算する
  !
  ! :param(in) real(8)    lat: 緯度
  ! :param(in) real(8) sekkei: 天体の赤経 (α(T)(度))
  ! :param(in) real(8)  sekii: 天体の赤緯 (δ(T)(度))
  ! :param(in) real(8)  tm_sd: 恒星時Θ(度)
  ! :param(in) real(8)     ht: 観測地点の出没高度(度)
  ! :param(in) integer(4) kbn: 0: 出, 1: 入, 2: 南中
  ! return     real(8)     dt: 時角の差
  function hour_ang_diff(lat, sekkei, sekii, tm_sd, ht, kbn) result(dt)
    implicit none
    real(DP),    intent(in) :: lat, sekkei, sekii, tm_sd, ht
    integer(SP), intent(in) :: kbn
    real(DP) :: dt
    real(DP) :: tk

    ! 南中の場合は天体の時角を返す
    if (kbn== 2) then
      tk = 0.0_DP
    else
      tk = sin(PI_180 * ht) - sin(PI_180 * sekii) * sin(PI_180 * lat)
      tk = tk / (cos(PI_180 * sekii) * cos(PI_180 * lat))
      ! 出没点の時角
      tk = acos(tk) / PI_180
      ! tkは出のときマイナス、入のときプラス
      if (kbn == 0 .and. tk > 0.0_DP) tk = dsign(tk, -1.0_DP)
      if (kbn == 1 .and. tk < 0.0_DP) tk = dsign(tk,  1.0_DP)
    end if
    ! 天体の時角
    dt = tk - tm_sd + sekkei
    ! dtの絶対値を180°以下に調整
    if (dt >  180.0_DP) then
      do while (dt > 180.0_DP)
        dt = dt - 360.0_DP
      end do
    end if
    if (dt < -180.0_DP) then
      do while (dt < -180.0_DP)
        dt = dt + 360.0_DP
      end do
    end if
  end function hour_ang_diff

  ! 時刻(t)における黄経、黄緯(λ(jy),β(jy))の天体の方位角(ang)計算
  !
  ! :param(in) real(8)   lon: 経度
  ! :param(in) real(8)   lat: 緯度
  ! :param(in) real(8) kokei: 天体の黄経(λ(T)(度))
  ! :param(in) real(8)   koi: 天体の黄緯(β(T)(度))
  ! :param(in) real(8)    jy: 経過ユリウス年
  ! :param(in) real(8)     t: 時刻 (0.xxxx日)
  ! :return    real(8)   ang: 角度(xx.x度)
  function calc_angle(lon, lat, kokei, koi, num, jy) result(ang)
    implicit none
    real(DP), intent(in) :: lon, lat, kokei, koi, num, jy
    real(DP) :: ang
    real(DP) :: sekkei, sekii, tm_sd, hang, a_0, a_1

    call ko2se(kokei, koi, jy, sekkei, sekii)  ! 黄道 -> 赤道変換
    tm_sd = time_sidereal(lon, jy, num)        ! 恒星時
    hang  = tm_sd - sekkei                     ! 天体の時角
    ! 天体の方位角
    a_0 = -cos(PI_180 * sekii) * sin(PI_180 * hang)
    a_1 = sin(PI_180 * sekii) * cos(PI_180 * lat) &
      & - cos(PI_180 * sekii) * sin(PI_180 * lat) * cos(PI_180 * hang)
    ang = atan(a_0 / a_1) / PI_180
    ! 分母がプラスのときは -90°< ang < 90°
    if (a_1 > 0.0_DP .and. ang < 0.0_DP) ang = ang + 360.0_DP
    ! 分母がマイナスのときは 90°< ang < 270° → 180°加算する
    if (a_1 < 0.0_DP) ang = ang + 180.0_DP
    ang = nint(ang * 100.0_DP) / 100.0_DP
  end function calc_angle

  ! 時刻(t)における黄経、黄緯(λ(jy),β(jy))の天体の高度(height)計算
  !
  ! :param(in) real(8)   lon: 経度
  ! :param(in) real(8)   lat: 緯度
  ! :param(in) real(8) kokei: 天体の黄経(λ(T)(度))
  ! :param(in) real(8)   koi: 天体の黄緯(β(T)(度))
  ! :param(in) real(8)    jy: 経過ユリウス年
  ! :param(in) real(8)     t: 時刻 (0.xxxx日)
  ! :return    real(8)    ht: 高度(xx.x度)
  function calc_height(lon, lat, kokei, koi, num, jy) result(ht)
    implicit none
    real(DP), intent(in) :: lon, lat, kokei, koi, num, jy
    real(DP) :: ht
    real(DP) :: sekkei, sekii, tm_sd, sd, h, tan_ht

    call ko2se(kokei, koi, jy, sekkei, sekii)  ! 黄道 -> 赤道変換
    tm_sd = time_sidereal(lon, jy, num)        ! 恒星時
    sd    = tm_sd - sekkei                     ! 天体の時角
    ! 天体の高度
    ht  = sin(PI_180 * sekii) * sin(PI_180 * lat) &
      & + cos(PI_180 * sekii) * cos(PI_180 * lat) * cos(PI_180 * sd)
    ht  = asin(ht) / PI_180

    ! フランスの天文学者ラドー(R.Radau)の計算式
    ! * 平均大気差と1秒程度の差で大気差を求めることが可能
    !   (標準的大気(気温10ﾟC，気圧1013.25hPa)の場合)
    !   (視高度 4ﾟ以上)
    tan_ht = tan(PI_180 * (90.0_DP - ht))
    h  = 58.76_DP   * tan_ht &
     & -  0.406_DP  * tan_ht * tan_ht &
     & -  0.0192_DP * tan_ht * tan_ht * tan_ht
    h = h / 3600.0_DP

    ht = ht + h
    ht = nint(ht * 100.0_DP) / 100.0_DP
  end function calc_height

  ! 黄道座標 -> 赤道座標変換
  !
  ! :param(in)  real(8)  kokei: 黄経(λ(jy)(度))
  ! :param(in)  real(8)    koi: 黄緯(β(jy)(度))
  ! :param(in)  real(8)     jy: 経過ユリウス年
  ! :param(out) real(8) sekkei: 赤経(α(jy)(度))
  ! :param(out) real(8)  sekii: 赤緯(δ(jy)(度))
  subroutine ko2se(kokei, koi, jy, sekkei, sekii)
    implicit none
    real(DP), intent(in)  :: kokei, koi, jy
    real(DP), intent(out) :: sekkei, sekii
    real(DP) :: ang_k, lmd, bet, a, b, c

    ang_k = (23.439291_DP - 0.000130042_DP * jy) * PI_180  ! 黄道傾角
    lmd = kokei * PI_180  ! 赤経
    bet = koi   * PI_180  ! 赤緯
    a =  cos(bet) * cos(lmd)
    b = -sin(bet) * sin(ang_k) + cos(bet) * sin(lmd) * cos(ang_k)
    c =  sin(bet) * cos(ang_k) + cos(bet) * sin(lmd) * sin(ang_k)
    sekkei  = atan(b / a) / PI_180
    ! aがマイナスのときは 90°< α < 270° → 180°加算する
    if (a < 0.0_DP) sekkei = sekkei + 180.0_DP
    !sekii = c / sqrt(a * a + b * b)
    !sekii = atan(sekii) / PI_180
    !上記の sekii の計算は以下と同じ
    sekii = asin(c) / PI_180
  end subroutine ko2se

  ! 時間：数値->時間：時分変換(xx.xxxx -> hh:mm:ss)
  !
  ! :param(in)  real(8)         n: 時刻 ( xx.xxxx日 )
  ! :param(out) type(t_time) time: 時刻(hh:mm:ss)
  subroutine conv_time(n, time)
    implicit none
    real(DP),     intent(in)  :: n
    type(t_time), intent(out) :: time
    integer(SP) :: h, m, s
    real(DP)    :: n_2, n_3

    ! 整数部(時)
    h = int(n)
    ! 小数部
    n_2 = n - real(h, DP)
    ! (分)計算
    m = int(n_2 * 60.0_DP)
    ! (秒)計算
    n_3 = n_2 - (real(m, DP) / 60.0_DP)
    s = nint(n_3 * 3600.0_DP)
    time = t_time(0, 0, 0, h, m, s)
  end subroutine conv_time
end module calc

