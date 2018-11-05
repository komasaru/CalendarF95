!****************************************************
! 太陽・月の視位置計算
! * JPLEPH(JPL の DE430 バイナリデータ)を読み込み、視位置を計算する
!
!   Date          Author          Version
!   2018.10.25    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数 : JST
!        * JST は 日本標準時（省略可）
!          書式：YYYYMMDDHHMMSSUUUUUU
!          無指定なら現在(システム日時)を JST とみなす。
! ---
! * 構造型 type(t_time) は time    モジュール内で定義
! * 構造型 type(t_bin)  は eph_jpl モジュール内で定義
!****************************************************
!
program apparent_sun_moon_jpl
  use const, only : SP, DP, PI, FMT_DT_0, FMT_DT_1
  use time
  use apos
  use eph_jpl
  implicit none
  type(t_time)  :: jst, utc, tdb
  real(DP)      :: jd_tdb
  type(t_bin)   :: bin  ! バイナリデータ用構造型
  integer(SP)   :: i
  real(DP)      :: jds(2)
  ! 時刻 t1(TDB), t2(TDB) における位置・速度(ICRS座標)用配列
  real(DP) :: icrs_1(6, 3), icrs_2(6, 3)
  real(DP) :: d_e_m, d_e_s                  ! 地球と月・太陽の距離
  real(DP) :: r_s, r_m, r_e                 ! 太陽・月・地球の半径
  real(DP) :: apos_eq(3, 2), apos_ec(3, 2)  ! 太陽・月の視位置
  real(DP) :: rad_para(2, 2)                ! 太陽・月の視半径・視差

  ! コマンドライン引数(JST)取得, UTC 計算
  call get_arg(jst)
  if (jst%year == 0) then
    stop
  else
    call jst2utc(jst, utc)
  end if

  ! === 時刻 t2 のユリウス日
  call utc2tdb(utc, tdb)
  call gc2jd(tdb, jd_tdb)

  ! === バイナリデータ読み込み
  call get_bin(jd_tdb, bin)

  ! === 時刻 t2(= TDB) における地球・月・太陽の位置・速度（ICRS 座標）の計算
  call get_icrs( 3, jd_tdb, icrs_2(1:6, 1))
  call get_icrs(10, jd_tdb, icrs_2(1:6, 2))
  call get_icrs(11, jd_tdb, icrs_2(1:6, 3))

  ! === 時刻 t2(= TDB) における地球と月・太陽の距離
  call get_dist_e(icrs_2, d_e_m, d_e_s)

  ! === 地球・月・太陽の半径取得
  call get_r(jd_tdb, bin%cnam, bin%cval, r_e, r_m, r_s)

  ! === 太陽・月の視位置計算
  call calc_sun(jd_tdb, icrs_2, d_e_s, r_e, r_s, bin%au, &
    & apos_eq(1:3, 1), apos_ec(1:3, 1), rad_para(1:2, 1))
  call calc_moon(jd_tdb, icrs_2, d_e_m, r_e, r_m, bin%au, &
    & apos_eq(1:3, 2), apos_ec(1:3, 2), rad_para(1:2, 2))

  ! 結果出力
  print '("* 計算対象時刻 (JST) = ", A)', date_fmt(jst)
  print '("               (UTC) = ", A)', date_fmt(utc)
  print '("               (TDB) = ", A)', date_fmt(tdb)
  print '("                (JD) = ", F18.10, " (days)")', jd_tdb
  print *
  print '(A)', "* 視位置：太陽"
  print '("  = [赤経: ", F14.10, " rad, 赤緯: ", F14.10, " rad]")', &
    & apos_eq(1:2, 1)
  print '("  = [赤経: ", F14.10, " deg, 赤緯: ", F14.10, " deg]")', &
    & apos_eq(1:2, 1) * 180.0_DP / PI
  print '("  = [黄経: ", F14.10, " rad, 黄緯: ", F14.10, " rad]")', &
    & apos_ec(1:2, 1)
  print '("  = [黄経: ", F14.10, " deg, 黄緯: ", F14.10, " deg]")', &
    & apos_ec(1:2, 1) * 180.0_DP / PI
  print '(A)', "* 視位置：月"
  print '("  = [赤経: ", F14.10, " rad, 赤緯: ", F14.10, " rad]")', &
    & apos_eq(1:2, 2)
  print '("  = [赤経: ", F14.10, " deg, 赤緯: ", F14.10, " deg]")', &
    & apos_eq(1:2, 2) * 180.0_DP / PI
  print '("  = [黄経: ", F14.10, " rad, 黄緯: ", F14.10, " rad]")', &
    & apos_ec(1:2, 2)
  print '("  = [黄経: ", F14.10, " deg, 黄緯: ", F14.10, " deg]")', &
    & apos_ec(1:2, 2) * 180.0_DP / PI
  print '(A)', "* 視黄経差：太陽 - 月"
  print '("  = ", F14.10, " rad")', &
    & apos_ec(1, 1) - apos_ec(1, 2)
  print '("  = ", F14.10, " deg")', &
    & (apos_ec(1, 1) - apos_ec(1, 2)) * 180.0 / PI
  print '(A)', "* 距離：太陽"
  print '("  = ", F12.10, " AU")', apos_ec(3, 1)
  print '("  = ", F12.2,  " km")', apos_ec(3, 1) * bin%au
  print '(A)', "* 距離：月"
  print '("  = ", F12.10, " AU")', apos_ec(3, 2)
  print '("  = ", F12.2,  " km")', apos_ec(3, 2) * bin%au
  print '(A)', "* 視半径：太陽"
  print '("  = ", F6.2,   " ″")', rad_para(1, 1)
  print '(A)', "* 視半径：月"
  print '("  = ", F6.2,   " ″")', rad_para(1, 2)
  print '(A)', "* （地平）視差：太陽"
  print '("  = ", F6.2,   " ″")', rad_para(2, 1)
  print '(A)', "* （地平）視差：月"
  print '("  = ", F7.2,   " ″")', rad_para(2, 2)

  stop
contains
  ! コマンドライン引数取得
  ! * YYYYMMDDHHMMSSUUUUUU 形式
  ! * 20桁超入力された場合は、21桁目以降の部分は切り捨てる
  ! * コマンドライン引数がなければ、システム日付を JST とする
  ! * 日時の整合性チェックは行わない
  !
  ! :param(inout) type(t_time) jst
  subroutine get_arg(jst)
    implicit none
    type(t_time), intent(inout) :: jst
    character(20) :: gc
    integer(SP)   :: dt(8)

    if (iargc() == 0) then
      call date_and_time(values=dt)
      jst = t_time(dt(1), dt(2), dt(3), &
        & dt(5), dt(6), dt(7), dt(8) * 1000)
    else
      call getarg(1, gc)
      if (len(trim(gc)) /= 20) then
        print *, "Format: YYYYMMDDHHMMSSUUUUUU"
        return
      end if
      read (gc, FMT_DT_0) jst
    end if
  end subroutine get_arg

  ! UTC -> TDB
  !
  ! :param(in)  type(t_time) utc
  ! :param(out) type(t_time) tdb
  subroutine utc2tdb(utc, tdb)
    implicit none
    type(t_time), intent(in)  :: utc
    type(t_time), intent(out) :: tdb
    type(t_time) :: tai, tt, tcb
    integer(SP)  :: utc_tai
    real(DP)     :: jd, jd_tcb

    call utc2utc_tai(utc, utc_tai)
    call utc2tai(utc, utc_tai, tai)
    call tai2tt(tai, tt)
    call gc2jd(utc, jd)
    call tt2tcb(tt, jd, tcb)
    call gc2jd(tcb, jd_tcb)
    call tcb2tdb(tcb, jd_tcb, tdb)
  end subroutine utc2tdb

  ! 地球／月／太陽の半径取得
  !
  ! :param(in)  real(8)              jd
  ! :param(in)  character(6) cnam(1000)
  ! :param(in)  real(8)      cval(1000)
  ! :param(out) real(8)             r_e
  ! :param(out) real(8)             r_m
  ! :param(out) real(8)             r_s
  subroutine get_r(jd, cnam, cval, r_e, r_m, r_s)
    implicit none
    character(*), intent(in)  :: cnam(1000)
    real(DP),     intent(in)  :: jd, cval(1000)
    real(DP),     intent(out) :: r_e, r_m, r_s

    do i = 1, 1000
      select case (trim(cnam(i)))
      case("ASUN")
        r_s = cval(i)
      case("AM")
        r_m = cval(i)
      case("RE")
        r_e = cval(i)
      end select
    end do
  end subroutine get_r
end program apparent_sun_moon_jpl

