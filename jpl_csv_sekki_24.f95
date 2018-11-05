!****************************************************
! 二十四節気一覧(CSV 出力)
! * 計算対象：「Y_MIN - 1 年〜 Y_MAX + 1 年」
!
!   Date          Author          Version
!   2018.10.27    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time    モジュール内で定義
! * 構造型 type(t_bin)  は eph_jpl モジュール内で定義
!****************************************************
!
program jpl_sekki_24
  use const, only : SP, DP, Y_MIN, Y_MAX, JST_D, PI
  use apos
  use eph_jpl
  use time
  implicit none
  character(*), parameter :: F_CSV      = "csv/sekki_24.csv"
  integer(SP),  parameter :: UID_CSV    = 11  ! 10 は JPLEPH で使用
  integer(SP),  parameter :: LOOP_LIMIT = 50
  real(DP),     parameter :: EPS        = 1.0e-8_DP
  type(t_time) :: tm_l, tm_h, tm_x
  integer(SP)  :: year, ios
  integer(SP)  :: y, m, d, hh = 0, mm = 0, ss = 0, us = 0
  real(DP)     :: jd, sekki_x, kokei

  ! 書き込み用 CSV ファイル OPEN
  open (unit   = UID_CSV,     &
      & iostat = ios,         &
      & file   = F_CSV,       &
      & action = "write",     &
      & form   = "formatted", &
      & status = "new")
  if (ios /= 0) then
    print *, "[ERROR] Failed to open file: " // F_CSV
    stop
  end if

  ! 計算
  do y = Y_MIN - 1, Y_MAX + 1
    tm_l = t_time(y, 1, 1, 0, 0, 0, 0)
    tm_h = t_time(y, 1, 1, 0, 0, 0, 0)
    tm_h%day = tm_h%day + 29
    call norm_time(tm_h)
    sekki_x = 285.0_DP
    do while (tm_l%year == y)
      call bisection(tm_l, tm_h, sekki_x, tm_x)
      print '("* ", I4, ":", I3, " - ", &
          & I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)', &
          & y, int(sekki_x), &
          & tm_x%year, tm_x%month, tm_x%day, &
          & tm_x%hour, tm_x%minute, tm_x%second
      write (UID_CSV, &
          & '(I4, ",", I2, ",", I2 ",", I3, ",", &
          & I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)') &
          & tm_x%year, tm_x%month, tm_x%day, int(sekki_x), &
          & tm_x%year, tm_x%month, tm_x%day, &
          & tm_x%hour, tm_x%minute, tm_x%second
      tm_l = tm_x
      tm_h = tm_l
      tm_h%day = tm_h%day + 29
      call norm_time(tm_h)
      sekki_x = sekki_x + 15.0_DP
      if (sekki_x == 360.0_DP) sekki_x = 0.0
      if (sekki_x == 285.0_DP) exit
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  stop
contains
  ! Bisection method
  !
  ! :param(in)  type(t_time) tm_l
  ! :param(in)  type(t_time) tm_h
  ! :param(in)  real(8)   sekki_x
  ! :param(out) type(t_time) tm_x
  subroutine bisection(tm_l, tm_h, sekki_x, tm_x)
    implicit none
    type(t_time), intent(in)  :: tm_l, tm_h
    real(DP),     intent(in)  :: sekki_x
    type(t_time), intent(out) :: tm_x
    integer(SP)  :: i
    real(DP)     :: jd_l, jd_h, jd_x, kokei_x

    call gc2jd(tm_l, jd_l)
    call gc2jd(tm_h, jd_h)
    do i = 1, LOOP_LIMIT
      jd_x = (jd_l + jd_h) / 2.0_DP
      call comp_kokei(jd_x, kokei_x)
      if (kokei_x > sekki_x) then
        if (kokei_x - sekki_x < 15.0_DP) then
          jd_h = jd_x
        else
          jd_l = jd_x
        end if
      else
        if (kokei_x - sekki_x < 15.0_DP) then
          jd_l = jd_x
        else
          jd_h = jd_x
        end if
      end if
      if (kokei_x == sekki_x) exit
      if (jd_h - jd_l < EPS) exit
    end do
    call jd2gc(jd_x, tm_x)
  end subroutine bisection

  ! 太陽・月の視黄経計算
  !
  ! :param real(8)         jd: Julian Day
  ! :param real(8)  kokei_sun: 視黄経（太陽）
  subroutine comp_kokei(jd, kokei_sun)
    implicit none
    real(DP), intent(in)  :: jd
    real(DP), intent(out) :: kokei_sun
    type(t_time) :: utc, tdb
    real(DP)     :: jd_tdb
    type(t_bin)  :: bin
    integer(SP)  :: i
    real(DP)     :: jds(2)
    ! 時刻 t1(TDB), t2(TDB) における位置・速度(ICRS座標)用配列
    real(DP) :: icrs_1(6, 3), icrs_2(6, 3)
    real(DP) :: d_e_m, d_e_s            ! 地球と月・太陽の距離
    real(DP) :: r_s, r_m, r_e           ! 太陽・月・地球の半径
    real(DP) :: apos_eq(3), apos_ec(3)  ! 太陽・月の視位置
    real(DP) :: rad_para(2)             ! 太陽・月の視半径・視差

    ! === 時刻 t2 のユリウス日
    call jd2gc(jd - JST_D, utc)  ! JD(JST) -> GC(UTC)
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
      & apos_eq(1:3), apos_ec(1:3), rad_para(1:2))

    kokei_sun  = apos_ec(1) * 180.0_DP / PI
  end subroutine comp_kokei

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
    integer :: i

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
end program jpl_sekki_24

