!*******************************************************************************
! 朔望一覧(CSV 出力)
! * 計算対象：「Y_MIN - 1 年〜 Y_MAX + 1 年」
!
!   Date          Author          Version
!   2018.10.29    mk-mode.com     1.00 新規作成
!   2018.11.10    mk-mode.com     1.01 テキストファイル OPEN/READ 時のエラー処理
!                                      を変更
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time    モジュール内で定義
! * 構造型 type(t_bin)  は eph_jpl モジュール内で定義
!*******************************************************************************
!
program jpl_moon
  use const, only : SP, DP, Y_MIN, Y_MAX, JST_D, PI
  use apos
  use eph_jpl
  use time
  implicit none
  character(*), parameter :: F_CSV      = "csv/moon.csv"
  integer(SP),  parameter :: UID_CSV    = 11  ! 10 は JPLEPH で使用
  integer(SP),  parameter :: LOOP_LIMIT = 50
  real(DP),     parameter :: EPS        = 1.0e-8_DP
  type(t_time) :: tm
  type(t_time) :: tm_l, tm_h, tm_x
  integer(SP)  :: ios
  real(DP)     :: jd
  real(DP)     :: kokei_s, kokei_m, kokei_d, kokei_t  ! Sun, Moon, diff, target

  ! 書き込み用 CSV ファイル OPEN
  open (unit   = UID_CSV,     &
      & iostat = ios,         &
      & file   = F_CSV,       &
      & action = "write",     &
      & form   = "formatted", &
      & status = "new")
  if (ios /= 0) then
    print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV
    stop
  end if

  ! 計算
  tm_l = t_time(Y_MIN - 1, 1, 1, 0, 0, 0, 0)
  tm_h = t_time(Y_MIN - 1, 1, 1, 0, 0, 0, 0)
  tm_h%day = tm_h%day + 10
  call norm_time(tm_h)
  call gc2jd(tm_l, jd)
  call comp_kokei(jd - JST_D, kokei_s, kokei_m)
  kokei_d = kokei_m - kokei_s
  if (kokei_d < 0.0_DP) kokei_d = kokei_d + 360.0_DP
  kokei_t = real(int(kokei_d / 90.0_DP), DP) * 90.0_DP + 90.0_DP
  if (int(kokei_t) == 360) kokei_t = 0.0_DP
  do
    call bisection(tm_l, tm_h, kokei_t, tm_x)
    if (tm_x%year > Y_MAX + 1) exit
    print '("* ", I4, ":", I3, " - ", &
        & I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)', &
        & tm_x%year, int(kokei_t), &
        & tm_x%year, tm_x%month, tm_x%day, &
        & tm_x%hour, tm_x%minute, tm_x%second
    write (UID_CSV, &
        & '(I4, ",", I2, ",", I2 ",", I3, ",", &
        & I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)') &
        & tm_x%year, tm_x%month, tm_x%day, int(kokei_t), &
        & tm_x%year, tm_x%month, tm_x%day, &
        & tm_x%hour, tm_x%minute, tm_x%second
    tm_l = tm_x
    tm_h = tm_l
    tm_h%day = tm_h%day + 10
    call norm_time(tm_h)
    kokei_t = kokei_t + 90.0_DP
    if (int(kokei_t) == 360) kokei_t = 0.0_DP
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  stop
contains
  ! Bisection method
  !
  ! :param(in)  type(t_time) tm_l
  ! :param(in)  type(t_time) tm_h
  ! :param(in)  real(8)   kokei_t
  ! :param(out) type(t_time) tm_x
  subroutine bisection(tm_l, tm_h, kokei_t, tm_x)
    implicit none
    type(t_time), intent(in)  :: tm_l, tm_h
    real(DP),     intent(in)  :: kokei_t
    type(t_time), intent(out) :: tm_x
    integer(SP)   :: i
    real(DP)      :: jd_l, jd_h, jd_x, kokei_s_x, kokei_m_x, kokei_d

    call gc2jd(tm_l, jd_l)
    call gc2jd(tm_h, jd_h)
    do i = 1, LOOP_LIMIT
      jd_x = (jd_l + jd_h) / 2.0_DP
      call comp_kokei(jd_x, kokei_s_x, kokei_m_x)
      kokei_d = kokei_m_x - kokei_s_x
      if (kokei_d < 0.0_DP) kokei_d = kokei_d + 360.0
      if (kokei_d > kokei_t) then
        if (kokei_d - kokei_t < 90.0_DP) then
          jd_h = jd_x
        else
          jd_l = jd_x
        end if
      else
        if (kokei_d - kokei_t < 90.0_DP) then
          jd_l = jd_x
        else
          jd_h = jd_x
        end if
      end if
      if (kokei_d == kokei_t) exit
      if (jd_h - jd_l < EPS) exit
    end do
    call jd2gc(jd_x, tm_x)
  end subroutine bisection

  ! 太陽・月の視黄経計算
  !
  ! :param real(8)         jd: Julian Day
  ! :param real(8)  kokei_sun: 視黄経（太陽）
  ! :param real(8) kokei_moon: 視黄経（月）
  subroutine comp_kokei(jd, kokei_sun, kokei_moon)
    implicit none
    real(DP), intent(in)  :: jd
    real(DP), intent(out) :: kokei_sun, kokei_moon
    type(t_time) :: utc, tdb
    real(DP)     :: jd_tdb
    type(t_bin)  :: bin
    integer(SP)  :: i
    real(DP)     :: jds(2)
    ! 時刻 t1(TDB), t2(TDB) における位置・速度(ICRS座標)用配列
    real(DP) :: icrs_1(6, 3), icrs_2(6, 3)
    real(DP) :: d_e_m, d_e_s                  ! 地球と月・太陽の距離
    real(DP) :: r_s, r_m, r_e                 ! 太陽・月・地球の半径
    real(DP) :: apos_eq(3, 2), apos_ec(3, 2)  ! 太陽・月の視位置
    real(DP) :: rad_para(2, 2)                ! 太陽・月の視半径・視差

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
      & apos_eq(1:3, 1), apos_ec(1:3, 1), rad_para(1:2, 1))
    call calc_moon(jd_tdb, icrs_2, d_e_m, r_e, r_m, bin%au, &
      & apos_eq(1:3, 2), apos_ec(1:3, 2), rad_para(1:2, 2))

    kokei_sun  = apos_ec(1, 1) * 180.0_DP / PI
    kokei_moon = apos_ec(1, 2) * 180.0_DP / PI
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
end program jpl_moon

