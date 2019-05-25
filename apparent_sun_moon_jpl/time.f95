!*******************************************************************************
! Modules for time calculation
!
!   date          name            version
!   2018.10.25    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!   2018.11.10    mk-mode.com     1.02 テキストファイル OPEN/READ 時のエラー処理
!                                      を変更
!   2018.11.25    mk-mode.com     1.03 日時の正常化処理を削除し、日の加減算処理を
!                                      追加
!   2018.11.25    mk-mode.com     1.04 JD -> GC 変換アルゴリズムを変更
!   2019.05.25    mk-mode.com     1.05 JST -> UTC 変換時のうるう年処理を修正
!                                      JD -> GC 変換時の日時正規化（繰上・繰下）処理を修正
!
! Copyright(C) 2018-2019 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module time
  use const
  implicit none
  private
  public :: jst2utc, gc2jd, jd2jc, utc2utc_tai, utc2tai, &
          & tai2tt, tt2tcb, tcb2tdb, date_fmt
  type, public :: t_time
    integer(SP) :: year    = 0
    integer(SP) :: month   = 0
    integer(SP) :: day     = 0
    integer(SP) :: hour    = 0
    integer(SP) :: minute  = 0
    integer(SP) :: second  = 0
    integer(SP) :: msecond = 0
  end type t_time

contains
  ! =============================
  ! Private subroutines/functions
  ! =============================

  ! うるう年判定
  !
  ! :param(in) integer(4)   ye: 年
  ! :return    logical is_leap: うるう年(T), うるう年でない(F)
  logical function is_leap(ye)
    implicit none
    integer(SP), intent(in) :: ye

    if (mod(ye, 400) == 0) then
      is_leap = .true.
    else
      if (mod(ye, 4) == 0 .and. mod(ye, 100) /= 0) then
        is_leap = .true.
      else
        is_leap = .false.
      end if
    end if
  end function is_leap

  ! 日の加減算
  !
  ! :param(inout) type(t_time) gc: Gregorian Calendar
  ! :param(in)    real(8)       d: days
  subroutine add_day(gc, d)
    implicit none
    type(t_time), intent(inout) :: gc
    real(DP),     intent(in)    :: d
    real(DP) :: jd

    call gc2jd(gc, jd)
    call jd2gc(jd + d, gc)
  end subroutine add_day

  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! JST -> UTC
  !
  ! :param(in)  type(t_time) jst
  ! :param(out) type(t_time) utc
  subroutine jst2utc(jst, utc)
    implicit none
    type(t_time), intent(in)  :: jst
    type(t_time), intent(out) :: utc
    integer(SP) :: ye, mo, da, ho, mi, se, ms

    ye = jst%year
    mo = jst%month
    da = jst%day
    ho = jst%hour
    mi = jst%minute
    se = jst%second
    ms = jst%msecond

    ho = ho - JST_UTC
    if (ho < 0) then
      ho = ho + 24
      da = da - 1
      if (da < 1) then
        mo = mo - 1
        if (mo < 1) then
          mo = mo + 12
          ye = ye - 1
        end if
      end if
    end if
    if (da < 1) then
      da = da + DAYS(mo)
      !if (is_leap(ye)) then
      if (mo == 2 .and. is_leap(ye)) then
        da = da + 1
      end if
    end if
    utc = t_time(ye, mo, da, ho, mi, se, ms)
  end subroutine jst2utc

  ! GC(Gregoria Calendar) -> JD(Julian Day)
  !
  ! :param(in)  type(t_time) utc
  ! :param(out) real(8)       jd
  subroutine gc2jd(utc, jd)
    implicit none
    type(t_time), intent(in)  :: utc
    real(DP),     intent(out) :: jd
    integer  :: ye, mo, da, ho, mi, se, ms
    real(DP) :: d, t

    ye = utc%year
    mo = utc%month
    da = utc%day
    ho = utc%hour
    mi = utc%minute
    se = utc%second
    ms = utc%msecond

    if (mo < 3) then
      ye= ye - 1
      mo= mo + 12
    end if
    d = int(365.25_DP * ye)      &
    & + int(ye / 400.0_DP)       &
    & - int(ye / 100.0_DP)       &
    & + int(30.59_DP * (mo - 2)) &
    & + da + 1721088.5
    t = (ms / (3600.0_DP * 1.0e3_DP) &
    & + se / 3600.0_DP               &
    & + mi / 60.0_DP                 &
    & + ho) / 24.0_DP
    jd = d + t
  end subroutine gc2jd

  ! JD(Julian Day) -> GC(Gregoria Calendar)
  !
  ! :param(in)  real(8)      jd: Julian Day
  ! :param(out) type(t_time) gc: Gregoria Calendar
  subroutine jd2gc(jd, gc)
    implicit none
    real(DP),     intent(in)  :: jd
    type(t_time), intent(out) :: gc
    integer(SP) :: i, n, a, b, tm(0:6)
    real(DP)    :: tm_f, tm_w

    ! n, a, b 計算
    n = int(jd - 1721119.5_DP)  ! = -2400000.5 + 678881.0
    a = 4 * n + 3 + 4 * floor((3.0_DP / 4.0_DP) &
      & * (floor(4 * (n + 1) / 146097.0_DP) + 1))
    b = 5 * floor(mod(a, 1461) / 4.0_DP) + 2

    ! 年・月・日 計算
    tm(0) = floor(a / 1461.0_DP)
    tm(1) = floor(b / 153.0_DP)
    tm(2) = floor(mod(b, 153) / 5.0_DP)
    tm_w  = floor((tm(1) + 2) / 12.0_DP)
    tm(0) = tm(0) + tm_w
    tm(1) = tm(1) + 2 - tm_w * 12 + 1
    tm(2) = tm(2) + 1

    ! 時・分・秒・ミリ秒 計算
    tm_f = 86400.0_DP * (jd - .5_DP - int(jd - .5_DP))
    tm(3) = int(tm_f / 3600.0_DP)
    tm(4) = int((tm_f - 3600 * tm(3)) / 60.0_DP)
    tm_w = tm_f - 3600 * tm(3) - 60 * tm(4)
    tm(5) = int(tm_w)
    tm(6) = nint((tm_w - tm(5)) * 1.0e3_DP)

    ! 日時正規化
    call norm_tm(tm)
    gc = t_time(tm(0), tm(1), tm(2), tm(3), tm(4), tm(5), tm(6))
  end subroutine jd2gc

  ! JD(Julian Day) -> JC(Julian Century)
  !
  ! :param(in)  real(8) jd
  ! :param(out) real(8) jc
  subroutine jd2jc(jd, jc)
    implicit none
    real(DP), intent(in) :: jd
    real(DP) :: jc

    jc = (jd - J2000) / DAY_JC
  end subroutine jd2jc

  ! UTC(協定世界時) -> UTC(協定世界時) - TAI(国際原子時) (=うるう秒の総和)
  !
  ! :param(in)  type(t_time) utc
  ! :param(out) integer  utc_tai
  subroutine utc2utc_tai(utc, utc_tai)
    implicit none
    type(t_time), intent(in)  :: utc
    integer(SP),  intent(out) :: utc_tai
    character(8) :: date, utc_t
    integer(SP)  :: ios, val

    ! 対象の UTC 年月日
    write (utc_t, '(I4I0.2I0.2)') utc%year, utc%month, utc%day

    open (unit   = 10,          &
        & iostat = ios,         &
        & file   = F_LEAP_SEC,  &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_LEAP_SEC
      stop
    end if

    utc_tai = 0
    do
      read (10, *, iostat = ios) date, val
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_LEAP_SEC
      end if
      if (date == "") exit
      if (date > utc_t) exit
      utc_tai = val
    end do

    close(10)
  end subroutine utc2utc_tai

  ! UTC(協定世界時) -> TAI(国際原子時)
  !
  ! :param(in)  type(t_time) utc
  ! :param(in)  integer  utc_tai
  ! :param(out) type(t_time) tai
  subroutine utc2tai(utc, utc_tai, tai)
    implicit none
    type(t_time), intent(in)  :: utc
    integer(SP),  intent(in)  :: utc_tai
    type(t_time), intent(out) :: tai

    tai = utc
    call add_day(tai, real(-utc_tai, DP) / SEC_DAY)
  end subroutine utc2tai

  ! TAI(国際原子時) -> TT(地球時)
  !
  ! :param(in)  type(t_time) tai
  ! :param(out) type(t_time)  tt
  subroutine tai2tt(tai, tt)
    implicit none
    type(t_time), intent(in)  :: tai
    type(t_time), intent(out) :: tt

    tt = tai
    call add_day(tt, TT_TAI / SEC_DAY)
  end subroutine tai2tt

  ! TT(地球時) -> TCB(太陽系重心座標時)
  !
  ! :param(in)  type(t_time)  tt
  ! :param(in)  real(8)       jd
  ! :param(out) type(t_time) tcb
  subroutine tt2tcb(tt, jd, tcb)
    implicit none
    type(t_time), intent(in)  :: tt
    real(8),      intent(in)  :: jd
    type(t_time), intent(out) :: tcb
    real(DP)     :: s

    s = L_B * (jd - T_0) * SEC_DAY
    tcb = tt
    call add_day(tcb, s / SEC_DAY)
  end subroutine tt2tcb

  ! TCB(太陽系重心座標時) -> TDB(太陽系力学時)
  !
  ! :param(in)  type(t_time) tcb
  ! :param(in)  real(8)   jd_tcb
  ! :param(out) type(t_time) tdb
  subroutine tcb2tdb(tcb, jd_tcb, tdb)
    implicit none
    type(t_time), intent(in)  :: tcb
    real(8),      intent(in)  :: jd_tcb
    type(t_time), intent(out) :: tdb
    real(DP)     :: s

    s = L_B * (jd_tcb - T_0) * SEC_DAY - TDB_0
    tdb = tcb
    call add_day(tdb, -s / SEC_DAY)
  end subroutine tcb2tdb

  ! 日付文字列の整形
  ! * type(t_time)型 -> YYYY-MM-DD HH:MM:SS.UUUUUU
  !
  ! :param(in) type(t_time)  d
  ! :return    character(26) f
  function date_fmt(d) result(f)
    type(t_time), intent(in) :: d
    character(26) :: f

    write (f, FMT_DT_2) &
      & d%year, d%month, d%day, d%hour, d%minute, d%second, d%msecond
  end function date_fmt

  ! 日時正規化
  !
  ! :param(inout) integer(4) tm
  subroutine norm_tm(tm)
    implicit none
    integer(SP) :: tm(0:6), d

    ! (繰り上がり)
    do while (tm(6) > 999)
      tm(6) = tm(6) - 1000
      tm(5) = tm(5) + 1
    end do
    do while (tm(5) > 59)
      tm(5) = tm(5) - 60
      tm(4) = tm(4) + 1
    end do
    do while (tm(4) > 59)
      tm(4) = tm(4) - 60
      tm(3) = tm(3) + 1
    end do
    do while (tm(3) > 23)
      tm(3) = tm(3) - 24
      tm(2) = tm(2) + 1
    end do
    d = DAYS(tm(1))
    if (tm(1) == 2 .and. is_leap(tm(0))) d = d + 1
    do while (tm(2) > d)
      tm(2) = tm(2) - d
      tm(1) = tm(1) + 1
      d = DAYS(tm(1))
      if (tm(1) == 2 .and. is_leap(tm(0))) d = d + 1
    end do
    do while (tm(1) > 12)
      tm(1) = tm(1) - 12
      tm(0) = tm(0) + 1
    end do

    ! (繰り下がり)
    do while (tm(1) < 1)
      tm(0) = tm(0) - 1
      tm(1) = tm(1) + 12
    end do
    d = DAYS(tm(1))
    if (tm(1) == 2 .and. is_leap(tm(0))) d = d + 1
    do while (tm(2) < 1)
      tm(1) = tm(1) - 1
      tm(2) = tm(2) + d
      d = DAYS(tm(1))
      if (tm(1) == 2 .and. is_leap(tm(0))) d = d + 1
    end do
    do while (tm(3) < 0)
      tm(2) = tm(2) - 1
      tm(3) = tm(3) + 24
    end do
    do while (tm(4) < 0)
      tm(3) = tm(3) - 1
      tm(4) = tm(4) + 60
    end do
    do while (tm(5) < 0)
      tm(4) = tm(4) - 1
      tm(5) = tm(5) + 60
    end do
    do while (tm(6) < 0)
      tm(5) = tm(5) - 1
      tm(6) = tm(6) + 1000
    end do
  end subroutine
end module time

