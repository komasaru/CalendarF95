!****************************************************
! Modules for time calculation
!
! date          name            version
! 2018.10.14    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module time
  use const
  implicit none
  private
  public :: jst2utc, gc2jd, jd2jc, utc2utc_tai, utc2dut1, utc2tai, &
          & utc2ut1, tai2tt, tt2tcg, tt2tcb, tcb2tdb, date_fmt
  type, public :: t_time
    integer(SP) :: year    = 0
    integer(SP) :: month   = 0
    integer(SP) :: day     = 0
    integer(SP) :: hour    = 0
    integer(SP) :: minute  = 0
    integer(SP) :: second  = 0
    integer(SP) :: usecond = 0
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

  ! 日時の正常化
  !
  ! :param(inout) type(t_time) dt: 年, 月, 日, 時, 分, 秒, μ秒
  subroutine norm_time(dt)
    implicit none
    type(t_time), intent(inout) :: dt
    integer(SP) :: kbn = 0  ! 繰り上げ(1)か繰り下げ(-1)が
    integer(SP) :: days_m   ! 月内の日数

    ! マイクロ秒（繰り上げ／繰り下げ）
    do while (dt%usecond > 999)
      kbn = 1
      dt%usecond = dt%usecond - 1000000
      dt%second  = dt%second + 1
    end do
    do while (dt%usecond < 0)
      kbn = -1
      dt%usecond = dt%usecond + 1000000
      dt%second  = dt%second - 1
    end do
    ! 秒（繰り上げ／繰り下げ）
    do while (dt%second > 59)
      kbn = 1
      dt%second = dt%second - 60
      dt%minute = dt%minute + 1
    end do
    do while (dt%second < 0)
      kbn = -1
      dt%second = dt%second + 60
      dt%minute = dt%minute - 1
    end do
    ! 分（繰り上げ／繰り下げ）
    do while (dt%minute > 59)
      kbn = 1
      dt%minute = dt%minute - 60
      dt%hour   = dt%hour + 1
    end do
    do while (dt%minute < 0)
      kbn = -1
      dt%minute = dt%minute + 60
      dt%hour   = dt%hour - 1
    end do
    ! 時（繰り上げ／繰り下げ）
    do while (dt%hour > 23)
      kbn = 1
      dt%hour = dt%hour - 24
      dt%day  = dt%day + 1
    end do
    do while (dt%hour< 0)
      kbn = -1
      dt%hour = dt%hour + 24
      dt%day  = dt%day - 1
    end do
    ! 月内の日数
    if (is_leap(dt%year)) then
      if ((kbn ==  1 .and. dt%month == 2) .or. &
          (kbn == -1 .and. dt%month == 3)) then
        days_m = DAYS(2) + 1
      else
        days_m = DAYS(dt%month)
      end if
    else
      days_m = DAYS(dt%month)
    end if
    ! 日（繰り上げ／繰り下げ）
    do while (dt%day > days_m)
      dt%day   = dt%day - days_m
      dt%month = dt%month + 1
    end do
    do while (dt%day < 0)
      dt%day   = dt%day + days_m
      dt%month = dt%month - 1
    end do
    ! 月（繰り上げ／繰り下げ）
    do while (dt%month> 12)
      dt%month = dt%month - 12
      dt%year  = dt%year + 1
    end do
    do while (dt%month< 0)
      dt%month = dt%month + 12
      dt%year  = dt%year- 1
    end do
  end subroutine norm_time

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
    integer(SP) :: ye, mo, da, ho, mi, se, us

    ye = jst%year
    mo = jst%month
    da = jst%day
    ho = jst%hour
    mi = jst%minute
    se = jst%second
    us = jst%usecond

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
      if (is_leap(ye)) then
        da = da + 1
      end if
    end if
    utc = t_time(ye, mo, da, ho, mi, se, us)
  end subroutine jst2utc

  ! GC(Gregoria Calendar) -> JD(Julian Day)
  !
  ! :param(in)  type(t_time) utc
  ! :param(out) real(8)       jd
  subroutine gc2jd(utc, jd)
    implicit none
    type(t_time), intent(in)  :: utc
    real(DP),     intent(out) :: jd
    integer  :: ye, mo, da, ho, mi, se, us
    real(DP) :: d, t

    ye = utc%year
    mo = utc%month
    da = utc%day
    ho = utc%hour
    mi = utc%minute
    se = utc%second
    us = utc%usecond

    if (mo < 3) then
      ye= ye - 1
      mo= mo + 12
    end if
    d =   int(365.25_DP * ye)      &
      & + int(ye / 400.0_DP)       &
      & - int(ye / 100.0_DP)       &
      & + int(30.59_DP * (mo - 2)) &
      & + da + 1721088.5
    t =  (us / (3600.0_DP * 1000000.0_DP) &
      & + se / 3600.0_DP                  &
      & + mi / 60.0_DP                    &
      & + ho) / 24.0_DP
    jd = d + t
  end subroutine gc2jd

  ! JD(Julian Day) -> JC(Julian Century)
  !
  ! :param(in)  real(8) jd
  ! :param(out) real(8) jc
  subroutine jd2jc(jd, jc)
    implicit none
    real(DP), intent(in)  :: jd
    real(DP), intent(out) :: jc

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
      print *, "[ERROR] Failed to open file: " // F_LEAP_SEC
      stop
    end if

    utc_tai = 0
    do
      read (10, *, iostat = ios) date, val
      if (ios /= 0) exit
      if (date == "") exit
      if (date > utc_t) exit
      utc_tai = val
    end do

    close(10)
  end subroutine utc2utc_tai

  ! UTC(協定世界時) -> DUT1(UT1(世界時1) - UTC(協定世界時))
  !
  ! :param(in)  type(t_time) utc
  ! :param(out) real(8)     dut1
  subroutine utc2dut1(utc, dut1)
    implicit none
    type(t_time), intent(in)  :: utc
    real(DP),     intent(out) :: dut1
    character(8) :: date, utc_t
    integer(SP)  :: ios
    real(DP)     :: val

    ! 対象の UTC 年月日
    write (utc_t, '(I4I0.2I0.2)') utc%year, utc%month, utc%day

    open (unit   = 10,          &
        & iostat = ios,         &
        & file   = F_DUT1,      &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_DUT1
      stop
    end if

    dut1 = 0.0
    do
      read (10, *, iostat = ios) date, val
      if (ios /= 0) exit
      if (date == "") exit
      if (date > utc_t) exit
      dut1 = val
    end do

    close(10)
  end subroutine utc2dut1

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
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    integer(SP)  :: days_m
    type(t_time) :: tmp

    ye = utc%year
    mo = utc%month
    da = utc%day
    ho = utc%hour
    mi = utc%minute
    se = utc%second
    us = utc%usecond

    se = se - utc_tai
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    tai = tmp
  end subroutine utc2tai

  ! UTC(協定世界時) -> UT1(世界時1)
  !
  ! :param(in)  type(t_time) utc
  ! :param(in)  real(8)     dut1
  ! :param(out) type(t_time) ut1
  subroutine utc2ut1(utc, dut1, ut1)
    implicit none
    type(t_time), intent(in)  :: utc
    real(DP),     intent(in)  :: dut1
    type(t_time), intent(out) :: ut1
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    type(t_time) :: tmp

    ye = utc%year
    mo = utc%month
    da = utc%day
    ho = utc%hour
    mi = utc%minute
    se = utc%second
    us = utc%usecond

    se = se + int(dut1)
    us = us + nint((dut1 - int(dut1)) * 1000000.0_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    ut1 = tmp
  end subroutine utc2ut1

  ! TAI(国際原子時) -> TT(地球時)
  !
  ! :param(in)  type(t_time) tai
  ! :param(out) type(t_time)  tt
  subroutine tai2tt(tai, tt)
    implicit none
    type(t_time), intent(in)  :: tai
    type(t_time), intent(out) :: tt
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    type(t_time) :: tmp

    ye = tai%year
    mo = tai%month
    da = tai%day
    ho = tai%hour
    mi = tai%minute
    se = tai%second
    us = tai%usecond

    se = se + int(TT_TAI)
    us = us + nint((TT_TAI - int(TT_TAI)) * 1000000.0_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    tt = tmp
  end subroutine tai2tt

  ! TT(地球時) -> TCG(地球重心座標時)
  !
  ! :param(in)  type(t_time)  tt
  ! :param(in)  real(8)       jd
  ! :param(out) type(t_time) tcg
  subroutine tt2tcg(tt, jd, tcg)
    implicit none
    type(t_time), intent(in)  :: tt
    real(DP),     intent(in)  :: jd
    type(t_time), intent(out) :: tcg
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    real(DP)     :: s
    type(t_time) :: tmp

    ye = tt%year
    mo = tt%month
    da = tt%day
    ho = tt%hour
    mi = tt%minute
    se = tt%second
    us = tt%usecond

    s = L_G * (jd - T_0) * SEC_DAY
    se  = se + int(s)
    us = us + nint((s - int(s)) * 1000000.0_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    tcg = tmp
  end subroutine tt2tcg

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
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    real(DP)     :: s
    type(t_time) :: tmp

    ye = tt%year
    mo = tt%month
    da = tt%day
    ho = tt%hour
    mi = tt%minute
    se = tt%second
    us = tt%usecond

    s = L_B * (jd - T_0) * SEC_DAY
    se = se + int(s)
    us = us + nint((s - int(s)) * 1000000.0_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    tcb = tmp
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
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    real(DP)     :: s
    type(t_time) :: tmp

    ye = tcb%year
    mo = tcb%month
    da = tcb%day
    ho = tcb%hour
    mi = tcb%minute
    se = tcb%second
    us = tcb%usecond

    s = L_B * (jd_tcb - T_0) * SEC_DAY - TDB_0
    se = se - int(s)
    us = us - nint((s - int(s)) * 1000000.0_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, us)
    call norm_time(tmp)
    tdb = tmp
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
      & d%year, d%month, d%day, d%hour, d%minute, d%second, d%usecond
  end function date_fmt
end module time

