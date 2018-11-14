!*******************************************************************************
! Modules for time calculation
!
!   date          name            version
!   2018.11.11    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module time
  use const, only : SP, DP, JST_UTC, F_LEAP_SEC, F_DUT1, DAYS
  implicit none
  private
  public :: jst2utc, utc2utc_tai, utc2dut1
  type, public :: t_time
    integer(SP) :: year    = 0
    integer(SP) :: month   = 0
    integer(SP) :: day     = 0
    integer(SP) :: hour    = 0
    integer(SP) :: minute  = 0
    integer(SP) :: second  = 0
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
    integer(SP) :: ye, mo, da, ho, mi, se

    ye = jst%year
    mo = jst%month
    da = jst%day
    ho = jst%hour
    mi = jst%minute
    se = jst%second

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
    utc = t_time(ye, mo, da, ho, mi, se)
  end subroutine jst2utc

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
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_DUT1
      stop
    end if

    dut1 = 0.0
    do
      read (10, *, iostat = ios) date, val
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_DUT1
      end if
      if (date == "") exit
      if (date > utc_t) exit
      dut1 = val
    end do

    close(10)
  end subroutine utc2dut1
end module time

