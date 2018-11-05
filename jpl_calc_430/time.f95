!****************************************************
! Modules for time calculation
!
! date          name            version
! 2018.10.21    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module time
  use const
  implicit none
  private
  public :: gc2jd
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
  ! ============================
  ! Public subroutines/functions
  ! ============================

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
end module time

