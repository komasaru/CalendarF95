!*******************************************************************************
! Modules for time calculation
!
!   date          name            version
!   2018.10.18    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module time
  use const
  implicit none
  private
  public :: gc2jd, jd2jc, date_fmt
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
    d =   int(365.25_DP * ye)      &
      & + int(ye / 400.0_DP)       &
      & - int(ye / 100.0_DP)       &
      & + int(30.59_DP * (mo - 2)) &
      & + da + 1721088.5
    t =  (ms / (3600.0_DP * 1.0e3_DP) &
      & + se / 3600.0_DP              &
      & + mi / 60.0_DP                &
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
end module time

