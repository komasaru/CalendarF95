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
  public :: gc2jd, jd2jc, utc2utc_tai, tt2ut1, deg2hms, date_fmt
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

  ! 日時の正常化
  !
  ! :param(inout) type(t_time) dt: 年, 月, 日, 時, 分, 秒, ミリ秒
  subroutine norm_time(dt)
    implicit none
    type(t_time), intent(inout) :: dt
    integer(SP) :: kbn = 0  ! 繰り上げ(1)か繰り下げ(-1)が
    integer(SP) :: days_m   ! 月内の日数

    ! ミリ秒（繰り上げ／繰り下げ）
    do while (dt%msecond > 999)
      kbn = 1
      dt%msecond = dt%msecond - 1000
      dt%second  = dt%second + 1
    end do
    do while (dt%msecond < 0)
      kbn = -1
      dt%msecond = dt%msecond + 1000
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

  ! TT(地球時) -> UT1(世界時1)
  !
  ! :param(in)  type(t_time)  tt
  ! :param(in)  real(8)       dt
  ! :param(out) type(t_time) ut1
  subroutine tt2ut1(tt, dt, ut1)
    implicit none
    type(t_time), intent(in)  :: tt
    real(DP),     intent(in)  :: dt
    type(t_time), intent(out) :: ut1
    integer(SP)  :: ye, mo, da, ho, mi, se, ms
    type(t_time) :: tmp

    ye = tt%year
    mo = tt%month
    da = tt%day
    ho = tt%hour
    mi = tt%minute
    se = tt%second
    ms = tt%msecond

    se = se - int(dt)
    ms = ms - nint((dt - int(dt)) * 1.0e3_DP)
    tmp = t_time(ye, mo, da, ho, mi, se, ms)
    call norm_time(tmp)
    ut1 = tmp
  end subroutine tt2ut1

  ! 99.999° -> 99h99m99s 変換
  !
  ! :param(in) real(8) deg: Degree
  ! :return  character(22): "-99 h 99 m 99.999999 s"
  character(22) function deg2hms(deg)
    implicit none
    real(DP), intent(in) :: deg
    character(1) :: sgn = " "
    integer(SP)  :: h, m
    real(DP)     :: s, mm, v

    h  = int(deg / 15.0_DP)
    mm = (deg - h * 15.0_DP) * 4.0_DP
    m  = int(mm)
    s = (mm - real(m, DP)) * 60.0_DP
    if (s < 0) then
      s   = -s
      sgn = "-"
    end if
    write (deg2hms, &
      & '(A1, I2, " h ", I0.2, " m ", I0.2, ".", I0.6, " s")') &
      & sgn, h, m, int(s), int((s - int(s)) * 1000000.0_DP)
  end function deg2hms

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

