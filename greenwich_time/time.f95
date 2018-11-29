!*******************************************************************************
! Modules for time calculation
!
!   date          name            version
!   2018.10.18    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!   2018.11.10    mk-mode.com     1.02 テキストファイル OPEN/READ 時のエラー処理
!                                      を変更
!   2018.11.11    mk-mode.com     1.03 UTC -> DUT1 計算を追加
!   2018.11.25    mk-mode.com     1.04 日時の正常化処理を削除し、日の加減算処理を
!                                      追加
!   2018.11.25    mk-mode.com     1.05 JD -> GC 変換アルゴリズムを変更
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module time
  use const
  implicit none
  private
  public :: gc2jd, jd2jc, utc2utc_tai, utc2dut1, tt2ut1, deg2hms, date_fmt
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
    d = int(365.25_DP * ye)      &
    & + int(ye / 400.0_DP)       &
    & - int(ye / 100.0_DP)       &
    & + int(30.59_DP * (mo - 2)) &
    & + da + 1721088.5_DP
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
    ! ミリ秒四捨五入で 1000 になった場合
    if (tm(6) > 999) then
      tm(5) = tm(5) + 1
      tm(6) = tm(6) - 1000
      if (tm(5) > 59) then
        tm(4) = tm(4) + 1
        tm(5) = tm(5) - 60
        if (tm(4) > 59) then
          tm(3) = tm(3) + 1
          tm(4) = tm(4) - 60
        end if
      end if
    end if
    gc = t_time(tm(0), tm(1), tm(2), tm(3), tm(4), tm(5), tm(6))
  end subroutine jd2gc

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

    ut1 = tt
    call add_day(ut1, -dt / SEC_DAY)
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

