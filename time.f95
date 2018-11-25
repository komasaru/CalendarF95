!*******************************************************************************
! Modules for time calculation
!
!   date          name            version
!   2018.10.27    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!   2018.11.10    mk-mode.com     1.02 テキストファイル OPEN/READ 時のエラー処理
!                                      を変更
!   2018.11.25    mk-mode.com     1.03 日時の正常化処理を削除し、日の加減算処理を
!                                      追加
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module time
  use const
  implicit none
  private
  public :: is_leap, is_valid_date, add_day, jst2utc, gc2jd, jd2gc, jd2jc, &
          & utc2utc_tai, utc2tai, tai2tt, tt2tcb, tcb2tdb, date_fmt
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

  ! 日付の整合性チェック
  !
  ! :param(in) type(t_time) gc: 日時
  ! :return    logical        : T: 正常, F: 異常
  logical function is_valid_date(gc)
    implicit none
    type(t_time), intent(in) :: gc

    is_valid_date = .true.
    if (gc%month == 2 .and. is_leap(gc%year)) then
      if (gc%day < 1 .or. gc%day > DAYS(gc%month) + 1) then
        is_valid_date = .false.
      end if
    else
      if (gc%day < 1 .or. gc%day > DAYS(gc%month)) then
        is_valid_date = .false.
      end if
    end if
    if (gc%hour    < 0 .or. gc%hour    >     23 .or. &
      & gc%minute  < 0 .or. gc%minute  >     59 .or. &
      & gc%second  < 0 .or. gc%second  >     59 .or. &
      & gc%msecond < 0 .or. gc%msecond > 999999) then
      is_valid_date = .false.
    end if
  end function is_valid_date

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
      if (is_leap(ye)) then
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
    integer(SP) :: i, x(0:6), ut(0:6)
    real(DP)    :: jd_w, tm, tm_w

    ut = (/(0, i=0,6)/)
    jd_w = jd - 0.5_DP
    x(0) = int(jd_w + 68570.0_DP)
    x(1) = int(x(0) / 36524.25_DP)
    x(2) = x(0) - int(36524.25_DP * x(1) + 0.75_DP)
    x(3) = int((x(2) + 1) / 365.2425_DP)
    x(4) = x(2) - int(365.25_DP * x(3)) + 31
    x(5) = int(int(x(4)) / 30.59_DP)
    x(6) = int(int(x(5)) / 11.0_DP)

    ! 年・月・日
    ut(2) = x(4) - int(30.59_DP * x(5))
    ut(1) = x(5) - 12 * x(6) + 2
    ut(0) = 100 * (x(1) - 49) + x(3) + x(6)
    ! 2月30日の補正
    if (ut(1) == 2 .and. ut(2) > 28) then
      if (mod(ut(0), 100) == 0 .and. mod(ut(0), 400) == 0) then
        ut(2) = 29
      else if (mod(ut(0), 4) == 0) then
        ut(2) = 29
      else
        ut(2) = 28
      end if
    end if

    ! 時・分・秒・ミリ秒
    tm = 86400.0_DP * (jd_w - int(jd_w))
    ut(3) = int(tm / 3600.0_DP)
    ut(4) = int((tm - 3600 * ut(3)) / 60.0_DP)
    ! ミリ秒対応
    !ut(5) = tm - 3600 * ut(3) - 60 * ut(4)
    tm_w = tm - 3600 * ut(3) - 60 * ut(4)
    ut(5) = int(tm_w)
    ut(6) = nint((tm_w - ut(5)) * 1.0e3_DP)
    if (ut(6) > 999) then
      ut(5) = ut(5) + 1
      ut(6) = ut(6) - 1000
    end if
    gc = t_time(ut(0), ut(1), ut(2), ut(3), ut(4), ut(5), ut(6))
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

    tdb = tcb
    call add_day(tdb, - s / SEC_DAY)
  end subroutine tcb2tdb

  ! 日付文字列の整形
  ! * type(t_time)型 -> YYYY-MM-DD HH:MM:SS.MMM
  !
  ! :param(in) type(t_time)  d
  ! :return    character(23) f
  function date_fmt(d) result(f)
    type(t_time), intent(in) :: d
    character(23) :: f

    write (f, FMT_DT_2) &
      & d%year, d%month, d%day, d%hour, d%minute, d%second, d%msecond
  end function date_fmt
end module time

