!*******************************************************************************
! Modules for delta_t calculation
!
!   date          name            version
!   2018.11.11    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module delta_t
  use const
  use time, only : t_time
  implicit none
  private
  public :: utc2dt

contains
  ! =============================
  ! Private subroutines/functions
  ! =============================

  ! year < -500
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_m500(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y / 100.0_DP
    dt =  10583.6_DP           &
     & + (-1014.41_DP          &
     & + (   33.78311_DP       &
     & + (   -5.952053_DP      &
     & + (   -0.1798452_DP     &
     & + (    0.022174192_DP   &
     & + (    0.0090316521_DP) &
     & * t) * t) * t) * t) * t) * t
  end function before_m500

  ! -500 <= year and year < 500
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_500(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y / 100.0_DP
    dt =  10583.6_DP           &
     & + (-1014.41_DP          &
     & + (   33.78311_DP       &
     & + (   -5.952053_DP      &
     & + (   -0.1798452_DP     &
     & + (    0.022174192_DP   &
     & + (    0.0090316521_DP) &
     & * t) * t) * t) * t) * t) * t
  end function before_500

  ! 500 <= year and year < 1600
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1600(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = (y - 1000.0_DP) / 100.0_DP
    dt =  1574.2_DP           &
     & + (-556.01_DP          &
     & + (  71.23472_DP       &
     & + (   0.319781_DP      &
     & + (  -0.8503463_DP     &
     & + (  -0.005050998_DP   &
     & + (   0.0083572073_DP) &
     & * t) * t) * t) * t) * t) * t
  end function before_1600

  ! 1600 <= year and year < 1700
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1700(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1600.0_DP
    dt =  120.0_DP              &
     & + ( -0.9808_DP           &
     & + ( -0.01532_DP          &
     & + (  1.0_DP / 7129.0_DP) &
     & * t) * t) * t
  end function before_1700

  ! 1700 <= year and year < 1800
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1800(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1700.0_DP
    dt =   8.83_DP                &
     & + ( 1.1603_DP              &
     & + (-0.0059285_DP           &
     & + ( 0.00013336_DP          &
     & + (-1.0_DP / 1174000.0_DP) &
     & * t) * t) * t) * t
  end function before_1800

  ! 1800 <= year and year < 1860
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1860(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1800.0_DP
    dt =  13.72_DP            &
     & + (-0.332447_DP        &
     & + ( 0.0068612_DP       &
     & + ( 0.0041116_DP       &
     & + (-0.00037436_DP      &
     & + ( 0.0000121272_DP    &
     & + (-0.0000001699_DP    &
     & + ( 0.000000000875_DP) &
     & * t) * t) * t) * t) * t) * t) * t
  end function before_1860

  ! 1860 <= year and year < 1900
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1900(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1860.0_DP
    dt =   7.62_DP          &
     & + ( 0.5737_DP        &
     & + (-0.251754_DP      &
     & + ( 0.01680668_DP    &
     & + (-0.0004473624_DP  &
     & + ( 1.0_DP / 233174.0_DP) &
     & * t) * t) * t) * t) * t
  end function before_1900

  ! 1900 <= year and year < 1920
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1920(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1900.0_DP
    dt =  -2.79_DP      &
     & + ( 1.494119_DP  &
     & + (-0.0598939_DP &
     & + ( 0.0061966_DP &
     & + (-0.000197_DP) &
     & * t) * t) * t) * t
  end function before_1920

  ! 1920 <= year and year < 1941
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1941(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1920.0_DP
    dt =  21.20_DP       &
     & + ( 0.84493_DP    &
     & + (-0.076100_DP   &
     & + ( 0.0020936_DP) &
     & * t) * t) * t
  end function before_1941

  ! 1941 <= year and year < 1961
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function before_1961(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = y - 1950.0_DP
    dt =  29.07_DP             &
     & + ( 0.407_DP            &
     & + (-1.0_DP / 233.0_DP   &
     & + ( 1.0_DP / 2547.0_DP) &
     & * t) * t) * t
  end function before_1961

  ! 1961 <= year and year < 1986
  !
  ! :param(in) real(8)       y
  ! :param(in) integer    year
  ! :param(in) integer   month
  ! :param(in) integer utc_tai
  ! :param(in) real(8)    dut1
  ! :return    real(8)      dt
  function before_1986(y, year, month, utc_tai, dut1) result(dt)
    implicit none
    real(DP),    intent(in) :: y
    integer(SP), intent(in) ::year, month, utc_tai
    real(DP),    intent(in) :: dut1
    real(DP)     :: dt
    character(8) :: utc
    real(DP)     :: t

    write (utc, '(I4I2.2, "01")') year, month
    t = y - 1975.0_DP
    if (utc < "19720101") then
      dt =  45.45_DP            &
       & + ( 1.067_DP           &
       & + (-1.0_DP / 260.0_DP  &
       & + (-1.0_DP / 718.0_DP) &
       & * t) * t) * t
    else
      dt = TT_TAI - utc_tai - dut1
    end if
  end function before_1986

  ! 1986 <= year and year < 2005
  !
  ! :param(in) real(8)       y
  ! :param(in) integer    year
  ! :param(in) integer   month
  ! :param(in) integer utc_tai
  ! :param(in) real(8)    dut1
  ! :return    real(8)      dt
  function before_2005(y, year, month, utc_tai, dut1) result(dt)
    implicit none
    real(DP),    intent(in) :: y
    integer(SP), intent(in) :: year, month, utc_tai
    real(DP),    intent(in) :: dut1
    real(DP)     :: dt
    character(8) :: utc
    real(DP)     :: t

    write (utc, '(I4I2, "01")') year, month
    t = y - 2000.0_DP
    dt = TT_TAI - utc_tai - dut1
  end function before_2005

  ! 2005 <= year and year < 2050
  !
  ! :param(in) real(8)       y
  ! :param(in) integer    year
  ! :param(in) integer   month
  ! :param(in) integer utc_tai
  ! :param(in) real(8)    dut1
  ! :return    real(8)      dt
  function before_2050(y, year, month, utc_tai, dut1) result(dt)
    implicit none
    real(DP),    intent(in) :: y
    integer(SP), intent(in) :: year, month, utc_tai
    real(DP),    intent(in) :: dut1
    real(DP)     :: dt
    character(8) :: utc
    real(DP)     :: t

    write (utc, '(I4I2, "01")') year, month
    t = y - 2000.0_DP
    ! うるう秒の暫定エンドポイント日付以降なら、
    ! NASA 提供の略算式
    if (utc_tai == 0) then
      dt =  62.92_DP      &
       & + ( 0.32217_DP   &
       & + ( 0.005589_DP) &
       & * t) * t
    else
      dt = TT_TAI - utc_tai - dut1
    end if
  end function before_2050

  ! 2050 <= year and year <= 2150
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function until_2150(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    dt = -20.0_DP &
     & + 32.0_DP * ((y - 1820.0_DP) / 100.0_DP) ** 2 &
     & - 0.5628_DP * (2150.0_DP - y)
  end function until_2150

  ! 2150 < year
  !
  ! :param(in) real(8)  y
  ! :return    real(8) dt
  function after_2150(y) result(dt)
    implicit none
    real(DP), intent(in) :: y
    real(DP) :: dt
    real(DP) :: t

    t = (y - 1820.0_DP) / 100.0_DP
    dt = -20 + 32 * t**2
  end function after_2150

  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! delta T calculation
  !
  ! :param(in)  type(t_time) utc
  ! :param(in)  integer  utc_tai
  ! :param(in)  real(8)     dut1
  ! :param(out) real(8)       dt
  subroutine utc2dt(utc, utc_tai, dut1, dt)
    implicit none
    type(t_time), intent(in)  :: utc
    integer(SP),  intent(in)  :: utc_tai
    real(DP),     intent(in)  :: dut1
    real(DP),     intent(out) :: dt
    integer(SP) :: year, month, day, hour, minute, second, msecond
    real(DP)    :: y

    y = utc%year + (utc%month - 0.5) / 12.0
    dt = 0.0_DP
    select case (utc%year)
    case (:-501)
      dt = before_m500(y)
    case (-500:499)
      dt = before_500(y)
    case (500:1599)
      dt = before_1600(y)
    case (1600:1699)
      dt = before_1700(y)
    case (1700:1799)
      dt = before_1800(y)
    case (1800:1859)
      dt = before_1860(y)
    case (1860:1899)
      dt = before_1900(y)
    case (1900:1919)
      dt = before_1920(y)
    case (1920:1940)
      dt = before_1941(y)
    case (1941:1960)
      dt = before_1961(y)
    case (1961:1985)
      dt = before_1986(y, utc%year, utc%month, utc_tai, dut1)
    case (1986:2004)
      dt = before_2005(y, utc%year, utc%month, utc_tai, dut1)
    case (2005:2049)
      dt = before_2050(y, utc%year, utc%month, utc_tai, dut1)
    case (2050:2149)
      dt = until_2150(y)
    case (2150:)
      dt = after_2150(y)
    end select
  end subroutine utc2dt
end module delta_t

