!****************************************************
! Modules for fundamental arguments
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module fundamental_argument
  use const
  implicit none
  private
  public :: l_iers2003, p_iers2003, f_iers2003, d_iers2003, om_iers2003, &
          & ve_iers2003, ea_iers2003, pa_iers2003, me_iers2003, ma_iers2003, &
          & ju_iers2003, sa_iers2003, ur_iers2003, lp_mhb2000, d_mhb2000, &
          & l_mhb2000, f_mhb2000, d_mhb2000_2, om_mhb2000, ne_mhb2000

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Mean anomaly of the Moon (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean anomaly of the Moon
  real(DP) function l_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    l_iers2003 = mod( &
      &   (    485868.249036_DP    &
      & + (1717915923.2178_DP      &
      & + (        31.8792_DP      &
      & + (         0.051635_DP    &
      & + (        -0.00024470_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (l_iers2003 < 0.0_DP) l_iers2003 = l_iers2003 + TURNAS
    l_iers2003 = l_iers2003 * AS2R
  end function l_iers2003

  ! Mean anomaly of the Sun (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean anomaly of the Sun
  real(DP) function p_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    p_iers2003 = mod( &
      &   (   1287104.793048_DP    &
      & + ( 129596581.0481_DP      &
      & + (        -0.5532_DP      &
      & + (         0.000136_DP    &
      & + (        -0.00001149_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (p_iers2003 < 0.0_DP) p_iers2003 = p_iers2003 + TURNAS
    p_iers2003 = p_iers2003 * AS2R
  end function p_iers2003

  ! Mean longitude of the Moon minus that of the ascending node (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean longitude of the Moon minus that of the ascending node
  real(DP) function f_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    f_iers2003 = mod( &
      &   (     335779.526232_DP   &
      & + (1739527262.8478_DP      &
      & + (       -12.7512_DP      &
      & + (        -0.001037_DP    &
      & + (         0.00000417_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (f_iers2003 < 0.0_DP) f_iers2003 = f_iers2003 + TURNAS
    f_iers2003 = f_iers2003 * AS2R
  end function f_iers2003

  ! Mean elongation of the Moon from the Sun (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean elongation of the Moon from the Sun
  real(DP) function d_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    d_iers2003 = mod( &
      &   (   1072260.703692_DP    &
      & + (1602961601.2090_DP      &
      & + (       - 6.3706_DP      &
      & + (         0.006593_DP    &
      & + (       - 0.00003169_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (d_iers2003 < 0.0_DP) d_iers2003 = d_iers2003 + TURNAS
    d_iers2003 = d_iers2003 * AS2R
  end function d_iers2003

  ! Mean longitude of the ascending node of the Moon
  ! (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean longitude of the ascending node of the
  !                  Moon
  real(DP) function om_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    om_iers2003 = mod( &
      &   (    450160.398036_DP    &
      & + (  -6962890.5431_DP      &
      & + (         7.4722_DP      &
      & + (         0.007702_DP    &
      & + (        -0.00005939_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (om_iers2003 < 0.0_DP) om_iers2003 = om_iers2003 + TURNAS
    om_iers2003 = om_iers2003 * AS2R
  end function om_iers2003

  ! Venus longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Venus longitudes
  real(DP) function ve_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    ve_iers2003 = mod((3.176146697_DP + 1021.3285546211_DP * t), PI2)
  end function ve_iers2003

  ! Earth longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Earth longitudes
  real(DP) function ea_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    ea_iers2003 = mod((1.753470314_DP + 628.3075849991_DP * t), PI2)
  end function ea_iers2003

  ! General accumulated precession in longitude (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : General accumulated precession in longitude
  real(DP) function pa_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    pa_iers2003 = (0.024381750_DP + 0.00000538691_DP * t) * t
  end function pa_iers2003

  ! Mercury longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mercury longitudes
  real(DP) function me_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    me_iers2003 = mod((4.402608842_DP + 2608.7903141574_DP * t), PI2)
  end function me_iers2003

  ! Mars longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mars longitudes
  real(DP) function ma_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    ma_iers2003 = mod((6.203480913_DP + 334.0612426700_DP * t), PI2)
  end function ma_iers2003

  ! Jupiter longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Jupiter longitudes
  real(DP) function ju_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    ju_iers2003 = mod((0.599546497_DP + 52.9690962641_DP * t), PI2)
  end function ju_iers2003

  ! Saturn longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Saturn longitudes
  real(DP) function sa_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    sa_iers2003 = mod((0.874016757_DP + 21.3299104960_DP * t), PI2)
  end function sa_iers2003

  ! Uranus longitudes (IERS 2003)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Uranus longitudes
  real(DP) function ur_iers2003(t)
    implicit none
    real(DP), intent(in) :: t

    ur_iers2003 = mod((5.481293872_DP + 7.4781598567_DP * t), PI2)
  end function ur_iers2003

  ! Mean anomaly of the Sun (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean anomaly of the Sun
  real(DP) function lp_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    lp_mhb2000 = mod( &
      &   (  1287104.79305_DP     &
      & + (129596581.0481_DP      &
      & + (       -0.5532_DP      &
      & + (        0.000136_DP    &
      & + (       -0.00001149_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (lp_mhb2000 < 0.0_DP) lp_mhb2000 = lp_mhb2000 + TURNAS
    lp_mhb2000 = lp_mhb2000 * AS2R
  end function lp_mhb2000

  ! Mean elongation of the Moon from the Sun (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean elongation of the Moon from the Sun
  real(DP) function d_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    d_mhb2000 = mod( &
      &   (   1072260.70369_DP     &
      & + (1602961601.2090_DP      &
      & + (        -6.3706_DP      &
      & + (         0.006593_DP    &
      & + (        -0.00003169_DP) &
      & * t) * t) * t) * t), TURNAS)
    if (d_mhb2000 < 0.0_DP) d_mhb2000 = d_mhb2000 + TURNAS
    d_mhb2000 = d_mhb2000 * AS2R
  end function d_mhb2000

  ! Mean anomaly of the Moon (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean anomaly of the Moon
  real(DP) function l_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    l_mhb2000 = mod((2.35555598_DP + 8328.6914269554_DP * t), PI2)
  end function l_mhb2000

  ! Mean longitude of the Moon minus that of the ascending node
  ! (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean longitude of the Moon minus that of the
  !                  ascending node
  real(DP) function f_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    f_mhb2000 = mod((1.627905234_DP + 8433.466158131_DP * t), PI2)
  end function f_mhb2000

  ! Mean elongation of the Moon from the Sun (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean elongation of the Moon from the Sun
  real(DP) function d_mhb2000_2(t)
    implicit none
    real(DP), intent(in) :: t

    d_mhb2000_2 = mod((5.198466741_DP + 7771.3771468121_DP * t), PI2)
  end function d_mhb2000_2

  ! Mean longitude of the ascending node of the Moon (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Mean longitude of the ascending node of the
  !                  Moon
  real(DP) function om_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    om_mhb2000 = mod((2.18243920_DP - 33.757045_DP * t), PI2)
    if (om_mhb2000 < 0.0_DP) om_mhb2000 = om_mhb2000 + PI2
  end function om_mhb2000

  ! Neptune longitude (MHB2000)
  !
  ! :param(in) real(8) t: ユリウス世紀数
  ! :return    real(8)  : Neptune longitude
  real(DP) function ne_mhb2000(t)
    implicit none
    real(DP), intent(in) :: t

    ne_mhb2000 = mod((5.321159000_DP + 3.8127774000_DP * t), PI2)
  end function ne_mhb2000
end module fundamental_argument

