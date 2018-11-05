!****************************************************
! Modules for greenwich time calculation
! * ERA(Earth rotation angle (IAU 2000 model), 地球回転角),
! * EORS(Equation of the origins, 原点差)
! * GMST(Greenwich mean sidereal time, グリニッジ平均恒星時)
! * GAST(Greenwich apparent sidereal time, グリニッジ視恒星時)
! * EE(Equation of Equinoxes, 分点均差)
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module greenwich
  use const
  use angle
  implicit none
  private
  public :: gw_era, gw_eors, gw_gast, gw_gmst, gw_ee

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Earth rotation angle (IAU 2000 model).
  !
  ! :param(in)  real(8)  jd: Jurian Day
  ! :param(out) real(8) era: Earth Rotation Angle (Unit: rad, Range: 0-2pi)
  !                             (地球回転角)
  subroutine gw_era(jd, era)
    implicit none
    real(DP), intent(in)  :: jd
    real(DP), intent(out) :: era
    real(DP) :: f

    ! Fractional part of T (days).
    f = mod(jd, 1.0_DP)
    if (f < 0.0_DP) f = f + 1.0_DP
    ! Earth rotation angle at this UT1.
    era = (f + 0.7790572732640_DP + &
        & 0.00273781191135448_DP * (jd - J2000)) * PI2
    call norm_angle(era)
  end subroutine gw_era

  ! Equation of the origins, given the classical NPB matrix and the
  ! quantity s.
  !
  ! :param(in)  real(8) mtx(3, 3): Rotation matrix
  ! :param(in)  real(8)         s: CIO locator
  ! :param(out) real(8)        eo: Equation of the origin (Unit: rad)
  !                                (原点差)
  subroutine gw_eors(mtx, s, eo)
    implicit none
    real(DP), intent(in)  :: mtx(3, 3), s
    real(DP), intent(out) :: eo
    real(DP) :: x, ax, xs, ys, zs, p, q

    x  = mtx(3, 1)
    ax = x / (1.0_DP + mtx(3, 3))
    xs = 1.0_DP - ax * x
    ys = -ax * mtx(3, 2)
    zs = -x
    p  = mtx(1, 1) * xs + mtx(1, 2) * ys + mtx(1, 3) * zs
    q  = mtx(2, 1) * xs + mtx(2, 2) * ys + mtx(2, 3) * zs
    if (p == 0.0_DP .and. q == 0.0_DP) then
      eo = s
    else
      eo = s - atan2(q, p)
    end if
  end subroutine gw_eors

  ! Greenwich apparent sidereal time
  !
  ! :param(in)  real(8)  era: Earth rotation angle
  ! :param(in)  real(8)   eo: Equation of the origin
  ! :param(out) real(8) gast: Greenwich apparent sidereal time (Unit: rad)
  !                           (グリニッジ視恒星時)
  subroutine gw_gast(era, eo, gast)
    implicit none
    real(DP), intent(in)  :: era, eo
    real(DP), intent(out) :: gast

    gast = era - eo
    call norm_angle(gast)
  end subroutine gw_gast

  ! Greenwich mean sidereal time, IAU 2006.
  !
  ! :param(in)  real(8) gast: Greenwich apparent sidereal time, グリニッジ視恒星時
  ! :param(in)  real(8)    t: Julian Century Number
  ! :param(out) real(8) gmst: Greenwich mean sidereal time (Unit: rad)
  !                           グリニッジ平均恒星時)
  subroutine gw_gmst(gast, t, gmst)
    implicit none
    real(DP), intent(in)  :: gast, t
    real(DP), intent(out) :: gmst

    gmst = gast &
      & + (   0.014506_DP      &
      & + (4612.156534_DP      &
      & + (   1.3915817_DP     &
      & + (  -0.00000044_DP    &
      & + (  -0.000029956_DP   &
      & + (  -0.0000000368_DP) &
      & * t) * t) * t) * t) * t) * AS2R
    call norm_angle(gmst)
  end subroutine gw_gmst

  ! Equation of Equinoxes

  ! :param(in)  real(8) gast: Greenwich apparent sidereal time, グリニッジ視恒星時
  ! :param(in)  real(8) gmst: Greenwich mean sidereal time, グリニッジ平均恒星時
  ! :param(out) real(8)   ee: Equation of Equinoxes (Unit: rad)
  !                           (分点均差)
  subroutine gw_ee(gast, gmst, ee)
    implicit none
    real(DP), intent(in)  :: gast, gmst
    real(DP), intent(out) :: ee

    ee = gast - gmst
  end subroutine gw_ee
end module greenwich

