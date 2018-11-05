!****************************************************
! Modules for precession calculation
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module precession
  use const
  implicit none
  private
  public :: pfw_06, obl_06

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Precession angles, IAU 2006 (Fukushima-Williams 4-angle formulation)
  !
  ! :param(in)  real(8)     t: ユリウス世紀数
  ! :param(out) real(8) gam_b: γ（実際には上にバーが付く）
  ! :param(out) real(8) phi_b: φ（実際には上にバーが付く）
  ! :param(out) real(8) psi_b: ψ（実際には上にバーが付く）
  subroutine pfw_06(t, gam_b, phi_b, psi_b)
    implicit none
    real(DP), intent(in)  :: t
    real(DP), intent(out) :: gam_b, phi_b, psi_b

    gam_b = (   -0.052928_DP      &
        & + (   10.556378_DP      &
        & + (    0.4932044_DP     &
        & + (   -0.00031238_DP    &
        & + (   -0.000002788_DP   &
        & + (    0.0000000260_DP) &
        & * t) * t) * t) * t) * t) * AS2R
    phi_b = (84381.412819_DP      &
        & + (  -46.811016_DP      &
        & + (    0.0511268_DP     &
        & + (    0.00053289_DP    &
        & + (   -0.000000440_DP   &
        & + (   -0.0000000176_DP) &
        & * t) * t) * t) * t) * t) * AS2R
    psi_b = (   -0.041775_DP      &
        & + ( 5038.481484_DP      &
        & + (    1.5584175_DP     &
        & + (   -0.00018522_DP    &
        & + (   -0.000026452_DP   &
        & + (   -0.0000000148_DP) &
        & * t) * t) * t) * t) * t) * AS2R
  end subroutine pfw_06

  ! Mean obliquity of the ecliptic, IAU 2006 precession model.
  !
  ! :param(in)  real(8)     t: ユリウス世紀数
  ! :param(out) real(8) eps_a: Mean obliquity of the ecliptic
  subroutine obl_06(t, eps_a)
    implicit none
    real(DP), intent(in)  :: t
    real(DP), intent(out) :: eps_a

    eps_a = (84381.406_DP         &
        & + (  -46.836769_DP      &
        & + (   -0.0001831_DP     &
        & + (    0.00200340_DP    &
        & + (   -0.000000576_DP   &
        & + (   -0.0000000434_DP) &
        & * t) * t) * t) * t) * t) * AS2R
  end subroutine obl_06
end module precession

