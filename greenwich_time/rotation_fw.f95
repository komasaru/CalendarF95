!****************************************************
! Modules for Rotation given the Fukushima-Williams angles
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module rotation_fw
  use matrix
  implicit none
  private
  public :: fw2m

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Form rotation matrix given the Fukushima-Williams angles.
  !
  ! :param(in)  real(8)     gam_b: γ-（実際には上にバーが付く）
  ! :param(in)  real(8)     phi_b: φ-（実際には上にバーが付く）
  ! :param(in)  real(8)       psi: ψ
  ! :param(in)  real(8)       eps: ε
  ! :param(out) real(8) mtx(3, 3): Rotation matrix
  subroutine fw2m(gam_b, phi_b, psi, eps, mtx)
    implicit none
    real(DP), intent(in)  :: gam_b, phi_b, psi, eps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: mtx_0(3, 3), mtx_1(3, 3), mtx_2(3, 3), mtx_3(3, 3)

    call mtx_z(gam_b, mtx_0)
    call mtx_x(phi_b, mtx_1)
    call mtx_z( -psi, mtx_2)
    call mtx_x( -eps, mtx_3)
    mtx = matmul(mtx_1, mtx_0)
    mtx = matmul(mtx_2, mtx)
    mtx = matmul(mtx_3, mtx)
  end subroutine fw2m
end module rotation_fw

