!*******************************************************************************
! Modules for matrix calculation
!
!   date          name            version
!   2018.10.27    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module matrix
  use const
  implicit none

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! 回転行列生成(x軸中心)
  !   ( 1      0          0     )
  !   ( 0  +cos(phi)  +sin(phi) )
  !   ( 0  -sin(phi)  +cos(phi) )
  !
  ! :param(in)  real(DP)    phi: Angle (Unit: rad)
  ! :param(out) real(DP) mtx(3): Rotated matrix
  subroutine mtx_x(phi, mtx)
    implicit none
    real(DP), intent(in)  :: phi
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: s, c

    s = sin(phi)
    c = cos(phi)
    mtx= transpose(reshape( &
      & (/1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, c, s, 0.0_DP, -s, c/), &
      & (/3, 3/) &
    & ))
  end subroutine mtx_x

  ! 回転行列生成(y軸中心)
  !   ( +cos(theta)  0  -sin(theta) )
  !   (     0        1      0       )
  !   ( +sin(theta)  0  +cos(theta) )
  !
  ! :param(in)  real(DP)  theta: Angle (Unit: rad)
  ! :param(out) real(DP) mtx(3): Rotated matrix
  subroutine mtx_y(theta, mtx)
    implicit none
    real(DP), intent(in)  :: theta
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: s, c

    s = sin(theta)
    c = cos(theta)
    mtx = transpose(reshape( &
      & (/c, 0.0_DP, -s, 0.0_DP, 1.0_DP, 0.0_DP, s, 0.0_DP, c/), &
      & (/3, 3/) &
    & ))
  end subroutine mtx_y

  ! 回転行列生成(z軸中心)
  !   ( +cos(psi)  +sin(psi)  0 )
  !   ( -sin(psi)  +cos(psi)  0 )
  !   (     0          0      1 )
  !
  ! :param(in)  real(DP)    psi: Angle (Unit: rad)
  ! :param(out) real(DP) mtx(3): Rotated matrix
  subroutine mtx_z(psi, mtx)
    implicit none
    real(DP), intent(in)  :: psi
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: s, c

    s = sin(psi)
    c = cos(psi)
    mtx = transpose(reshape( &
      & (/c, s, 0.0_DP, -s, c, 0.0_DP, 0.0_DP, 0.0_DP, 1.0_DP/), &
      & (/3, 3/) &
    & ))
  end subroutine mtx_z
end module matrix

