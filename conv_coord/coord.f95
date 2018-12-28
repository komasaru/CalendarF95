!****************************************************
! Modules for coordinate calculation
!
! date          name            version
! 2018.10.17    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module coord
  use const
  use matrix
  use trigonometric
  implicit none

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! 直交座標：赤道座標 -> 黄道座標.
  !
  ! :param(in)  real(DP) rect_eq(3): 赤道直交座標 [x, y, z]
  ! :param(in)  real(DP)        eps: 黄道傾斜角 (Unit: rad)
  ! :param(out) real(DP) rect_ec(3): 黄道直交座標 [x, y, z]
  subroutine rect_eq2ec(rect_eq, eps, rect_ec)
    implicit none
    real(DP), intent(in)  :: rect_eq(3), eps
    real(DP), intent(out) :: rect_ec(3)
    real(DP) :: mtx(3, 3)

    call mtx_x(eps, mtx)
    rect_ec = matmul(mtx, rect_eq)
  end subroutine rect_eq2ec

  ! 直交座標：黄道座標 -> 赤道座標.
  !
  ! :param(in)  real(DP) rect_ec(3): 黄道直交座標 [x, y, z]
  ! :param(in)  real(DP)        eps: 黄道傾斜角 (Unit: rad)
  ! :param(out) real(DP) rect_eq(3): 赤道直交座標 [x, y, z]
  subroutine rect_ec2eq(rect_ec, eps, rect_eq)
    implicit none
    real(DP), intent(in)  :: rect_ec(3), eps
    real(DP), intent(out) :: rect_eq(3)
    real(DP) :: mtx(3, 3)

    call mtx_x(-eps, mtx)
    rect_eq = matmul(mtx, rect_ec)
  end subroutine rect_ec2eq

  ! 直交座標 -> 極座標
  !
  ! :param(in)  real(DP) rect(3): 直交座標 [x, y, z]
  ! :param(out) real(DP)  pol(3): 極座標 [λ, φ, 距離]
  subroutine rect2pol(rect, pol)
    implicit none
    real(DP), intent(in)  :: rect(3)
    real(DP), intent(out) :: pol(3)
    real(DP) :: r, lmd, phi, d

    r = sqrt(rect(1)**2 + rect(2)**2)
    lmd = atan2(rect(2), rect(1))
    phi = atan2(rect(3), r)
    do while (lmd < 0.0_DP)
      lmd = lmd + PI * 2.0_DP
    end do
    d = sqrt(sum(rect * rect))
    pol = (/lmd, phi, d/)
  end subroutine rect2pol

  ! 極座標：赤道座標 -> 黄道座標
  !
  ! :param(in)  real(8) pol_eq(3): 赤道極座標 [α, δ, 距離]
  ! :param(in)  real(8)       eps: 黄道傾斜角 (Unit: rad)
  ! :param(out) real(8) pol_ec(3): 黄道極座標 [λ, β, 距離]
  subroutine pol_eq2ec(pol_eq, eps, pol_ec)
    implicit none
    real(DP), intent(in)  :: pol_eq(3), eps
    real(DP), intent(out) :: pol_ec(3)
    real(DP) :: lmd, bet

    lmd = comp_lambda(pol_eq(1), pol_eq(2), eps)
    bet = comp_beta(pol_eq(1), pol_eq(2), eps)
    pol_ec = (/lmd, bet, pol_eq(3)/)
  end subroutine pol_eq2ec

  ! 極座標：黄道座標 -> 赤道座標.
  !
  ! :param(in)  real(8) pol_ec(3): 赤道極座標 [λ, β, 距離]
  ! :param(in)  real(8)       eps: 黄道傾斜角 (Unit: rad)
  ! :param(out) real(8) pol_eq(3): 黄道極座標 [α, δ, 距離]
  subroutine pol_ec2eq(pol_ec, eps, pol_eq)
    implicit none
    real(DP), intent(in)  :: pol_ec(3), eps
    real(DP), intent(out) :: pol_eq(3)
    real(DP) :: alp, dlt

    alp = comp_alpha(pol_ec(1), pol_ec(2), eps)
    dlt = comp_delta(pol_ec(1), pol_ec(2), eps)
    pol_eq = (/alp, dlt, pol_ec(3)/)
  end subroutine pol_ec2eq

  ! 極座標 -> 直交座標
  !
  ! :param(in)  real(DP)  pol(3): 極座標 [λ, φ, 距離]
  ! :param(out) real(DP) rect(3): 直交座標 [x, y, z]
  subroutine pol2rect(pol, rect)
    implicit none
    real(DP), intent(in)  :: pol(3)
    real(DP), intent(out) :: rect(3)
    real(DP) :: mtx_0(3, 3), mtx_1(3, 3), mtx(3, 3)
    integer :: i

    call mtx_y(pol(2), mtx_0)
    call mtx_z(-pol(1), mtx_1)
    mtx = matmul(mtx_1, mtx_0)
    rect = matmul(mtx, (/pol(3), 0.0_DP, 0.0_DP/))
  end subroutine pol2rect
end module coord

