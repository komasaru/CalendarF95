!****************************************************
! Modules for angles
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module angle
  use const, only : DP, PI2
  implicit none
  private
  public :: norm_angle

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Normalize angle into the range 0 <= a < 2pi.
  !
  ! :param(inout) real(8) angle: Before/After normalized
  subroutine norm_angle(angle)
    implicit none
    real(DP), intent(inout) :: angle

    do while (angle < 0.0_DP)
      angle = angle + PI2
    end do
    do while (angle > PI2)
      angle = angle - PI2
    end do
  end subroutine norm_angle
end module angle

