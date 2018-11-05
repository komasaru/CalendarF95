!****************************************************
! Modules for CIP/CIO calculation
! *  CIP(Celestial Intermediate Pole, 瞬時の極軸),
! *  CIO(Celestial Intermediate Origin, 非回転原点)
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module cip_cio
  use const
  use fundamental_argument
  implicit none
  private
  public :: bpn2xy, s_06

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Extract from the bias-precession-nutation matrix the X,Y
  ! coordinates of the Celestial Intermediate Pole.
  !
  ! :param(in)  real(8) r(3, 3): Rotation Matrix
  ! :param(out) real(8)   xy(2): x, y cordinates of CIP
  subroutine bpn2xy(r, xy)
    implicit none
    real(DP), intent(in)  :: r(3, 3)
    real(DP), intent(out) :: xy(2)

    xy = (/r(3, 1), r(3, 2)/)
  end subroutine bpn2xy

  ! The CIO locator s, positioning the Celestial Intermediate Origin on
  ! the equator of the Celestial Intermediate Pole, given the CIP's X,Y
  ! coordinates.  Compatible with IAU 2006/2000A precession-nutation.
  !
  ! :param(in)  real(8)    t: Julian Century Number
  ! :param(in)  real(8) x(2): (x, y) coordinate of CIP
  ! :param(out) real(8)    s: CIO locator (Unit: rad)
  subroutine s_06(t, xy, s)
    implicit none
    real(DP), intent(in)  :: t, xy(2)
    real(DP), intent(out) :: s
    real(DP)    :: fas(8), w(0:5), b(2), v
    integer(SP) :: i, j, a(8)

    ! Fundamental Arguments (from IERS Conventions 2003)
    ! * Mean anomaly of the Moon.(Ref: iauFal03(t))
    ! * Mean anomaly of the Sun.(Ref: iauFalp03(t))
    ! * Mean longitude of the Moon minus that of the ascending node.(Ref: iauFaf03(t))
    ! * Mean elongation of the Moon from the Sun.(Ref: iauFad03(t))
    ! * Mean longitude of the ascending node of the Moon.(Ref: iauFaom03(t))
    ! * Mean longitude of Venus.(Ref: iauFave03(t))
    ! * Mean longitude of Earth.(Ref: iauFae03(t))
    ! * General precession in longitude.(Ref: iauFapa03(t))
    fas = (/ &
      & l_iers2003(t),  p_iers2003(t),  f_iers2003(t),  d_iers2003(t), &
      & om_iers2003(t), ve_iers2003(t), ea_iers2003(t), pa_iers2003(t) &
    & /)
    ! Evaluate s.
    w = POL
    do i = size(S_0), 1, -1
      read (S_0(i), *) a, b
      v = 0.0_DP
      do j = 1, 8
        v = v + a(j) * fas(j)
      end do
      w(0) = w(0) + b(1) * sin(v) + b(2) * cos(v)
    end do
    do i = size(S_1), 1, -1
      read (S_1(i), *) a, b
      v = 0.0_DP
      do j = 1, 8
        v = v + a(j) * fas(j)
      end do
      w(1) = w(1) + b(1) * sin(v) + b(2) * cos(v)
    end do
    do i = size(S_2), 1, -1
      read (S_2(i), *) a, b
      v = 0.0_DP
      do j = 1, 8
        v = v + a(j) * fas(j)
      end do
      w(2) = w(2) + b(1) * sin(v) + b(2) * cos(v)
    end do
    do i = size(S_3), 1, -1
      read (S_3(i), *) a, b
      v = 0.0_DP
      do j = 1, 8
        v = v + a(j) * fas(j)
      end do
      w(3) = w(3) + b(1) * sin(v) + b(2) * cos(v)
    end do
    do i = size(S_4), 1, -1
      read (S_4(i), *) a, b
      v = 0.0_DP
      do j = 1, 8
        v = v + a(j) * fas(j)
      end do
      w(4) = w(4) + b(1) * sin(v) + b(2) * cos(v)
    end do

    s = 0.0_DP
    s = (w(0) + (w(1) + (w(2) + (w(3) + (w(4) + w(5) &
      & * t) * t) * t) * t) * t) * AS2R &
      & - xy(1) * xy(2) / 2.0_DP
  end subroutine s_06
end module cip_cio

