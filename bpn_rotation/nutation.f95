!****************************************************
! Modules for nutation calculation
!
! date          name            version
! 2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module nutation
  use const
  use fundamental_argument
  implicit none
  private
  public :: calc_lunisolar, calc_planetary

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! 日月章動(luni-solar nutation)の計算
  ! * ユリウス世紀数と定数ファイル(NUT_LS.txt)から日月章動を計算
  ! * NUT_LS.txt の1行目はヘッダとみなし、読み飛ばす
  !
  ! :param(in)  real(8)      jc: ユリウス世紀数(Julian Century)
  ! :param(out) real(8) dpsi_ls: 黄経における章動(Δψ)
  ! :param(out) real(8) deps_ls: 黄道傾斜における章動(Δε)
  subroutine calc_lunisolar(jc, dpsi_ls, deps_ls)
    implicit none
    real(DP), intent(in)  :: jc
    real(DP), intent(out) :: dpsi_ls, deps_ls
    real(DP)    :: l, lp, f, d, om
    integer(SP) :: i
    integer(SP) :: a(0:4)
    real(DP)    :: b(5:10)
    real(DP)    :: arg, arg_s, arg_c

    dpsi_ls = 0.0_DP
    deps_ls = 0.0_DP
    l  = l_iers2003(jc)
    lp = lp_mhb2000(jc)
    f  = f_iers2003(jc)
    d  = d_mhb2000(jc)
    om = om_iers2003(jc)

    do i = 1, size(NUT_LS)
      read (NUT_LS(i), *) a, b
      b = b * 10000.0_DP
      arg = mod((a(0) * l + a(1) * lp + a(2) * f &
        & + a(3) * d + a(4) * om), PI2)
      if (arg < 0.0_DP) arg = arg + PI2
      arg_s = sin(arg)
      arg_c = cos(arg)
      dpsi_ls = dpsi_ls + (b(5) + b(6) * jc) * arg_s + b( 7) * arg_c
      deps_ls = deps_ls + (b(8) + b(9) * jc) * arg_c + b(10) * arg_s
    end do

    dpsi_ls = dpsi_ls * U2R
    deps_ls = deps_ls * U2R
  end subroutine calc_lunisolar

  ! 惑星章動(planetary nutation)
  ! * ユリウス世紀数と定数ファイル(NUT_PL.txt)から惑星章動を計算
  !
  ! :param(in)  real(8)      jc: ユリウス世紀数(Julian Century)
  ! :param(out) real(8) dpsi_pl: 黄経における章動(Δψ)
  ! :param(out) real(8) deps_pl: 黄道傾斜における章動(Δε)
  subroutine calc_planetary(jc, dpsi_pl, deps_pl)
    implicit none
    real(DP), intent(in)  :: jc
    real(DP), intent(out) :: dpsi_pl, deps_pl
    real(DP)    :: l, f, d, om, pa, me, ve, ea, ma, ju, sa, ur, ne
    integer(SP) :: i
    integer(SP) :: a(0:13)
    real(DP)    :: b(14:17)
    real(DP)    :: arg, arg_s, arg_c

    dpsi_pl = 0.0_DP
    deps_pl = 0.0_DP
    l  = l_mhb2000(jc)
    f  = f_mhb2000(jc)
    d  = d_mhb2000_2(jc)
    om = om_mhb2000(jc)
    pa = pa_iers2003(jc)
    me = me_iers2003(jc)
    ve = ve_iers2003(jc)
    ea = ea_iers2003(jc)
    ma = ma_iers2003(jc)
    ju = ju_iers2003(jc)
    sa = sa_iers2003(jc)
    ur = ur_iers2003(jc)
    ne = ne_mhb2000(jc)

    do i = 1, size(NUT_PL)
      read (NUT_PL(i), *) a, b
      b = b * 10000.0_DP
      arg = mod( &
        &   a( 0) * l  + a( 2) * f  + a( 3) * d  + a( 4) * om &
        & + a( 5) * me + a( 6) * ve + a( 7) * ea + a( 8) * ma &
        & + a( 9) * ju + a(10) * sa + a(11) * ur + a(12) * ne &
        & + a(13) * pa, PI2)
      if (arg < 0.0_DP) arg = arg + PI2
      arg_s = sin(arg)
      arg_c = cos(arg)
      dpsi_pl = dpsi_pl + b(14) * arg_s + b(15) * arg_c
      deps_pl = deps_pl + b(16) * arg_s + b(17) * arg_c
    end do

    dpsi_pl = dpsi_pl * U2R
    deps_pl = deps_pl * U2R
  end subroutine calc_planetary
end module nutation

