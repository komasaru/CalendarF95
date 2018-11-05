!****************************************************
! Modules for nutation calculation
!
! date          name            version
! 2018.10.25    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module eph_bpn
  use const
  use matrix
  use nutation
  implicit none
  private
  public :: calc_obliquity, calc_nutation, &
          & apply_bias, apply_bias_prec, apply_bias_prec_nut, &
          & apply_prec, apply_prec_nut, apply_nut

contains
  ! ============================
  ! Private subroutines/functions
  ! ============================

  ! Bias 変換行列（一般的な理論）
  ! * 赤道座標(J2000.0)の極は ICRS の極に対して12時（x軸のマイナス側）の
  !   方向へ 17.3±0.2 mas、18時（y軸のマイナス側）の方向へ 5.1±0.2 mas
  !   ズレているので、変換する。
  !   さらに、平均分点への変換はICRSでの赤経を78±10 mas、天の極を中心に
  !   回転させる。
  !     18時の方向の変換はx軸を-5.1mas回転
  !                 | 1     0      0   |
  !       R1(θ ) = | 0   cosθ  sinθ |
  !                 | 0  -sinθ  cosθ |
  !     12時の方向の変換はy軸を-17.3mas回転
  !                 | cosθ  0  -sinθ |
  !       R2(θ ) = |   0    1     0   |
  !                 | sinθ  0   cosθ |
  !     天の極を中心に78.0mas回転
  !                 |  cosθ  sinθ  0 |
  !       R3(θ ) = | -sinθ  cosθ  0 |
  !                 |    0      0    1 |
  !
  ! :param(out) real(8) mtx(3, 3): 回転行列
  subroutine mtx_b(mtx)
    implicit none
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: mtx_1(3, 3), mtx_2(3, 3), mtx_3(3, 3)

    call mtx_x( -5.1 * MAS2R, mtx_1)
    call mtx_y(-17.3 * MAS2R, mtx_2)
    call mtx_z( 78.0 * MAS2R, mtx_3)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
  end subroutine mtx_b

  ! Bias + Precession 変換行列
  ! * IAU 2006 (Fukushima-Williams 4-angle formulation) 理論
  !
  ! :param(in)  real(8)         t: Julian Century Number
  ! :param(in)  real(8)       eps: 平均黄道傾斜角(ε)
  ! :param(out) real(8) mtx(3, 3): 回転行列
  subroutine mtx_bp(t, eps, mtx)
    implicit none
    real(DP), intent(in)  :: t, eps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: gam, phi, psi
    real(DP) :: mtx_1(3, 3), mtx_2(3, 3), mtx_3(3, 3), mtx_4(3, 3)

    gam = calc_gamma_bp(t)
    phi = calc_phi_bp(t)
    psi = calc_psi_bp(t)
    call mtx_z( gam, mtx_1)
    call mtx_x( phi, mtx_2)
    call mtx_z(-psi, mtx_3)
    call mtx_x(-eps, mtx_4)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
    mtx = matmul(mtx_4, mtx)
  end subroutine mtx_bp

  ! Bias + Precession + Nutation 変換行列
  ! * IAU 2006 (Fukushima-Williams 4-angle formulation) 理論
  !
  ! :param(out) real(8)         t: Julian Century Number
  ! :param(out) real(8)       eps: 平均黄道傾斜角(ε)
  ! :param(out) real(8)      dpsi: 章動(Δψ)
  ! :param(out) real(8)      deps: 章動(Δε)
  ! :param(out) real(8) mtx(3, 3): 回転行列
  subroutine mtx_bpn(t, eps, dpsi, deps, mtx)
    implicit none
    real(DP), intent(in)  :: t, eps, dpsi, deps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: gam, phi, psi
    real(DP) :: mtx_1(3, 3), mtx_2(3, 3), mtx_3(3, 3), mtx_4(3, 3)

    gam = calc_gamma_bp(t)
    phi = calc_phi_bp(t)
    psi = calc_psi_bp(t)
    call mtx_z(gam, mtx_1)
    call mtx_x(phi, mtx_2)
    call mtx_z(-psi - dpsi, mtx_3)
    call mtx_x(-eps - deps, mtx_4)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
    mtx = matmul(mtx_4, mtx)
  end subroutine mtx_bpn

  ! precession（歳差）変換行列（J2000.0 用）
  ! * 歳差の変換行列
  !     P(ε, ψ, φ, γ) = R1(-ε) * R3(-ψ) * R1(φ) * R3(γ)
  !   但し、R1, R2, R3 は x, y, z 軸の回転。
  !              | 1     0      0   |           |  cosθ  sinθ  0 |
  !     R1(θ) = | 0   cosθ  sinθ |, R3(θ) = | -sinθ  cosθ  0 |
  !              | 0  -sinθ  cosθ |           |    0      0    1 |
  !                         | P_11 P_12 P_13 |
  !     P(ε, ψ, φ, γ) = | P_21 P_22 P_23 | とすると、
  !                         | P_31 P_32 P_33 |
  !     P_11 = cosψcosγ + sinψcosφsinγ
  !     P_12 = cosψsinγ - sinψcosφcosγ
  !     P_13 = -sinψsinφ
  !     P_21 = cosεsinψcosγ - (cosεcosψcosφ + sinεsinφ)sinγ
  !     P_22 = cosεsinψcosγ + (cosεcosψcosφ + sinεsinφ)cosγ
  !     P_23 = cosεcosψsinφ - sinεcosφ
  !     P_31 = sinεsinψcosγ - (sinεcosψcosφ - cosεsinφ)sinγ
  !     P_32 = sinεsinψcosγ + (sinεcosψcosφ - cosεsinφ)cosγ
  !     P_33 = sinεcosψsinφ + cosεcosφ
  !
  ! :param(in)  real(8)         t: Julian Century Number
  ! :param(in)  real(8)       eps: 黄道傾斜角(ε)
  ! :param(out) real(8) mtx(3, 3): 回転行列
  subroutine mtx_p(t, eps, mtx)
    implicit none
    real(DP), intent(in)  :: t, eps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: gam, phi, psi
    real(DP) :: mtx_1(3, 3),  mtx_2(3, 3),  mtx_3(3, 3),  mtx_4(3, 3)

    gam = calc_gamma_p(t)
    phi = calc_phi_p(t)
    psi = calc_psi_p(t)
    call mtx_z( gam, mtx_1)
    call mtx_x( phi, mtx_2)
    call mtx_z(-psi, mtx_3)
    call mtx_x(-eps, mtx_4)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
    mtx = matmul(mtx_4, mtx)
  end subroutine mtx_p

  ! Precession + Nutation 変換行列
  ! * IAU 2000A nutation with adjustments to match the IAU 2006 precession.
  !
  ! :param(in)  real(8)         t: Julian Century Number
  ! :param(in)  real(8)       eps: 黄道傾斜角(ε)
  ! :param(out) real(8)      dpsi: 章動(Δψ)
  ! :param(out) real(8)      deps: 章動(Δε)
  ! :param(out) real(8) mtx(3, 3): 回転行列
  subroutine mtx_pn(t, eps, dpsi, deps, mtx)
    implicit none
    real(DP), intent(in)  :: t, eps, dpsi, deps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: gam, phi, psi
    real(DP) :: mtx_1(3, 3),  mtx_2(3, 3),  mtx_3(3, 3),  mtx_4(3, 3)

    gam = calc_gamma_p(t)
    phi = calc_phi_p(t)
    psi = calc_psi_p(t)
    call mtx_z( gam, mtx_1)
    call mtx_x( phi, mtx_2)
    call mtx_z(-psi - dpsi, mtx_3)
    call mtx_x(-eps - deps, mtx_4)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
    mtx = matmul(mtx_4, mtx)
  end subroutine mtx_pn

  ! nutation（章動）変換行列
  ! * IAU 2000A nutation with adjustments to match the IAU 2006 precession.
  !
  ! :param(out) real(8)       eps: 平均黄道傾斜角(ε)
  ! :param(out) real(8)      dpsi: 章動(Δψ)
  ! :param(out) real(8)      deps: 章動(Δε)
  ! :param(out) real(8) mtx(3, 3): 変換行列
  subroutine  mtx_n(eps, dpsi, deps, mtx)
    implicit none
    real(DP), intent(in)  :: eps, dpsi, deps
    real(DP), intent(out) :: mtx(3, 3)
    real(DP) :: mtx_1(3, 3),  mtx_2(3, 3),  mtx_3(3, 3)

    call mtx_x(eps,         mtx_1)
    call mtx_z(-dpsi,       mtx_2)
    call mtx_x(-eps - deps, mtx_3)
    mtx = matmul(mtx_2, mtx_1)
    mtx = matmul(mtx_3, mtx)
  end subroutine  mtx_n

  ! 歳差変換行列用 gamma 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) gam: γ
  function calc_gamma_p(t) result(gam)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: gam

    gam = ((10.556403_DP      &
      & +  ( 0.4932044_DP     &
      & +  (-0.00031238_DP    &
      & +  (-0.000002788_DP   &
      & +  ( 0.0000000260_DP) &
      & *  t) * t) * t) * t) * t) * AS2R
  end function calc_gamma_p

  ! 歳差変換行列用 phi 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) phi: φ
  function calc_phi_p(t) result(phi)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: phi

    phi = (84381.406000_DP      &
      & + (  -46.811015_DP      &
      & + (    0.0511269_DP     &
      & + (    0.00053289_DP    &
      & + (   -0.000000440_DP   &
      & + (   -0.0000000176_DP) &
      & * t) * t) * t) * t) * t) * AS2R
  end function calc_phi_p

  ! 歳差変換行列用 psi 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) psi: ψ
  function calc_psi_p(t) result(psi)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: psi

    psi = (( 5038.481507_DP      &
      & +  (    1.5584176_DP     &
      & +  (   -0.00018522_DP    &
      & +  (   -0.000026452_DP   &
      & +  (   -0.0000000148_DP) &
      & *  t) * t) * t) * t) * t) * AS2R
  end function calc_psi_p

  ! バイアス＆歳差変換行列用 gamma 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) gam: γ
  function calc_gamma_bp(t) result(gam)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: gam

    gam = (-0.052928_DP      &
      & + (10.556378_DP      &
      & + ( 0.4932044_DP     &
      & + (-0.00031238_DP    &
      & + (-0.000002788_DP   &
      & + ( 0.0000000260_DP) &
      & * t) * t) * t) * t) * t) * AS2R
  end function calc_gamma_bp

  ! バイアス＆歳差変換行列用 phi 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) phi: φ
  function calc_phi_bp(t) result(phi)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: phi

    phi = (84381.412819_DP      &
      & + (  -46.811016_DP      &
      & + (    0.0511268_DP     &
      & + (    0.00053289_DP    &
      & + (   -0.000000440_DP   &
      & + (   -0.0000000176_DP) &
      & * t) * t) * t) * t) * t) * AS2R
  end function calc_phi_bp

  ! バイアス＆歳差変換行列用 psi 計算
  !
  ! :param(in) real(8)   t: Julian Century Number
  ! :return    real(8) psi: ψ
  function  calc_psi_bp(t) result(psi)
    implicit none
    real(DP), intent(in) :: t
    real(DP) :: psi

    psi =(  -0.041775_DP      &
      & +(5038.481484_DP      &
      & +(   1.5584175_DP     &
      & +(  -0.00018522_DP    &
      & +(  -0.000026452_DP   &
      & +(  -0.0000000148_DP) &
      & * t) * t) * t) * t) * t) * AS2R
  end function  calc_psi_bp

  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! 黄道傾斜角計算
  ! * 黄道傾斜角 ε （単位: rad）を計算する。
  !   以下の計算式により求める。
  !     ε = 84381.406 - 46.836769 * T - 0.0001831 T^2 + 0.00200340 T^3
  !       - 5.76 * 10^(-7) * T^4 - 4.34 * 10^(-8) * T^5
  !   ここで、 T は J2000.0 からの経過時間を 36525 日単位で表したユリウス
  !   世紀数で、 T = (JD - 2451545) / 36525 である。
  !
  ! :param(in)  real(8)   t: ユリウス世紀数
  ! :param(out) real(8) eps: 平均黄道傾斜角(ε)
  subroutine calc_obliquity(t, eps)
    implicit none
    real(DP), intent(in)  :: t
    real(DP), intent(out) :: eps

    eps = (84381.406_DP      &
      & + (  -46.836769_DP   &
      & + (   -0.0001831_DP  &
      & + (    0.00200340_DP &
      & + (   -5.76e-7_DP    &
      & + (   -4.34e-8_DP)   &
      & * t) * t) * t) * t) * t) * AS2R
  end subroutine calc_obliquity

  ! 章動計算
  !
  ! :param(in)  real(8)    t: Julian Century Number
  ! :param(out) real(8) dpsi: 章動(Δψ)
  ! :param(out) real(8) deps: 章動(Δε)
  subroutine calc_nutation(t, dpsi, deps)
    implicit none
    real(DP), intent(in)  :: t
    real(DP), intent(out) :: dpsi, deps
    real(DP) :: fj2
    real(DP) :: dpsi_ls, dpsi_pl
    real(DP) :: deps_ls, deps_pl

    fj2 = -2.7774e-6_DP * t
    call calc_lunisolar(t, dpsi_ls, deps_ls)
    call calc_planetary(t, dpsi_pl, deps_pl)
    dpsi = dpsi_ls + dpsi_pl
    deps = deps_ls + deps_pl
    dpsi = dpsi + dpsi * (0.4697e-6_DP + fj2)
    deps = deps + deps * fj2
  end subroutine calc_nutation

  ! Bias 適用
  !
  ! :param(in)  real(8) src: 元の座標
  ! :param(out) real(8) dst: 適用後座標
  subroutine apply_bias(src, dst)
    implicit none
    real(DP), intent(in)  :: src(3)
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_b(mtx)
    dst = matmul(mtx, src)
  end subroutine apply_bias

  ! Bias + Precession(歳差) 適用
  !
  ! :param(in)  real(8) src(3): 元の座標
  ! :param(in)  real(8)      t: Julian Century Number
  ! :param(in)  real(8)    eps: 平均黄道傾斜角(ε)
  ! :param(out) real(8) dst(3): 適用後座標
  subroutine apply_bias_prec(src, t, eps, dst)
    implicit none
    real(DP), intent(in)  :: src(3), t, eps
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_bp(t, eps, mtx)
    dst = matmul(mtx, src)
  end subroutine apply_bias_prec

  ! Bias + Precession(歳差) + Nutation(章動)適用
  !
  ! :param(in)  real(8) src(3): 元の座標
  ! :param(in)  real(8)      t: Julian Century Number
  ! :param(in)  real(8)    eps: 平均黄道傾斜角(ε)
  ! :param(in)  real(8)   dpsi: 章動(Δψ)
  ! :param(in)  real(8)   deps: 章動(Δε)
  ! :param(out) real(8) dst(3): 適用後座標
  subroutine apply_bias_prec_nut(src, t, eps, dpsi, deps, dst)
    implicit none
    real(DP), intent(in)  :: src(3), t, eps, dpsi, deps
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_bpn(t, eps, dpsi, deps, mtx)
    dst = matmul(mtx, src)
  end subroutine apply_bias_prec_nut

  ! Precession(歳差) 適用
  !
  ! :param(in)  real(8) src(3): 元の座標
  ! :param(in)  real(8)      t: Julian Century Number
  ! :param(in)  real(8)    eps: 平均黄道傾斜角(ε)
  ! :param(out) real(8) dst(3): 適用後座標
  subroutine apply_prec(src, t, eps, dst)
    implicit none
    real(DP), intent(in)  :: src(3), t, eps
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_p(t, eps, mtx)
    dst = matmul(mtx, src)
  end subroutine apply_prec

  ! Precession(歳差) + Nutation(章動) 適用
  !
  ! :param(in)  real(8) src(3): 元の座標
  ! :param(in)  real(8)      t: Julian Century Number
  ! :param(in)  real(8)    eps: 平均黄道傾斜角(ε)
  ! :param(in)  real(8)   dpsi: 章動(Δψ)
  ! :param(in)  real(8)   deps: 章動(Δε)
  ! :param(out) real(8) dst(3): 適用後座標
  subroutine apply_prec_nut(src, t, eps, dpsi, deps, dst)
    implicit none
    real(DP), intent(in)  :: src(3), t, eps, dpsi, deps
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_pn(t, eps, dpsi, deps, mtx)
    dst = matmul(mtx, src)
  end subroutine apply_prec_nut

  ! Nutation(章動) 適用
  !
  ! :param(in)  real(8) src(3): 元の座標
  ! :param(in)  real(8)    eps: 平均黄道傾斜角(ε)
  ! :param(in)  real(8)   dpsi: 章動(Δψ)
  ! :param(in)  real(8)   deps: 章動(Δε)
  ! :param(out) real(8) dst(3): 適用後座標
  subroutine apply_nut(src, eps, dpsi, deps, dst)
    implicit none
    real(DP), intent(in)  :: src(3), eps, dpsi, deps
    real(DP), intent(out) :: dst(3)
    real(DP) :: mtx(3, 3)

    call mtx_n(eps, dpsi, deps, mtx)
    dst = matmul(mtx, src)
  end subroutine apply_nut
end module eph_bpn

