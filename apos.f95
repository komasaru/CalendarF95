!*******************************************************************************
! Modules for apparent position of Sun/Moon
!
!   date          name            version
!   2018.10.25    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module apos
  use const
  use coord
  use eph_bpn
  use eph_jpl
  use time, only : jd2jc
  implicit none
  private
  public :: get_icrs, get_dist_e, calc_sun, calc_moon

contains
  ! ============================
  ! Private subroutines/functions
  ! ============================

  ! 天体Aと天体Bの距離計算
  !
  ! :param(in) real(8) pos_a(3): 位置ベクトル
  ! :param(in) real(8) pos_b(3): 位置ベクトル
  ! :return    real(8)        d: 距離
  function distance(pos_a, pos_b) result(dist)
    implicit none
    real(DP), intent(in) :: pos_a(3), pos_b(3)
    real(DP)    :: dist
    integer(SP) :: i
    real(DP)    :: s

    s = 0.0_DP
    do i = 1, 3
      s = s + (pos_b(i) - pos_a(i))**2.0_DP
    end do
    dist = sqrt(s)
  end function distance

  ! 対象天体が光を発した時刻 t1 の計算（太陽・月専用）
  ! * 計算式： c * (t2 - t1) = r12  (但し、 c: 光の速度。 Newton 法で近似）
  ! * 太陽・月専用なので、太陽・木星・土星・天王星・海王星の重力場による
  !   光の曲がりは非考慮。
  !
  ! :param(in)  integer            a: 対象天体(11:太陽, 10:月)
  ! :param(in)  real(8)       jd_tdb: Julian Day for TDB
  ! :param(in)  real(8) icrs_2(6, 3): t2 における ICRS 座標（地球, 月, 太陽）
  ! :param(in)  real(8)           au: AU from binary data
  ! :param(out) real(8)          t_1: Julian Day
  subroutine calc_t1(a, jd_tdb, icrs_2, au, t_1)
    implicit none
    integer(SP),  intent(in)  :: a
    real(DP),     intent(in)  :: jd_tdb, icrs_2(6, 3), au
    real(DP),     intent(out) :: t_1
    integer(SP) :: m, i
    real(DP)    :: t_2, pv_1(6), df, df_wk
    real(DP)    :: r_12(3), d_12

    t_1 = jd_tdb
    t_2 = t_1
    call get_icrs(a, jd_tdb, pv_1)

    m = 0
    df = 1.0_DP
    do while (df > 1.0e-10_DP)
      r_12 = (/(pv_1(i) - icrs_2(i, 1), i=1,3)/)
      d_12 = distance(pv_1(1:3), icrs_2(1:3, 1))
      df = (C * SEC_DAY / au) * (t_2 - t_1) - d_12
      df_wk = sum((/(r_12(i) * icrs_2(i + 3, 3), i=1,3)/))
      df = df / ((C * SEC_DAY / (au * 1000.0_DP)) + df_wk / d_12)
      t_1 = t_1 + df
      m = m + 1
      if (m > 10) then
        print *, "[ERROR] Newton method error!"
        stop
      end if
      call get_icrs(11, jd_tdb, pv_1)
    end do
  end subroutine calc_t1

  ! 天体Aから見た天体Bの方向ベクトル計算（太陽・月専用）
  ! * 太陽・月専用なので、太陽・木星・土星・天王星・海王星の重力場による
  !   光の曲がりは非考慮。
  !
  ! :param(in)  real(8) pos_a(3): 位置ベクトル(天体A)
  ! :param(in)  real(8) pos_b(3): 位置ベクトル(天体B)
  ! :param(out) real(8)   vec(3): 方向(単位)ベクトル
  subroutine calc_unit_vector(pos_a, pos_b, vec)
    implicit none
    real(DP), intent(in)  :: pos_a(3), pos_b(3)
    real(DP), intent(out) :: vec(3)
    integer(SP) :: i
    real(DP)    :: w

    vec = (/(0.0_DP, i=1,3)/)
    w = distance(pos_a, pos_b)
    if (w == 0.0_DP) return
    vec = (/(pos_b(i) - pos_a(i), i=1,3)/)
    vec = (/(vec(i) / w, i=1,3)/)
  end subroutine calc_unit_vector

  ! 光行差の補正（方向ベクトルの Lorentz 変換）
  ! * vec_dd = f * vec_d + (1 + g / (1 + f)) * vec_v
  !   但し、 f = vec_v * vec_d  (ベクトル内積)
  !          g = sqrt(1 - v^2)  (v: 速度)
  !
  ! :param(in)  real(8) icrs_2(3): t2 における地球の ICRS 座標（速度）
  ! :param(in)  real(8)  vec_d(3): 方向（単位）ベクトル
  ! :param(in)  real(8)        au: AU from binary data
  ! :param(out) real(8) vec_dd(3): 補正後ベクトル
  subroutine conv_lorentz(icrs_2, vec_d, au, vec_dd)
    implicit none
    real(DP), intent(in)  :: icrs_2(3), vec_d(3), au
    real(DP), intent(out) :: vec_dd(3)
    integer(SP) :: i
    real(DP)    :: vec_v(3), g, f, vec_dd_1(3), vec_dd_2(3)

    vec_v = (/(icrs_2(i) / SEC_DAY / (C / (au * 1000.0_DP)), i=1,3)/)
    g = sum((/(vec_v(i) * vec_d(i), i=1,3)/))
    f = sqrt(1.0_DP - sqrt(sum((/(vec_v(i) * vec_v(i), i=1,3)/))))
    vec_dd_1 = (/(vec_d(i) * f, i=1,3)/)
    vec_dd_2 = (/((1.0_DP + g / (1.0_DP + f)) * vec_v(i), i=1,3)/)
    vec_dd = (/(vec_dd_1(i) + vec_dd_2(i), i=1,3)/)
    vec_dd = (/(vec_dd(i) / (1.0_DP + g), i=1,3)/)
  end subroutine conv_lorentz

  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! ICRS 座標取得
  ! * JPL DE430 データを自作 RubyGems ライブラリ eph_jpl を使用して取得
  !
  ! :param(in)  integer(4)  a: 天体番号
  ! :param(in)  real(8)    jd: Julian Day
  ! :param(out) real(8) pv(6): 位置(x, y, z), 速度(x, y, z)
  subroutine get_icrs(a, jd, pv)
    implicit none
    integer(SP), intent(in)  :: a
    real(DP),    intent(in)  :: jd
    real(DP),    intent(out) :: pv(6)
    type(t_bin)   :: bin   ! バイナリデータ用構造型
    integer(SP)   :: i
    real(DP)      :: jds(2)
    ! 計算対象フラグ
    integer(SP)   :: list(12) = (/(0, i=1,12)/)
    ! 算出データ（対象 - 基準）配列（位置(x, y, z)・速度(x, y, z)）
    pv(1:6) = (/(0.0_DP, i=1,6)/)

    ! バイナリデータ取得
    ! * 1レコード目:     ttl - numde
    ! * 2レコード目:     cval
    ! * 3レコード目以降: 係数
    call get_bin(jd, bin)

    ! インデックスの開始／終了 JD
    !jds = coeff(1:2)
    jds = bin%coeff(1:2)

    ! 計算対象フラグ一覧取得
    call get_list(a, 12, bin%ipt, list)

    ! 位置・速度計算
    call calc_rrd(a, 12, jd, jds, bin, list, pv)
  end subroutine get_icrs

  ! t2(= TDB) における地球と太陽・月の距離
  !
  ! :param(in)  real(8) icrs_2(6, 3): 地球、月、太陽の位置・速度
  ! :param(out) real(8)        d_e_m: 地球と月の距離
  ! :param(out) real(8)        d_e_s: 地球と太陽の距離
  subroutine get_dist_e(icrs_2, d_e_m, d_e_s)
    implicit none
    real(DP), intent(in)  :: icrs_2(6, 3)
    real(DP), intent(out) :: d_e_m, d_e_s

    d_e_m = distance(icrs_2(1:3, 1), icrs_2(1:3, 2))
    d_e_s = distance(icrs_2(1:3, 1), icrs_2(1:3, 3))
  end subroutine get_dist_e

  ! 太陽の視位置計算
  !
  ! :param(in)  real(8)       jd_tdb: Julian Day for TDB
  ! :param(in)  real(8) icrs_2(6, 3): t2 における ICRS 座標
  ! :param(in)  real(8)        d_e_s: 地球と太陽の距離
  ! :param(in)  real(8)          r_e: 地球の半径
  ! :param(in)  real(8)          r_s: 太陽の半径
  ! :param(in)  real(8)           au: AU from binary data
  ! :param(out) real(8)   apos_eq(3): (視赤経, 視赤緯, 地心距離)
  ! :param(out) real(8)   apos_ec(3): (視黄経, 視黄緯, 地心距離)
  ! :param(out) real(8)  rad_para(2): (視半径, 視差)
  subroutine calc_sun(jd_tdb, icrs_2, d_e_s, r_e, r_s, au, apos_eq, apos_ec, rad_para)
    implicit none
    real(DP),     intent(in) :: jd_tdb, icrs_2(6, 3), d_e_s, r_e, r_s, au
    real(DP),     intent(out) :: apos_eq(3), apos_ec(3), rad_para(2)
    integer(SP) :: i
    real(DP)    :: t_1_jd, icrs_1(6, 3), v_12(3), dd(3), t
    real(DP)    :: pos_sun(3), pos_sun_bpn(3), eps, dpsi, deps, rect_ec(3)

    ! === 太陽が光を発した時刻 t1(JD) の計算
    call calc_t1(11, jd_tdb, icrs_2, au, t_1_jd)
    ! === 時刻 t1(= TDB) におけるの地球・月・太陽の位置・速度（ICRS 座標）の計算
    call get_icrs( 3, t_1_jd, icrs_1(1:6, 1))
    call get_icrs(10, t_1_jd, icrs_1(1:6, 2))
    call get_icrs(11, t_1_jd, icrs_1(1:6, 3))
    ! === 時刻 t2 における地球重心から時刻 t1 における太陽への方向ベクトルの計算
    call calc_unit_vector(icrs_2(1:3, 1), icrs_1(1:3, 3), v_12)
    ! === GCRS 座標系: 光行差の補正（方向ベクトルの Lorentz 変換）
    call conv_lorentz(icrs_2(4:6, 1), v_12, au, dd)
    pos_sun = (/(dd(i) * d_e_s, i=1,3)/)
    ! === ユリウス世紀数、平均黄道傾斜角(ε)、章動(Δψ, Δε) 計算
    call jd2jc(jd_tdb, t)
    call calc_obliquity(t, eps)
    call calc_nutation(t, dpsi, deps)
    ! === 瞬時の真座標系: GCRS への bias & precession（歳差） & nutation（章動）の適用
    call apply_bias_prec_nut(pos_sun, t, eps, dpsi, deps, pos_sun_bpn)
    ! === 座標変換
    call rect2pol(pos_sun_bpn, apos_eq)
    call rect_eq2ec(pos_sun_bpn, eps, rect_ec)
    call rect2pol(rect_ec, apos_ec)
    ! === 視半径／（地平）視差計算
    rad_para(1) = asin(r_s / (apos_eq(3) * au))
    rad_para(1) = rad_para(1) * 180.0_DP * 3600.0_DP / PI
    rad_para(2) = asin(r_e / (apos_eq(3) * au))
    rad_para(2) = rad_para(2) * 180.0_DP * 3600.0_DP / PI
  end subroutine calc_sun

  ! 月の視位置計算
  !
  ! :param(in)  real(8)       jd_tdb: Julian Day for TDB
  ! :param(in)  real(8) icrs_2(6, 3): t2 における ICRS 座標
  ! :param(in)  real(8)        d_e_m: 地球と月の距離
  ! :param(in)  real(8)          r_e: 地球の半径
  ! :param(in)  real(8)          r_m: 太陽の半径
  ! :param(in)  real(8)           au: AU from binary data
  ! :param(out) real(8)   apos_eq(3): (視赤経, 視赤緯, 地心距離)
  ! :param(out) real(8)   apos_ec(3): (視黄経, 視黄緯, 地心距離)
  ! :param(out) real(8)  rad_para(2): (視半径, 視差)
  subroutine calc_moon(jd_tdb, icrs_2, d_e_m, r_e, r_m, au, apos_eq, apos_ec, rad_para)
    implicit none
    real(DP),     intent(in) :: jd_tdb, icrs_2(6, 3), d_e_m, r_e, r_m, au
    real(DP),     intent(out) :: apos_eq(3), apos_ec(3), rad_para(2)
    integer(SP) :: i
    real(DP)    :: t_1_jd, icrs_1(6, 3), v_12(3), dd(3), t
    real(DP)    :: pos_moon(3), pos_moon_bpn(3), eps, dpsi, deps, rect_ec(3)

    ! === 月が光を発した時刻 t1(JD) の計算
    call calc_t1(10, jd_tdb, icrs_2, au, t_1_jd)
    ! === 時刻 t1(= TDB) におけるの地球・月・太陽の位置・速度（ICRS 座標）の計算
    call get_icrs( 3, t_1_jd, icrs_1(1:6, 1))
    call get_icrs(10, t_1_jd, icrs_1(1:6, 2))
    call get_icrs(11, t_1_jd, icrs_1(1:6, 3))
    ! === 時刻 t2 における地球重心から時刻 t1 における月への方向ベクトルの計算
    call calc_unit_vector(icrs_2(1:3, 1), icrs_1(1:3, 2), v_12)
    ! === GCRS 座標系: 光行差の補正（方向ベクトルの Lorentz 変換）
    call conv_lorentz(icrs_2(4:6, 1), v_12, au, dd)
    pos_moon = (/(dd(i) * d_e_m, i=1,3)/)
    ! === ユリウス世紀数、平均黄道傾斜角(ε)、章動(Δψ, Δε) 計算
    call jd2jc(jd_tdb, t)
    call calc_obliquity(t, eps)
    call calc_nutation(t, dpsi, deps)
    ! === 瞬時の真座標系: GCRS への bias & precession（歳差） & nutation（章動）の適用
    call apply_bias_prec_nut(pos_moon, t, eps, dpsi, deps, pos_moon_bpn)
    ! === 座標変換
    call rect2pol(pos_moon_bpn, apos_eq)
    call rect_eq2ec(pos_moon_bpn, eps, rect_ec)
    call rect2pol(rect_ec, apos_ec)
    ! === 視半径／（地平）視差計算
    rad_para(1) = asin(r_m / (apos_eq(3) * au))
    rad_para(1) = rad_para(1) * 180.0_DP * 3600.0_DP / PI
    rad_para(2) = asin(r_e / (apos_eq(3) * au))
    rad_para(2) = rad_para(2) * 180.0_DP * 3600.0_DP / PI
  end subroutine calc_moon
end module apos

