!****************************************************
! グリニジ視恒星時 GAST(= Greenwich Apparent Sidereal Time)等の計算
! : IAU2006 による計算
! 
!   * IAU SOFA(International Astronomical Union, Standards of Fundamental Astronomy)
!     の提供する C ソースコード "gst06.c" 等で実装されているアルゴリズムを使用する。
!   * 参考サイト
!     - [SOFA Library Issue 2016-05-03 for ANSI C: Complete List]
!       (http://www.iausofa.org/2016_0503_C/CompleteList.html)
!     - [USNO Circular 179]
!       (http://aa.usno.navy.mil/publications/docs/Circular_179.php)
!     - [IERS Conventions Center]
!       (http://62.161.69.131/iers/conv2003/conv2003_c5.html)
!   * うるう秒の判定は UTC でなく TT で行っている
!
!   Date          Author          Version
!   2018.10.18    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数 : 日時(TT（地球時）)
!        * 書式：YYYYMMDDHHMMSSUUUUUU
!        * 無指定なら現在(システム日時)を地球時とみなす。
!****************************************************
!
program greenwich_time
  use const
  use delta_t
  use time
  use precession
  use nutation
  use rotation_fw
  use cip_cio
  use greenwich
  implicit none
  type(t_time) :: tt, ut1
  integer(SP)  :: utc_tai, i
  real(DP)     :: jd, jc, dt, jd_ut1
  real(DP)     :: gam_b, phi_b, psi_b, eps_a, fj2
  real(DP)     :: d_psi_ls, d_eps_ls, d_psi_pl, d_eps_pl, d_psi, d_eps
  real(DP)     :: mtx(3, 3), xy(2), s
  real(DP)     :: era, eo, gast, gast_deg, gmst, gmst_deg, ee, ee_deg

  ! コマンドライン引数(TT)取得
  call get_arg(tt)
  if (tt%year == 0) stop

  ! === Time calculation
  call gc2jd(tt, jd)
  call jd2jc(jd, jc)
  call utc2utc_tai(tt, utc_tai)
  call utc2dt(tt, utc_tai, dt)
  call tt2ut1(tt, dt, ut1)
  call gc2jd(ut1, jd_ut1)
  ! === Fukushima-Williams angles for frame bias and precession.
  !     Ref: iauPfw06(date1, date2, &gamb, &phib, &psib, &epsa)
  call pfw_06(jc, gam_b, phi_b, psi_b)
  call obl_06(jc, eps_a)
  ! === Nutation components.
  !     Ref: iauNut06a(date1, date2, &dp, &de)
  ! * Factor correcting for secular variation of J2.
  fj2 = -2.7774e-6_DP * jc
  ! Calculation
  call calc_lunisolar(jc, d_psi_ls, d_eps_ls)
  call calc_planetary(jc, d_psi_pl, d_eps_pl)
  d_psi = d_psi_ls + d_psi_pl
  d_eps = d_eps_ls + d_eps_pl
  ! * Apply P03 adjustments (Wallace & Capitaine, 2006, Eqs.5).
  d_psi = d_psi + d_psi * (0.4697e-6_DP + fj2)
  d_eps = d_eps + d_eps * fj2
  ! === Equinox based nutation x precession x bias matrix.
  !     Ref: iauFw2m(gamb, phib, psib + dp, epsa + de, rnpb)
  call fw2m(gam_b, phi_b, psi_b + d_psi, eps_a + d_eps, mtx)
  ! === Extract CIP coordinates.
  !       Ref: iauBpn2xy(rnpb, &x, &y)
  call bpn2xy(mtx, xy)
  ! === The CIO locator, s.
  !       Ref: iauS06(tta, ttb, x, y)
  call s_06(jc, xy, s)
  !# Greenwich time
  ! === Greenwich apparent sidereal time.
  !     Ref: iauEra00(uta, utb), iauEors(rnpb, s)
  call gw_era(jd_ut1, era)
  call gw_eors(mtx, s, eo)
  call gw_gast(era, eo, gast)
  gast_deg = gast / PI_180
  ! === Greenwich mean sidereal time, IAU 2006.
  !       Ref: iauGmst06(uta, utb, tta, ttb)
  call gw_gmst(era, jc, gmst)
  gmst_deg = gmst / PI_180
  ! === Equation of Equinoxes
  call gw_ee(gast, gmst, ee)
  ee_deg = ee / PI_180

  ! 結果出力
  !print '("  DeltaPsi = ", E22.14e2, " rad")', dpsi
  print '("     TT = ", A)',            date_fmt(tt)
  print '("    UT1 = ", A)',           date_fmt(ut1)
  print '(" JD(TT) = ", F18.10, " (days)")',      jd
  print '("JD(UT1) = ", F18.10, " (days)")',  jd_ut1
  print '("     JC = ", F18.16, " (centuries)")', jc
  print '("     DT = ", F6.3,   " (seconds)")',   dt
  print '(" GAMMA_ = ", F21.18)',              gam_b
  print '("   PHI_ = ", F21.18)',              phi_b
  print '("   PSI_ = ", F21.18)',              psi_b
  print '("  EPS_A = ", F21.18)',              eps_a
  print '("  D_PSI = ", F21.18)',              d_psi
  print '("  D_EPS = ", F21.18)',              d_eps
  print '("  r_mtx = ")'
  do i = 1, 3
    print '(2X3(XF21.18))', mtx(i, :)
  end do
  print '("   x, y = ", (F21.18, X, F21.18))',     xy
  print '("      s = ", F21.18)',                  s
  print '("    ERA = ", F23.18, " (rad)")',      era
  print '("     EO = ", F23.18, " (rad)")',       eo
  print '("   GAST = ", F23.18, " (rad)")',     gast
  print '("        = ", F23.18, " (°)")' , gast_deg
  print '("        = ", A)' ,      deg2hms(gast_deg)
  print '("   GMST = ", F23.18, " (rad)")',     gmst
  print '("        = ", F23.18, " (°)")',  gmst_deg
  print '("        = ", A)' ,      deg2hms(gmst_deg)
  print '("     EE = ", F23.18, " (rad)")',       ee
  print '("        = ", F23.18, " (°)")',    ee_deg
  print '("        = ", A)' ,        deg2hms(ee_deg)

  stop
contains
  ! コマンドライン引数取得
  ! * YYYYMMDDHHMMSS 形式
  ! * 14桁超入力された場合は、15桁目以降の部分は切り捨てる
  ! * コマンドライン引数がなければ、システム日付を TT とする
  ! * 日時の整合性チェックは行わない
  !
  ! :param(inout) type(t_time) tt
  subroutine get_arg(tt)
    implicit none
    type(t_time), intent(inout) :: tt
    character(20) :: gc
    integer(SP)   :: dt(8)

    if (iargc() == 0) then
      call date_and_time(values=dt)
      tt = t_time(dt(1), dt(2), dt(3), &
        & dt(5), dt(6), dt(7), dt(8) * 1000)
    else
      call getarg(1, gc)
      if (len(trim(gc)) /= 14) then
        print *, "Format: YYYYMMDDHHMMSS"
        return
      end if
      read (gc, FMT_DT_0) tt
    end if
  end subroutine get_arg
end program greenwich_time

