!****************************************************
! バイアス・歳差・章動適用
!
!   Date          Author          Version
!   2018.10.24    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数 : X Y Z [TT]
!        * X, Y, Z は元の座標（各20桁以内）
!        * TT は 地球時（省略可）
!          書式：YYYYMMDDHHMMSSUUUUUU
!          無指定なら現在(システム日時)を TT とみなす。
!****************************************************
!
program bpn_rotation
  use const
  use time
  use eph_bpn
  implicit none
  real(DP)      :: crd(3)
  type(t_time) :: tt     ! 地球時
  real(DP)     :: jd, t  ! TT に対する Julian Day, Julian Century Number
  real(DP)     :: eps, dpsi, deps  ! 平均黄道傾斜角(ε)、章動(Δψ, Δε)
  real(DP)     :: crd_b(3), crd_bp(3), crd_bpn(3)
  real(DP)     :: crd_p(3), crd_pn(3), crd_n(3)

  ! コマンドライン引数取得
  call get_arg(crd, tt)
  if (tt%year == 0) stop

  ! ユリウス世紀数、平均黄道傾斜角(ε)、章動(Δψ, Δε) 計算
  call gc2jd(tt, jd)
  call jd2jc(jd, t)
  call calc_obliquity(t, eps)
  call calc_nutation(t, dpsi, deps)

  ! バイアス・歳差・章動の適用
  call apply_bias(crd, crd_b)
  call apply_prec(crd_b, t, eps, crd_p)
  call apply_nut(crd_p, eps, dpsi, deps, crd_n)
  call apply_bias_prec(crd, t, eps, crd_bp)
  call apply_bias_prec_nut(crd, t, eps, dpsi, deps, crd_bpn)
  call apply_prec_nut(crd_b, t, eps, dpsi, deps, crd_pn)

  ! 結果出力
  print '("TDB: ", A)',           date_fmt(tt)
  print '(" JD: ", F18.10, " (days)")',     jd
  print '(" JC: ", F18.10, " (centuries)")', t
  print '("EPS: ", F21.18)',               eps
  print *
  print '("* 元の GCRS 座標:", /, "  [", 3F20.16, "]")', crd
  print '("  バイアス適用:",   /, "  [", 3F20.16, "]")', crd_b
  print '("  歳差適用:",       /, "  [", 3F20.16, "]")', crd_p
  print '("  章動適用:",       /, "  [", 3F20.16, "]")', crd_n
  print '("* 元の GCRS 座標にバイアス＆歳差同時適用:", /,' // &
      & ' "  [", 3F20.16, "]")', crd_bp
  print '("* 元の GCRS 座標にバイアス＆歳差＆章動同時適用:", /,' // &
      & ' "  [", 3F20.16, "]")', crd_bpn
  print '("* 元の GCRS 座標にバイアス適用後、歳差＆章動同時適用:", /,' // &
      & ' "  [", 3F20.16, "]")', crd_pn

  stop
contains
  ! コマンドライン引数取得
  ! * X, Y, Z は実数形式（必須、各20桁以内）
  ! * TT は YYYYMMDDHHMMSSUUUUUU 形式（省略可）
  ! * TT が20桁超入力された場合は、21桁目以降の部分は切り捨てる
  ! * TT 無入力なら、システム日付を TT とみなす
  ! * 日時の整合性チェックは行わない
  !
  ! :param(inout) real(8)  crd(3)
  ! :param(inout) type(t_time) tt
  subroutine get_arg(crd, tt)
    implicit none
    real(DP),     intent(inout) :: crd(3)
    type(t_time), intent(inout) :: tt
    character(20) :: gc
    character(20) :: c(3)
    integer(SP)  :: i, dt(8)

    if (iargc() < 3) then
      crd = (/(0.0_DP, i=1,3)/)
      return
    end if
    do i = 1, 3
      call getarg(i, c(i))
      read (c(i), *) crd(i)
    end do
    if (iargc() == 3) then
      call date_and_time(values=dt)
      tt = t_time(dt(1), dt(2), dt(3), &
        & dt(5), dt(6), dt(7), dt(8) * 1000)
    else
      call getarg(4, gc)
      if (len(trim(gc)) /= 20) then
        print *, "Format: YYYYMMDDHHMMSSUUUUUU"
        return
      end if
      read (gc, FMT_DT_0) tt
    end if
  end subroutine get_arg
end program bpn_rotation

