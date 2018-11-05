!****************************************************
! 章動の計算
! : IAU2000A 章動理論(MHB2000, IERS2003)による
!   黄経における章動(Δψ), 黄道傾斜における章動(Δε) の計算
!
! * IAU SOFA(International Astronomical Union, Standards of Fundamental Astronomy)
!   の提供する C ソースコード "nut00a.c" で実装されているアルゴリズムを使用する。
! * 参考サイト
!   - [SOFA Library Issue 2012-03-01 for ANSI C: Complete List](http://www.iausofa.org/2012_0301_C/sofa/)
!   - [USNO Circular 179](http://aa.usno.navy.mil/publications/docs/Circular_179.php)
!   - [IERS Conventions Center](http://62.161.69.131/iers/conv2003/conv2003_c5.html)
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
program nutation_model
  use const
  use nutation
  use time
  implicit none
  type(t_time) :: tt
  real(DP)     :: jd, jc
  real(DP)     :: dpsi_ls, dpsi_pl, dpsi, dpsi_d, dpsi_s
  real(DP)     :: deps_ls, deps_pl, deps, deps_d, deps_s

  ! コマンドライン引数(TT)取得
  call get_arg(tt)
  if (tt%year == 0) stop

  ! 章動計算
  call gc2jd(tt, jd)
  call jd2jc(jd, jc)
  call calc_lunisolar(jc, dpsi_ls, deps_ls)
  call calc_planetary(jc, dpsi_pl, deps_pl)
  dpsi   = dpsi_ls + dpsi_pl
  deps   = deps_ls + deps_pl
  dpsi_d = dpsi   * R2D
  deps_d = deps   * R2D
  dpsi_s = dpsi_d * D2S
  deps_s = deps_d * D2S

  ! 結果出力
  print '("  [", A, " TT]")',           date_fmt(tt)
  print '("  DeltaPsi = ", E22.14e2, " rad")',  dpsi
  print '("           = ", E22.14e2, " °")', dpsi_d
  print '("           = ", E22.14e2, " ″")', dpsi_s
  print '("  DeltaEps = ", E22.14e2, " rad")',  deps
  print '("           = ", E22.14e2, " °")', deps_d
  print '("           = ", E22.14e2, " ″")', deps_s

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
end program nutation_model

