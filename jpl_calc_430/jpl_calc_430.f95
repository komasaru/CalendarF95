!*******************************************************************************
! JPLEPH(JPL の DE430 バイナリデータ)読み込み、座標（位置・速度）を計算
! * 対象天体
!    1: 水星            (Mercury)
!    2: 金星            (Venus)
!    3: 地球            (Earth)
!    4: 火星            (Mars)
!    5: 木星            (Jupiter)
!    6: 土星            (Saturn)
!    7: 天王星          (Uranus)
!    8: 海王星          (Neptune)
!    9: 冥王星          (Pluto)
!   10: 月              (Moon)
!   11: 太陽            (Sun)
!   12: 太陽系重心      (Solar system Barycenter)
!   13: 地球 - 月の重心 (Earth-Moon Barycenter)
!   14: 地球の章動      (Earth Nutations)
!   15: 月の秤動        (Lunar mantle Librations)
!
!   Date          Author          Version
!   2018.10.21    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数
!   [第１] 対象天体番号（必須）
!          * 1 〜 15
!   [第２] 基準天体番号（必須）
!          * 0, 1 〜 13
!            (0 は、対象天体番号が 14, 15 のときのみ)
!   [第３] ユリウス日（省略可）
!          * 省略時は現在日時を UTC とみなしたユリウス日
! ---
! * 構造型 type(t_time) は time    モジュール内で定義
! * 構造型 type(t_bin)  は eph_jpl モジュール内で定義
!*******************************************************************************
!
program jpl_calc_430
  use const
  use time
  use eph_jpl
  implicit none
  integer(SP)   :: t, c  ! 指定の天体番号（対象、基準）
  real(DP)      :: jd    ! 指定のJulian Day
  type(t_bin)   :: bin   ! バイナリデータ用構造型
  integer(SP)   :: i
  real(DP)      :: jds(2)
  ! 計算対象フラグ
  integer(SP)   :: list(12) = (/(0, i=1,12)/)
  ! 算出データ（対象 - 基準）配列（位置(x, y, z)・速度(x, y, z)）
  real(DP)      :: rrd(6) = (/(0.0_DP, i=1,6)/)

  ! コマンドライン引数(天体番号（対象、基準）、JD)取得
  call get_arg(t, c, jd)
  if (t == 0) then
    do i = 1, size(USAGE)
      print *, USAGE(i)
    end do
    stop
  end if

  ! バイナリデータ取得
  ! * 1レコード目:     ttl - numde
  ! * 2レコード目:     cval
  ! * 3レコード目以降: 係数
  call get_bin(jd, bin)

  ! インデックスの開始／終了 JD
  jds = bin%coeff(1:2)

  ! 計算対象フラグ一覧取得
  call get_list(t, c, bin%ipt, list)

  ! 位置・速度計算
  call calc_rrd(t, c, jd, jds, bin, list, rrd)

  ! 結果出力
  print '("  TARGET: ", I2, " (", A, ")")', t, trim(ASTRS(t))
  print '("  CENTER: ", I2, " (", A, ")")', c, trim(ASTRS(c))
  print '("      JD: ", F16.8, " days")', jd
  print '("      AU: ", F11.1, " km" /)', bin%au
  if (t == 14) then
    print '("  Position(Δψ) = ", F32.20, " rad")',     rrd(1)
    print '("  Position(Δε) = ", F32.20, " rad")',     rrd(2)
    print '("  Velocity(Δψ) = ", F32.20, " rad/day")', rrd(4)
    print '("  Velocity(Δε) = ", F32.20, " rad/day")', rrd(5)
  else if (t == 15) then
    print '("  Position(φ) = ",   F32.20, " AU")',      rrd(1)
    print '("  Position(θ) = ",   F32.20, " AU")',      rrd(2)
    print '("  Position(ψ) = ",   F32.20, " AU")',      rrd(3)
    print '("  Velocity(φ) = ",   F32.20, " AU/day")',  rrd(4)
    print '("  Velocity(θ) = ",   F32.20, " AU/day")',  rrd(5)
    print '("  Velocity(ψ) = ",   F32.20, " AU/day")',  rrd(6)
  else
    print '("  Position(x) = ",    F32.20, " AU")',      rrd(1)
    print '("  Position(y) = ",    F32.20, " AU")',      rrd(2)
    print '("  Position(z) = ",    F32.20, " AU")',      rrd(3)
    print '("  Velocity(x) = ",    F32.20, " AU/day")',  rrd(4)
    print '("  Velocity(y) = ",    F32.20, " AU/day")',  rrd(5)
    print '("  Velocity(z) = ",    F32.20, " AU/day")',  rrd(6)
  end if

  stop
contains
  ! コマンドライン引数(天体番号（対象、基準）、JD)取得
  ! * 天体番号: 1 〜 15
  ! * JD: 実数形式（整合性チェックは行わない）
  !
  ! :param(inout) integer  t
  ! :param(inout) integer  c
  ! :param(inout) real(8) jd
  subroutine get_arg(t, c, jd)
    implicit none
    integer(SP), intent(inout) :: t, c
    real(DP),    intent(inout) :: jd
    character(20) :: arg_j
    type(t_time)  :: utc
    character(2)  :: arg_t, arg_c
    integer(SP)   :: dt(8)

    if (iargc() < 2) then
      t = 0
      c = 0
      jd = 0.0_DP
      return
    end if
    call getarg(1, arg_t)
    read (arg_t, *) t
    call getarg(2, arg_c)
    read (arg_c, *) c
    if (iargc() < 3) then
      call date_and_time(values=dt)
      utc = t_time(dt(1), dt(2), dt(3), dt(5), dt(6), dt(7), dt(8))
      call gc2jd(utc, jd)
    else
      call getarg(3, arg_j)
      read (arg_j, *) jd
    end if
    if ((t < 1 .or. c < 0 .or. t > 15 .or. c > 13) .or. &
        (t > 13 .and. c /= 0) .or. (t < 14 .and. c == 0)) then
      t = 0
      c = 0
      jd = 0.0_DP
      return
    end if
  end subroutine get_arg
end program jpl_calc_430

