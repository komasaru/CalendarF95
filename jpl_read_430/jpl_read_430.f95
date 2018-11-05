!****************************************************
! JPLEPH(JPL の DE430 バイナリデータ)読み込み
! * 対象天体
!      1: 水星            (Mercury)
!      2: 金星            (Venus)
!      3: 地球 - 月の重心 (Earth-Moon barycenter)
!      4: 火星            (Mars)
!      5: 木星            (Jupiter)
!      6: 土星            (Saturn)
!      7: 天王星          (Uranus)
!      8: 海王星          (Neptune)
!      9: 冥王星          (Pluto)
!     10: 月（地心）      (Moon (geocentric))
!     11: 太陽            (Sun)
!     12: 地球の章動(1980年IAUモデル) (Earth Nutations in longitude and obliquity(IAU 1980 model))
!     13: 月の秤動        (Lunar mantle libration)

!   Date          Author          Version
!   2018.10.21    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数
!   [第１] 天体番号（省略可）
!          * 1 〜 13
!          * 省略時は 11 を指定したとみなす
!   [第２] ユリウス日（省略可）
!          * 省略時は現在日時を UTC とみなしたユリウス日
! ---
! * 構造型 type(t_time) は time    モジュール内で定義
! * 構造型 type(t_bin)  は eph_jpl モジュール内で定義
!****************************************************
!
program jpl_read_430
  use const
  use time
  use eph_jpl
  implicit none
  integer(SP) :: a = 11  ! 指定の天体番号（初期値： 11（太陽））
  real(DP)    :: jd      ! 指定のJulian Day
  type(t_bin) :: bin
  integer(SP) :: i, j, n
  real(DP)    :: jds(2)
  ! 対象係数配列
  real(DP), allocatable :: coeff_a(:, :, :)

  ! コマンドライン引数(天体番号、JD)取得
  call get_arg(a, jd)

  ! バイナリデータ取得
  ! * 1レコード目:     ttl - numde
  ! * 2レコード目:     cval
  ! * 3レコード目以降: 係数
  call get_bin(jd, bin)

  ! 対象インデックスの開始／終了 JD
  jds = bin%coeff(1:2)

  ! 対象係数配列のアロケート
  if (a == 12) then
    n = 2
  else
    n = 3
  end if
  allocate(coeff_a(bin%ipt(2, a), n, bin%ipt(3, a)))

  ! 対象係数配列
  coeff_a = reshape( &
    & bin%coeff(bin%ipt(1, a): &
    &   (bin%ipt(1, a)+bin%ipt(2, a)*n*bin%ipt(3, a)-1) &
    & ), shape(coeff_a))

  ! 結果出力
  print '("  Astro No: ", I2)',     a
  print '("Julian Day: ", F18.10)', jd
  print '(/A/)', "*** Header data ***"
  print *, "TTL:"
  do i = 1, 3
    print '(2XA)', trim(bin%ttl(i))
  end do
  print *, "CNAM:"
  print '(8(A8))', bin%cnam(1:bin%ncon)
  print *, "SS:"
  print '(3(F11.1))', bin%ss
  print *, "NCON:"
  print '(I5)', bin%ncon
  print *, "AU:"
  print *, bin%au
  print *, "EMRAT:"
  print *, bin%emrat
  print *, "IPT:"
  do i = 1, 13
    print '(3(I6))', bin%ipt(:, i)
  end do
  print *, "NUMDE:"
  print *, bin%numde
  print *, "NCON:"
  print '(3E24.16)', bin%cval(1:bin%ncon)
  print '(/A)', "*** Coefficients ***"
  print '("jds: ", F18.10XF18.10)', jds
  do i = 1, bin%ipt(3, a)
    do j = 1, n
      print *, ""
      print '(3E24.16)', coeff_a(:, j, i)
    end do
  end do

  ! 対象係数配列のデアロケート
  deallocate(coeff_a)

  stop
contains
  ! コマンドライン引数(天体番号、JD)取得
  ! * 天体番号: 1 〜 13
  ! * JD: 実数形式（整合性チェックは行わない）
  !
  ! :param(inout) integer  a
  ! :param(inout) real(8) jd
  subroutine get_arg(a, jd)
    implicit none
    integer(SP), intent(inout) :: a
    real(DP),    intent(inout) :: jd
    character(20) :: arg_j
    type(t_time)  :: utc
    character(2)  :: arg_a
    integer(SP)   :: dt(8)

    if (iargc() < 2) then
      call date_and_time(values=dt)
      utc = t_time(dt(1), dt(2), dt(3), &
        & dt(5), dt(6), dt(7), dt(8) * 1000)
      call gc2jd(utc, jd)
    end if
    if (iargc() > 0) then
      call getarg(1, arg_a)
      read (arg_a, *) a
    end if
    if (iargc() > 1) then
      call getarg(2, arg_j)
      read (arg_j, *) jd
    end if
  end subroutine get_arg
end program jpl_read_430

