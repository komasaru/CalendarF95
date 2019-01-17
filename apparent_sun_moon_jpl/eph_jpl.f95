!****************************************************
! Modules for JPL ephemeris
!
! date          name            version
! 2018.10.21    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module eph_jpl
  use const
  implicit none
  private
  public :: get_bin, get_list, calc_rrd
  type, public :: t_bin
    character(84) :: ttl(3)
    character(6)  :: cnam(1000)
    integer(SP)   :: ncon, ipt(3, 13), numde
    real(DP)      :: ss(3), au, emrat, cval(1000), coeff(1500)
  end type t_bin

contains
  ! =============================
  ! Private subroutines/functions
  ! =============================

  ! 補間
  ! * 使用するチェビシェフ多項式の係数は、
  ! * 天体番号が 1 〜 13 の場合は、 x, y, z の位置・速度（6要素）、
  !   天体番号が 14 の場合は、 Δψ, Δε の角位置・角速度（4要素）、
  !   天体番号が 15 の場合は、 φ, θ, ψ の角位置・角速度（6要素）。
  ! * 天体番号が 12 の場合は、 x, y, z の位置・速度の値は全て 0.0 とする。
  !
  ! :param(in) integer(4)          a: 天体番号
  ! :param(in) real(8)            jd: Julian Day
  ! :param(in) real(8)        jds(2): 対象区間の開始・終了 JD
  ! :param(in) real(8)         ss(3): SS of header
  ! :param(in) real(8)            au: AU of header
  ! :param(in) integer(4) ipt(3, 13): IPT of header
  ! :param(in) real(8)   coeff(1500): 係数一覧
  ! :return    real(8)         pv(6): 位置(x,y,z), 速度(x,y,z)
  function interpolate(a, jd, jds, ss, au, ipt, coeff) result(pv)
    implicit none
    integer(SP), intent(in) :: a, ipt(3, 13)
    real(DP),    intent(in) :: jd, jds(2), ss(3), au, coeff(1500)
    real(DP) :: pv(6)  ! return values
    integer(SP) :: i, j, k, n, idx, idx_s
    real(DP)    :: tc, val
    real(DP), allocatable :: ps(:), vs(:)

    ! 返却値初期化
    pv = (/(0.0_DP, i=1,6)/)

    ! 時刻の正規化（チェビシェフ多項式用）
    call norm_time(a, jd, jds(1), ss(3), ipt, tc, idx_s)

    ! 要素数
    if (a == 14) then
      n = 2
    else
      n = 3
    end if

    ! インデックス
    if (a > 13) then
      idx = a - 2
    else
      idx = a
    end if

    ! 位置・速度計算用配列アロケート
    allocate(ps(ipt(2, idx)))
    allocate(vs(ipt(2, idx)))

    ! 位置計算
    ps(1:2) = (/1.0_DP, tc/)
    do i = 3, ipt(2, idx)
      ps(i) = 2.0_DP * tc * ps(i - 1) - ps(i - 2)
    end do
    do i = 1, n
      val = 0.0_DP
      do j = 1, ipt(2, idx)
        k = ipt(1, idx) + idx_s * ipt(2, idx) * n &
          & + ipt(2, idx) * (i - 1) + (j - 1)
        val = val + coeff(k) * ps(j)
      end do
      if (.not. KM  .and. a < 14) val = val / au
      pv(i) = val
    end do

    ! 速度計算
    vs(1:3) = (/0.0_DP, 1.0_DP, 4.0_DP * tc/)
    do i = 4, ipt(2, idx)
      vs(i) = 2.0_DP * tc * vs(i - 1) + 2.0_DP * ps(i - 1) - vs(i - 2)
    end do
    do i = 1, n
      val = 0.0_DP
      do j = 1, ipt(2, idx)
        k = ipt(1, idx) + idx_s * ipt(2, idx) * n &
          & + ipt(2, idx) * (i - 1) + (j - 1)
        val = val + coeff(k) * vs(j) * 2.0_DP * ipt(3, idx) / ss(3)
      end do
      if (a < 14) then
        if (KM) then
          val = val / 86400.0_DP
        else
          val = val / au
        end if
      end if
      pv(i + 3) = val
    end do

    ! 位置・速度計算用配列デアロケート
    deallocate(ps)
    deallocate(vs)
  end function interpolate

  ! チェビシェフ多項式用に時刻を正規化、サブ区間のインデックス算出
  !
  ! :param(in)  integer(4)          a: 天体番号
  ! :param(in)  real(8)            jd: Julian Day
  ! :param(in)  real(8)         jd_st: 対象区間の開始 JD
  ! :param(in)  real(8)          ss_3: SS(3) of header
  ! :param(in)  integer(4) ipt(3, 13): IPT of header
  ! :param(out) real(8)            tc: チェビシェフ時間
  ! :param(out) integer(4)        idx: サブ区間のインデックス
  subroutine norm_time(a, jd, jd_st, ss_3, ipt, tc, idx)
    implicit none
    integer(SP), intent(in)  :: a, ipt(3, 13)
    real(DP),    intent(in)  :: jd, jd_st, ss_3
    integer(SP), intent(out) :: idx
    real(DP),    intent(out) :: tc
    real(DP) :: tmp

    if (a > 13) then
      idx = a - 2
    else
      idx = a
    end if
    tc = (jd - jd_st) / ss_3
    tmp = tc * ipt(3, idx)
    idx = int(tmp - int(tc))  ! サブ区間のインデックス
    tc = (mod(tmp, 1.0_DP) + int(tc)) * 2.0_DP - 1.0_DP  ! チェビシェフ時間
  end subroutine norm_time

  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Binary record getting
  ! * cnam(1:400) と cnam(401:), ipt(3, 1: 12) と ipt(3, 13) は
  !   連続していないので注意！
  !
  ! :param(in)  real(8)      jd: Julian Day
  ! :param(out) type(t_bin) bin: バイナリデータ
  subroutine get_bin(jd, bin)
    implicit none
    real(DP),    intent(in)  :: jd
    type(t_bin), intent(out) :: bin
    integer(SP) :: ios, i, j, idx
    integer(SP) :: n_coeff = KSIZE / 2

    open (unit   = UID,           &
      &   iostat = ios,           &
      &   file   = F_BIN,         &
      &   form   = "unformatted", &
      &   access = "direct",      &
      &   recl   = NRECL * KSIZE, &
      &   status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_BIN
      stop
    end if

    ! ヘッダ: 1レコード目
    read (UID, rec=1) &
      & bin%ttl, (bin%cnam(i),i=1,400), bin%ss, bin%ncon, &
      & bin%au, bin%emrat, ((bin%ipt(i,j),i=1,3),j=1,12), &
      & bin%numde, (bin%ipt(i,13),i=1,3), &
      & (bin%cnam(i),i=401,bin%ncon)
    ! ヘッダ: 2レコード目
    read (UID, rec=2) bin%cval
    ! 係数の読み込みインデックス計算
    idx = int((jd - bin%ss(1)) / bin%ss(3))
    ! 係数
    read (UID, rec=idx+3) (bin%coeff(i),i=1,n_coeff)

    close(UID)
  end subroutine get_bin

  ! 計算対象フラグ一覧取得
  ! * チェビシェフ多項式による計算が必要な天体の一覧をインスタンス変数
  !   （リスト） self.lists に設定する
  ! * 配列の並び順（係数データの並び順から「太陽」を除外した13個）
  !   list = (/
  !     水星, 金星, 地球 - 月の重心, 火星, 木星, 土星, 天王星, 海王星,
  !     冥王星, 月（地心）, 地球の章動, 月の秤動
  !   /)
  !
  ! :param(in)    integer(4)          t: 対象天体番号
  ! :param(in)    integer(4)          c: 基準天体番号
  ! :param(in)    integer(4) ipt(3, 13): IPT of header
  ! :param(inout) integer(4)   list(12): 計算対象フラグ
  subroutine get_list(t, c, ipt, list)
    implicit none
    integer(SP), intent(in)    :: t, c, ipt(3, 13)
    integer(SP), intent(inout) :: list(12)

    if (t == 14) then
      if (ipt(2, 12) > 0) list(11) = 1
      return
    end if
    if (t == 15) then
      if (ipt(2, 13) > 0) list(12) = 1
    end if
    if (t <= 10) list( t) = 1
    if (t == 10) list( 3) = 1
    if (t ==  3) list(10) = 1
    if (t == 13) list( 3) = 1
    if (c <= 10) list( c) = 1
    if (c == 10) list( 3) = 1
    if (c ==  3) list(10) = 1
    if (c == 13) list( 3) = 1
  end subroutine get_list

  ! 位置・速度計算
  !
  ! :param(in)  integer(4)          t: 対象天体番号
  ! :param(in)  integer(4)          c: 基準天体番号
  ! :param(in)  real(8)            jd: Julian Day
  ! :param(in)  real(8)        jds(2): 対象区間の開始・終了 JD
  ! :param(in)  type(t_bin)       bin: バイナリデータ
  ! :param(in)  integer(4)   list(12): 計算対象フラグ一覧
  ! :param(out) real(8)        rrd(6): 位置(x, y, z), 速度(x, y, z)
  subroutine calc_rrd(t, c, jd, jds, bin, list, rrd)
    implicit none
    integer(SP), intent(in)  :: t, c, list(12)
    real(dp),    intent(in)  :: jd, jds(2)
    type(t_bin), intent(in)  :: bin
    real(DP),    intent(out) :: rrd(6)
    integer(SP) :: i, j
    real(DP)    :: pv_sun(6), p_nut(6)

    ! 位置・角度データ配列
    real(DP) :: pv(6,11) = reshape((/((0.0_DP, i=1,6), j=1,11)/), shape(pv))
    ! 位置・角度データ配列（対象 - 基準 算出用）
    real(DP) :: pv_2(6,13) = reshape((/((0.0_DP, i=1,6), j=1,13)/), shape(pv_2))

    ! 補間
    ! （11:太陽）
    pv_sun(1:6) = interpolate(11, jd, jds, bin%ss, bin%au, bin%ipt, bin%coeff)
    ! （1:水星〜10:月）
    do i = 1, 10
      if (list(i) == 0) cycle
      pv(1:6, i) = interpolate(i, jd, jds, bin%ss, bin%au, bin%ipt, bin%coeff)
      if (i > 9) cycle
    end do
    ! （14:地球の章動）
    if (list(11) > 0 .and. bin%ipt(2, 12) > 0) then
      p_nut(1:6) = interpolate(14, jd, jds, bin%ss, bin%au, bin%ipt, bin%coeff)
    end if
    ! （15:月の秤動）
    if (list(12) > 0 .and. bin%ipt(2, 13) > 0) then
      pv(1:6, 11) = interpolate(15, jd, jds, bin%ss, bin%au, bin%ipt, bin%coeff)
    end if

    ! 対象天体と基準天体の差
    if (t == 14) then
      if (bin%ipt(2, 12) > 0) rrd(1:6) = p_nut(1:6)
    else if (t == 15) then
      if (bin%ipt(2, 13) > 0) rrd(1:6) = pv(1:6, 11)
    else
      pv_2(1:6, 1:10) = pv(1:6, 1:10)
      if (t == 11 .or. c == 11) pv_2(1:6, 11) = pv_sun(1:6)
      if (t == 12 .or. c == 12) pv_2(1:6, 12) = 0.0_DP
      if (t == 13 .or. c == 13) pv_2(1:6, 13) = pv(1:6, 3)
      if (t * c == 30 .or. t + c == 13) then
        pv_2(1:6, 3) = 0.0_DP
      else
        if (list(3) /= 0) then
          pv_2(1:6, 3) = pv(1:6, 3) - pv(1:6, 10) / (1.0_DP + bin%emrat)
        end if
        if (list(10) /= 0) then
          pv_2(1:6, 10) = pv_2(1:6, 3) + pv(1:6, 10)
        end if
      end if
      rrd(1:6) = pv_2(1:6, t) - pv_2(1:6, c)
    end if
  end subroutine calc_rrd
end module eph_jpl

