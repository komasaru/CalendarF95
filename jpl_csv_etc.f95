!****************************************************
! その他（曜日、JD、月齢、干支、節句）一覧(CSV出力)
!
!   Date          Author          Version
!   2018.10.30    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time モジュール内で定義
!****************************************************
!
program jpl_etc
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS, JST_D, FMT_DT_2
  use time
  implicit none
  character(*), parameter :: F_CSV     = "csv/etc.csv"
  character(*), parameter :: F_CSV_M   = "csv/moon.csv"
  integer(SP),  parameter :: UID_CSV   = 11  ! その他一覧CSV用
  integer(SP),  parameter :: UID_CSV_M = 12  ! 朔一覧CSV用
  character(34), allocatable :: sakus(:)     ! 朔一覧（各行は文字列として取得）
  character(34) :: tmp(5000)                 ! 朔一覧（一時処理用）
  type(t_time) :: jst
  integer(SP)  :: len_s
  integer(SP)  :: y, m, d, days_m, yobi, kanshi, ios
  real(DP)     :: jd, moon_age

  ! 朔一覧取得(CSV読み込み)
  ! (+朔一覧配列アロケート)
  call get_sakus(tmp, len_s)
  allocate(sakus(len_s))
  sakus = tmp(1:len_s)

  ! 書き込み用 CSV ファイル OPEN
  open (unit   = UID_CSV,     &
      & iostat = ios,         &
      & file   = F_CSV,       &
      & action = "write",     &
      & form   = "formatted", &
      & status = "new")
  if (ios /= 0) then
    print *, "[ERROR] Failed to open file: " // F_CSV
    stop
  end if

  ! 計算
  do y = Y_MIN, Y_MAX
    do m = 1, 12
      days_m = DAYS(m)
      if (m == 2 .and. is_leap(y)) days_m = days_m + 1
      do d = 1, days_m
        jst = t_time(y, m, d, 0, 0, 0, 0)
        call gc2jd(jst, jd)
        jd = jd - JST_D
        yobi   = calc_yobi(jd)
        kanshi = calc_kanshi(jd)
        moon_age = calc_moon_age(len_s, sakus, jst)
        print '("* ", I4, "-", I0.2, "-", I0.2, &
            & " - ", F11.3, ",", I1, ", ", I2, ", ", F11.8)', &
            & y, m, d, jd, yobi, kanshi, moon_age
        write (UID_CSV, &
            & '(I4, ",", I2, ",", I2, ",", &
            &   F11.3, ",", I1, ",", I2, ",", F11.8)') &
            & y, m, d, jd, yobi, kanshi, moon_age
      end do
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  ! 朔一覧配列デアロケート
  deallocate(sakus)

  stop
contains
  ! 朔一覧取得(CSV読み込み)
  ! * 朔望一覧から黄経差が 0 のもの（朔）のみ取得
  !
  ! :param(out) character(34) tmp(5000): 朔一覧（一次処理用）
  ! :param(out) integer(4)        len_s: 件数
  subroutine get_sakus(sakus, len_s)
    implicit none
    character(*), intent(out) :: sakus(5000)
    integer(SP),  intent(out) :: len_s
    character(34) :: buf
    integer(SP)   :: ios

    ! 配列初期化
    sakus = ""

    ! 朔一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_M,   &
        & iostat = ios,         &
        & file   = F_CSV_M,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_M
      stop
    end if

    ! 朔一覧 CSV ファイル READ
    len_s = 0
    do
      read (UID_CSV_M, '(A)', iostat = ios) buf
      if (ios /= 0) exit
      if (buf(1:1) == " ") cycle
      if (buf(12:14) /= "  0") cycle
      len_s = len_s + 1
      sakus(len_s) = buf
    end do

    ! 朔望一覧 CSV ファイル CLOSE
    close(UID_CSV_M)
  end subroutine get_sakus

  ! 曜日計算
  !
  ! :param(in) real(8)      jd: Julian Day
  ! :return    integer(4) yobi: 曜日
  function calc_yobi(jd) result(yobi)
    implicit none
    real(DP), intent(in) :: jd
    integer(SP) :: yobi

    yobi = mod(int(jd) + 2, 7)
  end function calc_yobi

  ! 干支計算
  !
  ! :param(in) real(8)        jd: Julian Day
  ! :return    integer(4) kanshi: 干支
  function calc_kanshi(jd) result(kanshi)
    implicit none
    real(DP), intent(in) :: jd
    integer(SP) :: kanshi

    kanshi = mod(int(jd) - 10, 60)
  end function calc_kanshi

  ! 月齢取得
  !
  ! :param(in) integer(4)      len_s: 朔一覧件数
  ! :param(in) character(*) sakus(*): 朔一覧(各行は文字列)
  ! :param(in) type(t_time)      jst: JST
  ! :return    real(8)      moon_age: 月齢
  function calc_moon_age(len_s, sakus, jst) result(moon_age)
    implicit none
    integer(SP),  intent(in) :: len_s
    character(*), intent(in) :: sakus(*)
    type(t_time), intent(in) :: jst
    real(DP)                 :: moon_age
    character(19) :: gc_t, gc
    integer(SP)  :: i, ye, mo, da, ho, mi, se, us
    real(DP)     :: jd, jd_s

    ! 対象の日時
    write (gc_t, '(I4, "-", I0.2, "-", I0.2, " 12:00:00")') &
      & jst%year, jst%month, jst%day
    call gc2jd(jst, jd)
    jd = jd + 0.5 - JST_D

    ! 逆順ループで取得
    do i = len_s, 1, -1
      gc = sakus(i)(16:34)
      if (gc < gc_t) exit
    end do

    ! 朔の JD
    read (gc( 1: 4), '(I4)') ye
    read (gc( 6: 7), '(I2)') mo
    read (gc( 9:10), '(I2)') da
    read (gc(12:13), '(I2)') ho
    read (gc(15:16), '(I2)') mi
    read (gc(18:19), '(I2)') se
    call gc2jd(t_time(ye, mo, da, ho, mi, se, 0), jd_s)
    jd_s = jd_s - JST_D

    ! 月齢
    moon_age = jd - jd_s
  end function calc_moon_age
end program jpl_etc

