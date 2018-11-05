!****************************************************
! カレンダ一覧(CSV 出力; DB 登録用)
! * 出力項目
!   年, 月, 日, ユリウス日, 曜日, 祝日, 黄経(太陽),
!   黄経(月), 月齢, 旧暦(年), 旧暦(閏月フラグ),
!   旧暦(月), 旧暦(日), 六曜, 干支(日), 二十四節気,
!   雑節1, 雑節2, 節句
!   （曜日, 祝日, 六曜, 干支(日), 二十四節気, 雑節,
!     節句を表す数字については const モジュールを参照
!     のこと）
!
!   Date          Author          Version
!   2018.11.02    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
!****************************************************
!
program jpl_csv_calendar
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS
  use time,  only : is_leap
  implicit none
  ! 定数
  character(*), parameter :: F_CSV      = "csv/calendar.csv"
  character(*), parameter :: F_CSV_K    = "csv/kokei.csv"
  character(*), parameter :: F_CSV_S    = "csv/sekki_24.csv"
  character(*), parameter :: F_CSV_Z    = "csv/zassetsu.csv"
  character(*), parameter :: F_CSV_H    = "csv/holiday.csv"
  character(*), parameter :: F_CSV_E    = "csv/etc.csv"
  character(*), parameter :: F_CSV_O    = "csv/oc.csv"
  integer(SP),  parameter :: UID_CSV    = 11  ! カレンダCSV用
  integer(SP),  parameter :: UID_CSV_IN = 12  ! 各種読み込みCSV用
  ! 構造型定義
  type :: t_kokei                      ! 黄経
    real(DP)    :: sun      = 0.0_DP   ! (太陽)
    real(DP)    :: moon     = 0.0_DP   ! (月)
  end type t_kokei
  type :: t_zassetsu                   ! 雑節
    integer(SP) :: z_1      = 99       ! (1)
    integer(SP) :: z_2      = 99       ! (2)
  end type t_zassetsu
  type :: t_etc                        ! その他
    real(DP)    :: jd       = 0.0_DP   ! (JD)
    integer(SP) :: yobi     = 9        ! (曜日)
    integer(SP) :: kanshi   = 99       ! (干支)
    real(DP)    :: moon_age = 0.0_DP   ! (月齢)
  end type t_etc
  type :: t_oc                         ! 旧暦
    integer(SP) :: year     = 0        ! (年)
    logical     :: leap     = .false.  ! (閏月フラグ)
    integer(SP) :: month    = 0        ! (月)
    integer(SP) :: day      = 0        ! (日)
    integer(SP) :: rokuyo   = 9        ! (六曜)
  end type t_oc
  ! 変数
  type(t_kokei)    :: kokeis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 黄経（太陽、月）
  integer(SP)      :: sekkis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 二十四節気
  type(t_zassetsu) :: zassetsus(Y_MAX - Y_MIN + 3, 12, 31)  ! 雑節
  integer(SP)      :: holidays( Y_MAX - Y_MIN + 3, 12, 31)  ! 祝日
  type(t_etc)      :: etcs(     Y_MAX - Y_MIN + 3, 12, 31)  ! その他
  type(t_oc)       :: ocs(      Y_MAX - Y_MIN + 3, 12, 31)  ! 旧暦
  integer(SP)      :: y, m, d, ios, days_m, sekku, flag_leap

  ! 各種CSV読み込み)
  call get_kokeis(kokeis)        ! 黄経（太陽、月）
  call get_sekkis(sekkis)        ! 二十四節気
  call get_zassetsus(zassetsus)  ! 雑節
  call get_holidays(holidays)    ! 祝日
  call get_etcs(etcs)            ! その他
  call get_ocs(ocs)              ! 旧暦

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
        sekku = get_sekku(m, d)
        flag_leap = 0
        if (ocs(y_idx(y), m, d)%leap) flag_leap = 1
        print '("* ", I4, "-", I0.2, "-", I0.2, X, &
            & F11.3, X, I1, X, I2, X, F12.8, X, F12.8, X, &
            & F11.8, X, I4, X, L1, X, I2, X, I2, X, &
            & I1, X, I2, X, I3, X, I2, X, I2, X, I1)', &
            & y, m, d, &
            & etcs(y_idx(y), m, d)%jd, etcs(y_idx(y), m, d)%yobi, &
            & holidays(y_idx(y), m, d), &
            & kokeis(y_idx(y), m, d)%sun, kokeis(y_idx(y), m, d)%moon, &
            & etcs(y_idx(y), m, d)%moon_age, &
            & ocs(y_idx(y), m, d), etcs(y_idx(y), m, d)%kanshi, &
            & sekkis(y_idx(y), m, d), &
            & zassetsus(y_idx(y), m, d), sekku
        write (UID_CSV, &
            & '(I4, ",", I2, ",", I2, ",", &
            & F11.3, ",", I1, ",", I2, ",", F12.8, ",", F12.8, ",", &
            & F11.8, ",", I4, ",", I1, ",", I2, ",", I2, ",", &
            & I1, ",", I2, ",", I3, ",", I2, ",", I2, ",", I1)') &
            & y, m, d, &
            & etcs(y_idx(y), m, d)%jd, etcs(y_idx(y), m, d)%yobi, &
            & holidays(y_idx(y), m, d), &
            & kokeis(y_idx(y), m, d)%sun, kokeis(y_idx(y), m, d)%moon, &
            & etcs(y_idx(y), m, d)%moon_age, &
            & ocs(y_idx(y), m, d)%year, flag_leap, &
            & ocs(y_idx(y), m, d)%month, ocs(y_idx(y), m, d)%day, &
            & ocs(y_idx(y), m, d)%rokuyo, etcs(y_idx(y), m, d)%kanshi, &
            & sekkis(y_idx(y), m, d), &
            & zassetsus(y_idx(y), m, d), sekku
      end do
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  stop
contains
  ! 黄経（太陽・月）一覧取得(CSV読み込み)
  !
  ! :param(out) type(t_kokei) kokeis(202, 12, 31): 太陽黄経一覧
  subroutine get_kokeis(kokeis)
    implicit none
    type(t_kokei), intent(out) :: kokeis(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    integer(SP)   :: ios, y, m, d
    real(DP)      :: k_s, k_m

    ! 黄経一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_K,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_K
      stop
    end if

    ! 黄経一覧 CSV ファイル READ
    do
      read (UID_CSV_IN, *, iostat = ios) y, m, d, k_s, k_m
      if (ios /= 0) exit
      kokeis(y_idx(y), m, d) = t_kokei(k_s, k_m)
    end do

    ! 黄経一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_kokeis

  ! 二十四節気一覧取得(CSV読み込み)
  !
  ! :param(out) integer(4) sekkis(202, 12, 31): 二十四節気一覧
  subroutine get_sekkis(sekkis)
    implicit none
    integer(SP), intent(out) :: sekkis(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    character(19) :: jst
    integer(SP)   :: i, j, k, ios, y, m, d, kokei

    ! 配列初期化(黄経 0 度と混同しないよう 999 で初期化)
    sekkis = 999

    ! 二十四節気一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_S,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_S
      stop
    end if

    ! 二十四節気一覧 CSV ファイル READ
    do
      read (UID_CSV_IN, *, iostat = ios) y, m, d, kokei, jst
      if (ios /= 0) exit
      sekkis(y_idx(y), m, d) = kokei
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_sekkis

  ! 雑節一覧取得(CSV読み込み)
  !
  ! :param(out) type(t_zassetsu) zassetsus(202, 12, 31): 雑節一覧
  subroutine get_zassetsus(zassetsus)
    implicit none
    type(t_zassetsu), intent(out) :: zassetsus(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    integer(SP)   :: ios, y, m, d, z_1, z_2

    ! 雑節一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_Z,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_Z
      stop
    end if

    ! 雑節一覧 CSV ファイル READ
    do
      read (UID_CSV_IN, *, iostat = ios) y, m, d, z_1, z_2
      if (ios /= 0) exit
      zassetsus(y_idx(y), m, d) = t_zassetsu(z_1, z_2)
    end do

    ! 雑節一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_zassetsus

  ! 祝日一覧取得(CSV読み込み)
  !
  ! :param(out) integer(4) holidays(202, 12, 31): 祝日一覧
  subroutine get_holidays(holidays)
    implicit none
    integer(SP), intent(out) :: holidays(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    character(19) :: jst
    integer(SP)   :: i, j, k, ios, y, m, d, holiday

    ! 配列初期化(「元日(0)」と混同しないよう 99 で初期化)
    holidays = 99

    ! 祝日一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_H,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_H
      stop
    end if

    ! 祝日一覧 CSV ファイル READ
    do
      read (UID_CSV_IN, *, iostat = ios) y, m, d, holiday
      if (ios /= 0) exit
      holidays(y_idx(y), m, d) = holiday
    end do

    ! 祝日一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_holidays

  ! その他一覧取得(CSV読み込み)
  !
  ! :param(out) type(t_etc) etcs(202, 12, 31): その他一覧
  subroutine get_etcs(etcs)
    implicit none
    type(t_etc), intent(out) :: etcs(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    integer(SP)   :: ios, y, m, d, yobi, kanshi
    real(DP)      :: jd, moon_age

    ! その他一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_E,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_E
      stop
    end if

    ! その他一覧 CSV ファイル READ
    do
      read (UID_CSV_IN, *, iostat = ios) y, m, d, jd, yobi, kanshi, moon_age
      if (ios /= 0) exit
      etcs(y_idx(y), m, d) = t_etc(jd, yobi, kanshi, moon_age)
    end do

    ! その他一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_etcs

  ! 旧暦一覧取得(CSV読み込み)
  !
  ! :param(out) type(t_oc) os(202, 12, 31): 旧暦一覧
  subroutine get_ocs(ocs)
    implicit none
    type(t_oc), intent(out) :: ocs(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    integer(SP)   :: ios, y, m, d, year, leap, month, day, rokuyo
    logical       :: flag_leap

    ! 旧暦一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_IN,  &
        & iostat = ios,         &
        & file   = F_CSV_O,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_CSV_O
      stop
    end if

    ! 旧暦一覧 CSV ファイル READ
    do
      flag_leap = .false.
      read (UID_CSV_IN, *, iostat = ios) &
        & y, m, d, year, leap, month, day, rokuyo
      if (ios /= 0) exit
      if (leap == 1) flag_leap = .true.
      ocs(y_idx(y), m, d) = t_oc(year, flag_leap, month, day, rokuyo)
    end do

    ! 旧暦一覧 CSV ファイル CLOSE
    close(UID_CSV_IN)
  end subroutine get_ocs

  ! 読み込み配列用のインデックス（年）
  !
  ! :param(in) integer(4) y
  ! :return    integer(4) y_idx
  integer(SP) function y_idx(y)
    implicit none
    integer(SP), intent(in) :: y

    y_idx = y - Y_MIN + 2
  end function y_idx

  ! 節句取得
  !
  ! :param(in) integer(SP)     m: 月
  ! :param(in) integer(SP)     d: 日
  ! :return    integer(SP) sekku: 0(人日), 1(上巳), 2(端午), 3(七夕), 4(重陽)
  !                               9(該当なし)
  function get_sekku(m, d) result(sekku)
    implicit none
    integer(SP), intent(in) :: m, d
    integer(SP) :: sekku

    sekku = 9
    if (m == 1 .and. d == 7) then
      sekku = 0
    else if (m == 3 .and. d == 3) then
      sekku = 1
    else if (m == 5 .and. d == 5) then
      sekku = 2
    else if (m == 7 .and. d == 7) then
      sekku = 3
    else if (m == 9 .and. d == 9) then
      sekku = 4
    end if
  end function get_sekku
end program jpl_csv_calendar

