!****************************************************
! カレンダ一覧(CSV 出力; 日本語版)
! * jpl_csv_calendar のコード部分を日本語化したもの
! * 祝日マスタの名称は全角10文字(30byte)以下を想定
! * 出力項目
!   年, 月, 日, ユリウス日, 曜日, 祝日, 黄経(太陽),
!   黄経(月), 月齢, 旧暦(年), 旧暦(閏月フラグ),
!   旧暦(月), 旧暦(日), 六曜, 干支(日), 二十四節気,
!   雑節1, 雑節2, 節句
!
!   Date          Author          Version
!   2018.11.02    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
!****************************************************
!
program jpl_csv_calendar_j
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS, &
                  & YOBI, ROKUYO, KANSHI, SEKKI_24, ZASSETSU, SEKKU
  use time,  only : is_leap
  implicit none
  ! 定数
  character(*), parameter :: F_CSV      = "csv/calendar_j.csv"
  character(*), parameter :: F_CSV_K    = "csv/kokei.csv"
  character(*), parameter :: F_CSV_S    = "csv/sekki_24.csv"
  character(*), parameter :: F_CSV_Z    = "csv/zassetsu.csv"
  character(*), parameter :: F_CSV_H    = "csv/holiday.csv"
  character(*), parameter :: F_CSV_E    = "csv/etc.csv"
  character(*), parameter :: F_CSV_O    = "csv/oc.csv"
  character(*), parameter :: F_TXT_H    = "HOLIDAY.txt"
  integer(SP),  parameter :: UID_CSV    = 11  ! カレンダCSV用
  integer(SP),  parameter :: UID_CSV_IN = 12  ! 各種読み込みCSV用
  integer(SP),  parameter :: UID_TXT_H  = 13  ! 祝日マスタTXT用
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
  character(30)    :: mst_h(0:99)      ! 祝日マスタ
  type(t_kokei)    :: kokeis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 黄経（太陽、月）
  integer(SP)      :: sekkis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 二十四節気
  type(t_zassetsu) :: zassetsus(Y_MAX - Y_MIN + 3, 12, 31)  ! 雑節
  integer(SP)      :: holidays( Y_MAX - Y_MIN + 3, 12, 31)  ! 祝日
  type(t_etc)      :: etcs(     Y_MAX - Y_MIN + 3, 12, 31)  ! その他
  type(t_oc)       :: ocs(      Y_MAX - Y_MIN + 3, 12, 31)  ! 旧暦
  character(3)     :: leap      ! 閏月文字列
  character(18)    :: z_1, z_2  ! 雑節文字列
  character(6)     :: sekki     ! 二十四節気文字列
  integer(SP)      :: y, m, d, ios, days_m

  ! 祝日マスタデータ読み込み
  call get_mst_holiday(mst_h)

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

  ! CSV ヘッダの書き込み
  write (UID_CSV, '(A)') &
    & "年,月,日,ユリウス通日,曜日,祝日,黄経（太陽）,黄経（月）," // &
    & "月齢,旧暦（年）,旧暦（閏月）,旧暦（月）,旧暦（日）,六曜," // &
    & "干支（日）,二十四節気,雑節1,雑節2,節句"

  ! 計算
  do y = Y_MIN, Y_MAX
    do m = 1, 12
      days_m = DAYS(m)
      if (m == 2 .and. is_leap(y)) days_m = days_m + 1
      do d = 1, days_m
        leap  = ""
        z_1   = ""
        z_2   = ""
        sekki = ""
        if (ocs(y_idx(y), m, d)%leap) leap = "閏"
        if (zassetsus(y_idx(y), m, d)%z_1 /= 99) then
          z_1 = space_zen2han(ZASSETSU(zassetsus(y_idx(y), m, d)%z_1))
        end if
        if (zassetsus(y_idx(y), m, d)%z_2 /= 99) then
          z_2 = space_zen2han(ZASSETSU(zassetsus(y_idx(y), m, d)%z_2))
        end if
        if (sekkis(y_idx(y), m, d) /= 999) then
          sekki = SEKKI_24(sekkis(y_idx(y), m, d) / 15)
        end if
        print '("* ", I4, "-", I0.2, "-", I0.2, ",", &
            & F0.3, ",", A, ",", A, ",", F0.8, ",", F0.8, ",", &
            & F0.8, ",", I4, ",", A, ",", I2, ",", I2, ",", &
            & A, ",", A, ",", A, ",", A, ",", A, ",", A)', &
            & y, m, d, &
            & etcs(y_idx(y), m, d)%jd, YOBI(etcs(y_idx(y), m, d)%yobi), &
            & trim(mst_h(holidays(y_idx(y), m, d))), &
            & kokeis(y_idx(y), m, d)%sun, kokeis(y_idx(y), m, d)%moon, &
            & etcs(y_idx(y), m, d)%moon_age, &
            & ocs(y_idx(y), m, d)%year, &
            & trim(leap), &
            & ocs(y_idx(y), m, d)%month, &
            & ocs(y_idx(y), m, d)%day, &
            & ROKUYO(ocs(y_idx(y), m, d)%rokuyo), &
            & KANSHI(etcs(y_idx(y), m, d)%kanshi), &
            & trim(sekki), &
            & trim(z_1), trim(z_2), &
            & trim(get_sekku(m, d))
        write (UID_CSV, &
            & '(I0, ",", I0, ",", I0, ",", &
            & F0.3, ",", A, ",", A, ",", F0.8, ",", F0.8, ",", &
            & F0.8, ",", I0, ",", A, ",", I0, ",", I0, ",", &
            & A, ",", A, ",", A, ",", A, ",", A, ",", A)') &
            & y, m, d, &
            & etcs(y_idx(y), m, d)%jd, YOBI(etcs(y_idx(y), m, d)%yobi), &
            & trim(mst_h(holidays(y_idx(y), m, d))), &
            & kokeis(y_idx(y), m, d)%sun, kokeis(y_idx(y), m, d)%moon, &
            & etcs(y_idx(y), m, d)%moon_age, &
            & ocs(y_idx(y), m, d)%year, &
            & trim(leap), &
            & ocs(y_idx(y), m, d)%month, &
            & ocs(y_idx(y), m, d)%day, &
            & ROKUYO(ocs(y_idx(y), m, d)%rokuyo), &
            & KANSHI(etcs(y_idx(y), m, d)%kanshi), &
            & trim(sekki), &
            & trim(z_1), trim(z_2), &
            & trim(get_sekku(m, d))
      end do
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  stop
contains
  ! 祝日マスタ取得(TXT読み込み)
  !
  ! :param(out) character(48) mst_h(0:99): 祝日名称一覧
  subroutine get_mst_holiday(mst_h)
    implicit none
    character(*), intent(out) :: mst_h(0:99)
    character(30) :: h_name
    integer(SP)   :: ios, h_id, m, d, kbn, y_s, y_e

    ! 配列初期化
    mst_h = ""

    ! 祝日マスタ TXT ファイル OPEN
    open (unit   = UID_TXT_H,   &
        & iostat = ios,         &
        & file   = F_TXT_H,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_TXT_H
      stop
    end if

    ! 祝日マスタ TXT ファイル READ
    ! * 名称のない項目は半角スペース30個
    do
      read (UID_TXT_H, *, iostat = ios) h_id, m, d, kbn, y_s, y_e, h_name
      if (ios /= 0) exit
      mst_h(h_id) = h_name
    end do

    ! 祝日マスタ TXT ファイル CLOSE
    close(UID_TXT_H)
  end subroutine get_mst_holiday

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
    integer(SP)   :: i, j, k, ios, y, m, d, h_id

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
      read (UID_CSV_IN, *, iostat = ios) y, m, d, h_id
      if (ios /= 0) exit
      holidays(y_idx(y), m, d) = h_id
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
  ! :return    integer(SP) sekku: 人日, 上巳, 端午, 七夕, 重陽
  !                               (該当なし：「」)
  function get_sekku(m, d) result(sekku)
    implicit none
    integer(SP), intent(in) :: m, d
    character(6) :: sekku

    sekku = ""
    if (m == 1 .and. d == 7) then
      sekku = "人日"
    else if (m == 3 .and. d == 3) then
      sekku = "上巳"
    else if (m == 5 .and. d == 5) then
      sekku = "端午"
    else if (m == 7 .and. d == 7) then
      sekku = "七夕"
    else if (m == 9 .and. d == 9) then
      sekku = "重陽"
    end if
  end function get_sekku

  ! 後ろの半角スペースを全角に置換
  ! * 全角文字は3byte
  ! * 最大全角33文字(99byte)に対応
  function space_han2zen(src) result(dst)
    implicit none
    character(*), intent(in) :: src
    character(99) :: dst
    integer(SP) :: i, idx, len_src

    len_src = len(src)
    idx = index(src, " ")
    dst(1:idx-1) = src(1:idx-1)
    do i = idx, len_src, 3
      dst(i:i+3) = "　"
    end do
  end function space_han2zen

  ! 後ろの全角スペースを半角に置換
  ! * 全角文字は3byte
  ! * 最大全角33文字(99byte)に対応
  function space_zen2han(src) result(dst)
    implicit none
    character(*), intent(in) :: src
    character(99) :: dst
    integer(SP) :: i, idx, len_src

    len_src = len(src)
    idx = index(src, "　")
    dst(1:idx-1) = src(1:idx-1)
    do i = idx, len_src
      dst(i:i) = " "
    end do
  end function space_zen2han
end program jpl_csv_calendar_j

