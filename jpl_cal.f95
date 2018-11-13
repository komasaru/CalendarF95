!*******************************************************************************
! カレンダー
! * 高野氏のプログラムのアルゴリズムを使用。
!   但し、天体の正確な位置データは JPL DE430 から取得
!
!   Date          Author          Version
!   2018.11.05    mk-mode.com     1.00 新規作成
!   2018.11.10    mk-mode.com     1.01 テキストファイル OPEN/READ 時のエラー処理
!   2018.11.13    mk-mode.com     1.02 雑節文字列のスペース全角->半角処理を修正
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: JST（日本標準時）
!       * 書式：YYYYMMDD
!       * 無指定なら現在(システム日時)と判断。
!       * 西暦 1900年〜2099年まで対応
!*******************************************************************************
!
program jpl_cal
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS, JST_D, &
                  & YOBI, ROKUYO, KANSHI, SEKKI_24, ZASSETSU, SEKKU
  use time,  only : t_time, is_leap, is_valid_date
  implicit none
  ! 定数
  character(*), parameter :: F_CSV_K = "csv/kokei.csv"
  character(*), parameter :: F_CSV_S = "csv/sekki_24.csv"
  character(*), parameter :: F_CSV_Z = "csv/zassetsu.csv"
  character(*), parameter :: F_CSV_H = "csv/holiday.csv"
  character(*), parameter :: F_CSV_E = "csv/etc.csv"
  character(*), parameter :: F_CSV_O = "csv/oc.csv"
  character(*), parameter :: F_TXT_H = "HOLIDAY.txt"
  integer(SP),  parameter :: UID_CSV = 11  ! 各種読み込みCSV用
  integer(SP),  parameter :: UID_TXT = 12  ! 祝日マスタTXT用
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
  type(t_time)     :: jst
  character(30)    :: mst_h(0:99)   ! 祝日マスタ
  type(t_kokei)    :: kokeis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 黄経（太陽、月）
  integer(SP)      :: sekkis(   Y_MAX - Y_MIN + 3, 12, 31)  ! 二十四節気
  type(t_zassetsu) :: zassetsus(Y_MAX - Y_MIN + 3, 12, 31)  ! 雑節
  integer(SP)      :: holidays( Y_MAX - Y_MIN + 3, 12, 31)  ! 祝日
  type(t_etc)      :: etcs(     Y_MAX - Y_MIN + 3, 12, 31)  ! その他
  type(t_oc)       :: ocs(      Y_MAX - Y_MIN + 3, 12, 31)  ! 旧暦
  logical          :: stat
  real(DP)         :: jd_jst        ! Julian Day (for JST)
  character(3)     :: s_leap        ! 閏月文字列
  character(18)    :: s_z_1, s_z_2  ! 雑節文字列
  character(6)     :: s_sekki       ! 二十四節気文字列
  character(6)     :: s_sekku       ! 節句文字列
  integer(SP)    :: i_z_1, i_z_2, i_sekki, i_sekku

  ! コマンドライン引数(JST)取得
  call get_arg(jst, stat)
  if (.not. stat) stop

  ! 祝日マスタデータ読み込み
  call get_mst_holiday(mst_h)

  ! 各種CSV読み込み)
  call get_kokeis(kokeis)        ! 黄経（太陽、月）
  call get_sekkis(sekkis)        ! 二十四節気
  call get_zassetsus(zassetsus)  ! 雑節
  call get_holidays(holidays)    ! 祝日
  call get_etcs(etcs)            ! その他
  call get_ocs(ocs)              ! 旧暦

  ! 各種データ取得
  s_leap  = ""
  s_sekki = ""
  s_z_1   = ""
  s_z_2   = ""
  s_sekku = ""
  jd_jst = etcs(y_idx(jst%year), jst%month, jst%day)%jd + JST_D
  if (ocs(y_idx(jst%year), jst%month, jst%day)%leap) s_leap = "閏"
  i_sekki = sekkis(y_idx(jst%year), jst%month, jst%day)
  if (i_sekki /= 999) s_sekki = SEKKI_24(i_sekki / 15)
  i_z_1 = zassetsus(y_idx(jst%year), jst%month, jst%day)%z_1
  i_z_2 = zassetsus(y_idx(jst%year), jst%month, jst%day)%z_2
  if (i_z_1 /= 99) then
    s_z_1 = ZASSETSU(i_z_1)
    s_z_1 = space_zen2han(s_z_1)
  end if
  if (i_z_2 /= 99) then
    s_z_2 = ZASSETSU(i_z_2)
    s_z_2 = space_zen2han(s_z_2)
  end if
  i_sekku = get_sekku(jst%month, jst%day)
  if (i_sekku /= 9) s_sekku = SEKKU(i_sekku)

  ! 結果出力
  print '(I4, "-", I0.2, "-", I0.2, ",",                      &
      & A, ",", A, ",", F0.3, "UTC(", F0.1, "JST),", A, ",",  &
      & I0.4, "-", A, I0.2, "-", I0.2, ",", A, ",", A, ",",   &
      & A, ",", A, ",", A, ",", F0.8, ",", F0.8, ",", F0.8)', &
      & jst%year, jst%month, jst%day,                                &
      & YOBI(etcs(y_idx(jst%year), jst%month, jst%day)%yobi),        &
      & trim(mst_h(holidays( y_idx(jst%year), jst%month, jst%day))), &
      & etcs(y_idx(jst%year), jst%month, jst%day)%jd, jd_jst,        &
      & KANSHI(etcs(y_idx(jst%year), jst%month, jst%day)%kanshi),    &
      & ocs(y_idx(jst%year), jst%month, jst%day)%year, trim(s_leap), &
      & ocs(y_idx(jst%year), jst%month, jst%day)%month,              &
      & ocs(y_idx(jst%year), jst%month, jst%day)%day,                &
      & ROKUYO(ocs(y_idx(jst%year), jst%month, jst%day)%rokuyo),     &
      & trim(s_sekki), trim(s_z_1), trim(s_z_2), trim(s_sekku),      &
      & kokeis(y_idx(jst%year), jst%month, jst%day)%sun,             &
      & kokeis(y_idx(jst%year), jst%month, jst%day)%moon,            &
      & etcs(y_idx(jst%year), jst%month, jst%day)%moon_age

  stop
contains
  ! コマンドライン引数取得
  ! * YYYYMMDD 形式
  ! * 8桁超入力された場合は、9桁目以降の部分は切り捨てる
  ! * コマンドライン引数がなければ、システム日付を JST とする
  ! * 日時の整合性チェックを行う
  !
  ! :param(inout) type(t_time) jst: JST（日本標準時）
  ! :param(out)   logical     stat: T: 正常, F: 異常
  subroutine get_arg(jst, stat)
    implicit none
    type(t_time), intent(inout) :: jst
    logical,      intent(out)   :: stat
    character(8) :: gc
    integer(SP)  :: dt(8)

    stat = .true.
    if (iargc() == 0) then
      call date_and_time(values=dt)
      jst = t_time(dt(1), dt(2), dt(3), 0, 0, 0, 0)
    else
      call getarg(1, gc)
      if (len(trim(gc)) /= 8) then
        print *, "*** Format must be 'YYYYMMDD'"
        stat = .false.
        return
      end if
      read (gc, '(I4, I2, I2)') jst%year, jst%month, jst%day
    end if

    ! 日付の整合性チェック
    if (.not. is_valid_date(jst)) then
      print *, "*** Date is invalid!"
      stat = .false.
    end if
    if (jst%year < Y_MIN .or. jst%year > Y_MAX) then
      print *, "*** Year must be from 1900 to 2099!"
      stat = .false.
    end if
  end subroutine get_arg

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
    open (unit   = UID_TXT,     &
        & iostat = ios,         &
        & file   = F_TXT_H,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_TXT_H
      stop
    end if

    ! 祝日マスタ TXT ファイル READ
    ! * 名称のない項目は半角スペース30個
    do
      read (UID_TXT, *, iostat = ios) h_id, m, d, kbn, y_s, y_e, h_name
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_TXT_H
      end if
      mst_h(h_id) = h_name
    end do

    ! 祝日マスタ TXT ファイル CLOSE
    close(UID_TXT)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_K,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_K
      stop
    end if

    ! 黄経一覧 CSV ファイル READ
    do
      read (UID_CSV, *, iostat = ios) y, m, d, k_s, k_m
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_K
      end if
      kokeis(y_idx(y), m, d) = t_kokei(k_s, k_m)
    end do

    ! 黄経一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_S,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_S
      stop
    end if

    ! 二十四節気一覧 CSV ファイル READ
    do
      read (UID_CSV, *, iostat = ios) y, m, d, kokei, jst
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_S
      end if
      sekkis(y_idx(y), m, d) = kokei
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_Z,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_Z
      stop
    end if

    ! 雑節一覧 CSV ファイル READ
    do
      read (UID_CSV, *, iostat = ios) y, m, d, z_1, z_2
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_Z
      end if
      zassetsus(y_idx(y), m, d) = t_zassetsu(z_1, z_2)
    end do

    ! 雑節一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_H,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_H
      stop
    end if

    ! 祝日一覧 CSV ファイル READ
    do
      read (UID_CSV, *, iostat = ios) y, m, d, holiday
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_H
      end if
      holidays(y_idx(y), m, d) = holiday
    end do

    ! 祝日一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_E,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_E
      stop
    end if

    ! その他一覧 CSV ファイル READ
    do
      read (UID_CSV, *, iostat = ios) y, m, d, jd, yobi, kanshi, moon_age
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_E
      end if
      etcs(y_idx(y), m, d) = t_etc(jd, yobi, kanshi, moon_age)
    end do

    ! その他一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    open (unit   = UID_CSV,     &
        & iostat = ios,         &
        & file   = F_CSV_O,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV_O
      stop
    end if

    ! 旧暦一覧 CSV ファイル READ
    do
      flag_leap = .false.
      read (UID_CSV, *, iostat = ios) &
        & y, m, d, year, leap, month, day, rokuyo
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_O
      end if
      if (leap == 1) flag_leap = .true.
      ocs(y_idx(y), m, d) = t_oc(year, flag_leap, month, day, rokuyo)
    end do

    ! 旧暦一覧 CSV ファイル CLOSE
    close(UID_CSV)
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
    if (idx == 0) then
      dst = src
      return
    end if
    dst(1:idx-1) = src(1:idx-1)
    do i = idx, len_src
      dst(i:i) = " "
    end do
  end function space_zen2han
end program jpl_cal

