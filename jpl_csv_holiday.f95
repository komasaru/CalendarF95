!*******************************************************************************
! 祝日一覧(CSV 出力)
! * 祝日マスタの名称は全角10文字(30byte)以下を想定
!
!   Date          Author          Version
!   2018.10.29    mk-mode.com     1.00 新規作成
!   2018.11.10    mk-mode.com     1.01 テキストファイル OPEN/READ 時のエラー処理
!                                      を変更
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time モジュール内で定義
!*******************************************************************************
!
program jpl_holiday
  use const, only : SP, DP, Y_MIN, Y_MAX, Y_ST_K, Y_ST_F, FMT_DT_0
  use time
  implicit none
  character(*), parameter :: F_CSV     = "csv/holiday.csv"
  character(*), parameter :: F_CSV_S   = "csv/sekki_24.csv"
  character(*), parameter :: F_TXT_H   = "HOLIDAY.txt"
  integer(SP),  parameter :: UID_CSV   = 11  ! 祝日一覧CSV用
  integer(SP),  parameter :: UID_CSV_S = 12  ! 二十四節気一覧CSV用
  integer(SP),  parameter :: UID_TXT_H = 13  ! 祝日一覧TXT用
  integer(SP),  parameter :: MAX_H     = 50  ! 最大50件/年
  type :: t_mst
    integer(SP)   :: h_id   = 0
    integer(SP)   :: m      = 0
    integer(SP)   :: d      = 0
    integer(SP)   :: kbn    = 0
    integer(SP)   :: y_s    = 0
    integer(SP)   :: y_e    = 0
    character(30) :: h_name = ""
  end type t_mst
  type :: t_hol
    integer(SP)   :: year  = 0
    integer(SP)   :: month = 0
    integer(SP)   :: day   = 0
    integer(SP)   :: h_id  = 0
  end type t_hol
  type(t_mst), allocatable :: mst_h(:)
  type(t_mst) :: tmp(MAX_H)
  integer(SP) :: len_h, y, i, ios
  integer(SP) :: sekkis(Y_MAX - Y_MIN + 3, 12, 31)
  type(t_hol) :: hols_0(MAX_H), hols_1(MAX_H), hols_2(MAX_H)
  type(t_hol) :: hols(MAX_H)

  ! 祝日マスタ取得(TXT読み込み)
  ! (+祝日マスタ配列アロケート)
  call get_holidays(tmp, len_h)
  allocate(mst_h(len_h))
  mst_h(1:len_h) = tmp(1:len_h)

  ! 二十四節気一覧取得(CSV読み込み)
  call get_sekkis(sekkis)

  ! 書き込み用 CSV ファイル OPEN
  open (unit   = UID_CSV,     &
      & iostat = ios,         &
      & file   = F_CSV,       &
      & action = "write",     &
      & form   = "formatted", &
      & status = "new")
  if (ios /= 0) then
    print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_CSV
    stop
  end if

  ! 計算
  do y = Y_MIN, Y_MAX
    if (y < 1948) cycle
    ! 変動の祝日の日付･曜日を計算 ( 振替休日,国民の休日を除く )
    call comp_holiday_0(y, mst_h, len_h, sekkis, hols_0)
    ! 振替休日計算
    call comp_holiday_1(y, hols_0, hols_1)
    ! 国民の休日計算
    call comp_holiday_2(hols_0, hols_2)
    ! 上記の３つの配列をマージ＆ソート（日付順）
    call merge_hols(hols_0, hols_1, hols_2, hols)
    do i = 1, 50
      if (hols(i)%year == 0) exit
      print '("* ", I4, "-", I0.2, "-", I0.2, ": ", I2)', hols(i)
      write (UID_CSV, '(I4, ",", I2, ",", I2, ",", I2)') hols(i)
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  ! 祝日マスタ配列デアロケート
  deallocate(mst_h)

  stop
contains
  ! 祝日マスタ取得(TXT読み込み)
  ! * 「振替休日」は 1973-04-12 以降
  !
  ! :param(out) character(*) mst_h(50): 祝日マスタ(各行テキスト形式)
  ! :param(out) integer(4)       len_h: データ件数
  subroutine get_holidays(mst_h, len_h)
    implicit none
    type(t_mst), intent(out) :: mst_h(50)
    integer(SP), intent(out) :: len_h
    integer(SP)   :: ios, i, h_id, m, d, kbn, y_s, y_e
    character(30) :: h_name

    ! 祝日一覧 TXT ファイル OPEN
    open (unit   = UID_TXT_H,   &
        & iostat = ios,         &
        & file   = F_TXT_H,     &
        & action = "read",      &
        & form   = "formatted", &
        & status = "old")
    if (ios /= 0) then
      print '("[ERROR:", I0 ,"] Failed to open file: ", A)', ios, F_TXT_H
      stop
    end if

    ! 祝日一覧 TXT ファイル READ
    i = 0
    do
      read (UID_TXT_H, &
        & '(I2, X, I2, X, I2, X, I1, X, I4, X, I4, X, A15)', &
        & iostat = ios) h_id, m, d, kbn, y_s, y_e, h_name
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_TXT_H
      end if
      i = i + 1
      mst_h(i) = t_mst(h_id, m, d, kbn, y_s, y_e, h_name)
    end do
    len_h = i

    ! 祝日一覧 TXT ファイル CLOSE
    close(UID_TXT_H)
  end subroutine get_holidays

  ! 日付固定の祝日、変動の祝日の日付・曜日を計算
  ! * 施行日：1948-07-20
  !
  ! :param(in)  integer(4)                 y_t: 計算対象の西暦年
  ! :param(in)  integer(4)               len_h: 祝日マスタ件数
  ! :param(in)  type(t_mst)           mst_h(*): 祝日マスタ
  ! :param(in)  integer(4) sekkis(202, 12, 31): 二十四節気一覧
  ! :param(out) type(t_hol)           hols(50): 祝日一覧
  subroutine comp_holiday_0(y_t, mst_h, len_h, sekkis, hols)
    implicit none
    integer(SP), intent(in)  :: y_t, len_h
    type(t_mst), intent(in)  :: mst_h(*)
    integer(SP), intent(in)  :: sekkis(202, 12, 31)
    type(t_hol), intent(out) :: hols(50)
    character(32) :: h_name
    type(t_time)  :: gc
    integer(SP)   :: i, d_2, yobi, idx
    real(DP)      :: jd

    ! 計算
    idx = 0  ! 結果格納配列（祝日一覧）のインデックス
    do i = 1, len_h
      if (mst_h(i)%kbn > 7 .or. &
        & y_t < mst_h(i)%y_s .or. &
        & y_t > mst_h(i)%y_e) cycle
      if (mst_h(i)%kbn == 0) then
        ! 月日が既定のもの
        gc = t_time(y_t, mst_h(i)%m, mst_h(i)%d, 0, 0, 0, 0)
        call gc2jd(gc, jd)
        yobi = calc_yobi(jd)
        idx = idx + 1
        hols(idx) = t_hol(y_t, mst_h(i)%m, mst_h(i)%d, mst_h(i)%h_id)
      else
        ! 月日が不定のもの
        if (mst_h(i)%kbn == 2) then
          ! 第2月曜日 ( 8 - 14 の月曜日)
          do d_2 = 8, 14
            gc = t_time(y_t, mst_h(i)%m, d_2, 0, 0, 0, 0)
            call gc2jd(gc, jd)
            yobi = calc_yobi(jd)
            if (yobi == 1) then
              idx = idx + 1
              hols(idx) = t_hol(y_t, mst_h(i)%m, d_2, mst_h(i)%h_id)
              exit
            end if
          end do
        else if (mst_h(i)%kbn == 3) then
          ! 第3月曜日 ( 15 - 21 の月曜日)
          do d_2 = 15, 21
            gc = t_time(y_t, mst_h(i)%m, d_2, 0, 0, 0, 0)
            call gc2jd(gc, jd)
            yobi = calc_yobi(jd)
            if (yobi == 1) then
              idx = idx + 1
              hols(idx) = t_hol(y_t, mst_h(i)%m, d_2, mst_h(i)%h_id)
              exit
            end if
          end do
        else if (mst_h(i)%kbn == 4) then
          ! 二分（春分、秋分）
          call get_last_nibun(y_t, mst_h(i)%m, 31, sekkis, d_2)
          gc = t_time(y_t, mst_h(i)%m, d_2, 0, 0, 0, 0)
          call gc2jd(gc, jd)
          yobi = calc_yobi(jd)
          idx = idx + 1
          hols(idx) = t_hol(y_t, mst_h(i)%m, d_2, mst_h(i)%h_id)
        end if
      end if
    end do
  end subroutine comp_holiday_0

  ! 振替休日計算
  ! ( 「国民の祝日」が日曜日に当たるときは、
  !   その日後においてその日に最も近い「国民の祝日」でない日 )
  ! * 施行日：1973-04-12
  !
  ! :param(in)  integer(4)         y_t: 計算対象の西暦年
  ! :param(in)  type(t_hol) hols_0(50): 祝日一覧（固定、変動祝日）
  ! :param(out) type(t_hol)   hols(50): 祝日一覧（振替休日）
  subroutine comp_holiday_1(y_t, hols_0, hols)
    implicit none
    integer(SP), intent(in)  :: y_t
    type(t_hol), intent(in)  :: hols_0(50)
    type(t_hol), intent(out) :: hols(50)
    character(32) :: h_name
    type(t_time)  :: gc
    integer(SP)   :: i, yobi, yobi_next, idx_0, idx
    integer(SP)   :: y_next, m_next, d_next
    integer(SP)   :: d_plus
    logical       :: flag
    real(DP)      :: jd, jd_next, jd_plus

    ! 計算対象年が「振替休日」の開始年より前なら終了
    if (hols_0(1)%year < Y_ST_F) return

    ! hols_0 のインデックス最大値
    idx_0 = 0
    do i = 1, size(hols_0)
      if (hols_0(i)%year == 0) exit
      idx_0 = i
    end do

    ! 計算
    idx = 0
    do i = 1, idx_0
      if (hols_0(i)%year == 1973) then
        if (hols_0(i)%month < 4 .or. &
         & (hols_0(i)%month == 4 .and. hols_0(i)%day < 12)) then
          cycle
        end if
      end if
      gc = t_time(hols_0(i)%year, hols_0(i)%month, hols_0(i)%day, 0, 0, 0, 0)
      call gc2jd(gc, jd)
      yobi = calc_yobi(jd)
      if (yobi /= 0) cycle
      jd_next = jd + 1
      yobi_next = calc_yobi(jd_next)
      if (i == idx_0) then
        call jd2gc(jd_next, gc)
        idx = idx + 1
        hols(idx) = t_hol(gc%year, gc%month, gc%day, 91)
      else
        flag = .false.
        d_plus = 1
        do while (.not. flag)
          if (i + d_plus - 1 < idx_0) then
            gc = t_time(hols_0(i + d_plus)%year, hols_0(i + d_plus)%month, &
              & hols_0(i + d_plus)%day, 0, 0, 0, 0)
            call gc2jd(gc, jd_plus)
            if (jd_next == jd_plus) then
              jd_next = jd_next + 1
              if (yobi_next == 6) then
                yobi_next = 0
              else
                yobi_next = yobi_next + 1
              end if
              d_plus = d_plus + 1
            else
              flag = .true.
              call jd2gc(jd_next, gc)
              idx = idx + 1
              hols(idx) = t_hol(gc%year, gc%month, gc%day, 90)
            end if
          end if
        end do
      end if
    end do
  end subroutine comp_holiday_1

  ! 国民の休日計算
  ! ( 「国民の祝日」で前後を挟まれた「国民の祝日」でない日 )
  ! ( 年またぎは考慮していない(今のところ不要) )
  ! * 施行日：1985-12-27
  !
  ! :param(in)  type(t_hol) hols_0(50): 祝日一覧（固定、変動祝日）
  ! :param(out) type(t_hol)   hols(50): 祝日一覧（国民の休日）
  subroutine comp_holiday_2(hols_0, hols)
    implicit none
    type(t_hol), intent(in)  :: hols_0(50)
    type(t_hol), intent(out) :: hols(50)
    character(32) :: h_name
    type(t_time)  :: gc
    integer(SP)   :: i, yobi, idx_0, idx
    real(DP)      :: jd, jd_0, jd_1

    ! 計算対象年が「国民の祝日」の開始年より前なら終了
    if (hols_0(1)%year < Y_ST_K) return

    ! hols のインデックス最大値
    idx_0 = 0
    do i = 1, size(hols_0)
      if (hols_0(i)%year == 0) exit
      idx_0 = i
    end do

    ! 計算
    idx = 0
    do i = 1, idx_0
      gc = t_time(hols_0(i)%year, hols_0(i)%month, hols_0(i)%day, 0, 0, 0, 0)
      call gc2jd(gc, jd_0)
      gc = t_time(hols_0(i + 1)%year, hols_0(i + 1)%month, hols_0(i + 1)%day, &
        & 0, 0, 0, 0)
      call gc2jd(gc, jd_1)
      if (jd_0 + 2 == jd_1) then
        jd = jd_0 + 1
        yobi = calc_yobi(jd)
        call jd2gc(jd, gc)
        idx = idx + 1
        hols(idx) = t_hol(gc%year, gc%month, gc%day, 91)
      end if
    end do
  end subroutine comp_holiday_2

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

  ! 二十四節気一覧取得(CSV読み込み)
  !
  ! :param(inout) integer(4) sekkis(202, 12, 31): 二十四節気一覧
  subroutine get_sekkis(sekkis)
    implicit none
    integer(SP), intent(out) :: sekkis(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    character(19) :: jst
    integer(SP)   :: i, j, k, ios, y, m, d, kokei

    ! 配列初期化(黄経 0 度と混同しないよう 999 で初期化)
    sekkis = 999

    ! 二十四節気一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_S,   &
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
      read (UID_CSV_S, *, iostat = ios) y, m, d, kokei, jst
      if (ios < 0) then
        exit
      else if (ios > 0) then
        print '("[ERROR:", I0 ,"] Failed to read file: ", A)', ios, F_CSV_S
      end if
      sekkis(y_idx(y), m, d) = kokei
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV_S)
  end subroutine get_sekkis

  ! 直近の二分（春分、秋分）取得
  ! * 3月か9月しかこの subroutine は利用されないはず
  !
  ! :param(in)  integer(4)                   y: 西暦年
  ! :param(in)  integer(4)                   m: 月
  ! :param(in)  integer(4)                   d: 日
  ! :param(in)  integer(4) sekkis(202, 12, 31): 二十四節気一覧
  ! :param(out) integer(4)                 d_n: 二分の日
  subroutine get_last_nibun(y, m, d, sekkis, d_n)
    implicit none
    integer(SP), intent(in)  :: y, m, d
    integer(SP), intent(in)  :: sekkis(202, 12, 31)
    integer(SP), intent(out) :: d_n
    integer(SP) :: i, kokei

    ! 3月か9月以外だった場合、当日を返す(念の為)
    d_n = d
    if (m /= 3 .and. m/=9) return

    ! 対象の太陽黄経
    if (m == 3) then
      kokei = 0
    else
      kokei = 180
    end if

    ! 直近の二分を探索
    do i = d, 1, -1
      if (sekkis(y_idx(y), m, i) == kokei) then
        d_n = i
        exit
      end if
    end do
  end subroutine get_last_nibun

  ! 二十四節気一覧配列用のインデックス
  !
  ! :param(in) integer(4) y
  ! :return    integer(4) y_idx
  integer(SP) function y_idx(y)
    implicit none
    integer(SP), intent(in) :: y

    y_idx = y - Y_MIN + 2
  end function y_idx

  ! 配列マージ
  !
  ! :param(in)  type(t_hol) hols_0(50): 変動の祝日一覧(振替休日,国民の休日を除く)
  ! :param(in)  type(t_hol) hols_1(50): 振替休日一覧
  ! :param(in)  type(t_hol) hols_2(50): 国民の休日一覧
  ! :param(out) type(t_hol)   hols(50): マージ後
  subroutine merge_hols(hols_0, hols_1, hols_2, hols)
    implicit none
    type(t_hol), intent(in)  :: hols_0(MAX_H)
    type(t_hol), intent(in)  :: hols_1(MAX_H)
    type(t_hol), intent(in)  :: hols_2(MAX_H)
    type(t_hol), intent(out) :: hols(MAX_H)
    character(8) :: dt, dt_a, dt_b
    integer(SP)  :: idx, i, j
    type(t_hol)  :: hol_tmp

    ! マージ
    idx = 0
    do i = 1, MAX_H
      if (hols_0(i)%year == 0) exit
      idx = idx + 1
      hols(idx) = hols_0(i)
    end do
    do i = 1, MAX_H
      if (hols_1(i)%year == 0) exit
      idx = idx + 1
      hols(idx) = hols_1(i)
    end do
    do i = 1, MAX_H
      if (hols_2(i)%year == 0) exit
      idx = idx + 1
      hols(idx) = hols_2(i)
    end do

    ! ソート（バブルソート）
    do i = 1, idx - 1
      do j = i + 1, idx
        write (dt_a, '(I0.4I0.2I0.2)') hols(i)%year, hols(i)%month, hols(i)%day
        write (dt_b, '(I0.4I0.2I0.2)') hols(j)%year, hols(j)%month, hols(j)%day
        if (dt_a > dt_b) then
          hol_tmp = hols(i)
          hols(i) = hols(j)
          hols(j) = hol_tmp
        end if
      end do
    end do

    ! 「振替休日」と「国民の休日」がダブった場合は
    ! 「振替休日」を優先（「国民の休日」を削除）
    do i = 2, idx
      if (hols(i - 1)%year  == hols(i)%year  .and. &
        & hols(i - 1)%month == hols(i)%month .and. &
        & hols(i - 1)%day   == hols(i)%day ) then
        if (hols(i - 1)%h_id == 90 .and. hols(i)%h_id == 91) then
          do j = i, idx - 1
            hols(j) = hols(j + 1)
          end do
          hols(idx) = t_hol(0, 0, 0, 0)
          idx = idx - 1
        end if
      end if
    end do
  end subroutine merge_hols
end program jpl_holiday

