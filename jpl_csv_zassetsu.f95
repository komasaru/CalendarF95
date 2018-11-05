!****************************************************
! 雑節一覧(CSV 出力)
!
!   Date          Author          Version
!   2018.10.28    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time モジュール内で定義
!****************************************************
!
program jpl_zassetsu
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS, JST_D
  use time
  implicit none
  character(*), parameter :: F_CSV     = "csv/zassetsu.csv"
  character(*), parameter :: F_CSV_K   = "csv/kokei.csv"
  character(*), parameter :: F_CSV_S   = "csv/sekki_24.csv"
  integer(SP),  parameter :: UID_CSV   = 11  ! 雑節一覧CSV用
  integer(SP),  parameter :: UID_CSV_K = 12  ! 太陽黄経一覧CSV用
  integer(SP),  parameter :: UID_CSV_S = 13  ! 二十四節気一覧CSV用
  type(t_time) :: jst
  character(5) :: str_z
  integer(SP)  :: y, m, d, days_m, ios
  real(DP)     :: jd
  real(DP)     :: kokeis(Y_MAX - Y_MIN + 3, 12, 31)
  integer(SP)  :: sekkis(Y_MAX - Y_MIN + 3, 12, 31)
  integer(SP)  :: zassetsu(2)

  ! 太陽黄経・二十四節気一覧取得(CSV読み込み)
  call get_kokeis(kokeis)
  call get_sekkis(sekkis)

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
        jd = jd - JST_D  ! JST -> UTC
        call comp_zassetsu(jd, kokeis, sekkis, zassetsu)
        if (zassetsu(1) == 99) cycle
        write (str_z, '(I2, ",", I2)') zassetsu
        print '("* ", I4, "-", I0.2, "-", I0.2, " - ", A)', &
            & y, m, d, str_z
        write (UID_CSV, &
            & '(I4, ",", I2, ",", I2, ",", I2, ",", I2)') &
            & y, m, d, zassetsu
      end do
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  stop
contains
  ! 太陽黄経一覧取得(CSV読み込み)
  !
  ! :param(out) real(8) kokeis(202, 12, 31): 太陽黄経一覧
  subroutine get_kokeis(kokeis)
    implicit none
    real(DP), intent(out) :: kokeis(Y_MAX - Y_MIN + 3, 12, 31)
    character(36) :: buf
    integer(SP)   :: ios, y, m, d
    real(DP)      :: k_s, k_m

    ! 配列初期化
    kokeis = 0.0_DP

    ! 黄経一覧 CSV ファイル OPEN
    open (unit   = UID_CSV_K,   &
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
      read (UID_CSV_K, *, iostat = ios) y, m, d, k_s, k_m
      if (ios /= 0) exit
      kokeis(y_idx(y), m, d) = k_s
    end do

    ! 黄経一覧 CSV ファイル CLOSE
    close(UID_CSV_K)
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
    open (unit   = UID_CSV_S,   &
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
      read (UID_CSV_S, *, iostat = ios) y, m, d, kokei, jst
      if (ios /= 0) exit
      sekkis(y_idx(y), m, d) = kokei
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV_S)
  end subroutine get_sekkis

  ! 雑節の計算
  !
  ! :param real(8)                     jd: Julian Day
  ! :param real(8)    kokeis(202, 12, 31): 太陽黄経一覧
  ! :param integer(4) sekkis(202, 12, 31): 二十四節気一覧
  ! :param integer(4)         zassetsu(2): 0-17の整数配列
  subroutine comp_zassetsu(jd, kokeis, sekkis, zassetsu)
    implicit none
    real(DP),    intent(in)  :: jd
    real(DP),    intent(in)  :: kokeis(202, 12, 31)
    integer(SP), intent(in)  :: sekkis(202, 12, 31)
    integer(SP), intent(out) :: zassetsu(2)
    type(t_time) :: gc
    integer(SP)  :: y, m, d, i
    real(DP)     :: lmd_b5, lmd_b4, lmd_0, lmd_a1, lmd_a5, lmd_a6
    real(DP)     :: lmd_today, lmd_tomorrow

    zassetsu = (/99, 99/)
    ! 計算対象日の太陽の黄経
    call jd2gc(jd, gc)
    lmd_0  = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 計算対象日の翌日の太陽の黄経
    call jd2gc(jd + 1, gc)
    lmd_a1 = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 計算対象日の5日前の太陽の黄経(社日計算用)
    call jd2gc(jd - 5, gc)
    lmd_b5 = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 計算対象日の4日前の太陽の黄経(社日計算用)
    call jd2gc(jd - 4, gc)
    lmd_b4 = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 計算対象日の5日後の太陽の黄経(社日計算用)
    call jd2gc(jd + 5, gc)
    lmd_a5 = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 計算対象日の6日後の太陽の黄経(社日計算用)
    call jd2gc(jd + 6, gc)
    lmd_a6 = kokeis(y_idx(gc%year), gc%month, gc%day)
    ! 太陽の黄経の整数部分( 土用, 入梅, 半夏生 計算用 )
    lmd_today    = int(lmd_0 )
    lmd_tomorrow = int(lmd_a1)

    ! #### ここから各種雑節計算
    ! 0:節分 ( 立春の前日 )
    call jd2gc(jd + 1, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 315) then
      call put_array(0, zassetsu)
    end if
    ! 1:彼岸入（春） ( 春分の日の3日前 )
    call jd2gc(jd + 3, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) ==   0) then
      call put_array(1, zassetsu)
    end if
    ! 2:彼岸（春） ( 春分の日 )
    call jd2gc(jd, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) ==   0) then
      call put_array(2, zassetsu)
    end if
    ! 3:彼岸明（春） ( 春分の日の3日後 )
    call jd2gc(jd - 3, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) ==   0) then
      call put_array(3, zassetsu)
    end if
    ! 4:社日（春） ( 春分の日に最も近い戊(つちのえ)の日 )
    ! * 計算対象日が戊の日の時、
    !   * 4日後までもしくは4日前までに春分の日がある時、
    !       この日が社日
    !   * 5日後が春分の日の時、
    !       * 春分点(黄経0度)が午前なら
    !           この日が社日
    !       * 春分点(黄経0度)が午後なら
    !           この日の10日後が社日
    if (mod(int(jd), 10) == 4) then
      ! [ 当日から4日後 ]
      do i = 0, 4
        call jd2gc(jd + i, gc)
        if (sekkis(y_idx(gc%year), gc%month, gc%day) == 0) then
          call put_array(4, zassetsu)
        end if
      end do
      ! [ 1日前から4日前 ]
      do i = 1, 4
        call jd2gc(jd - i, gc)
        if (sekkis(y_idx(gc%year), gc%month, gc%day) == 0) then
          call put_array(4, zassetsu)
        end if
      end do
      ! [ 5日後 ]
      call jd2gc(jd + 5, gc)
      if (sekkis(y_idx(gc%year), gc%month, gc%day) == 0) then
        ! 春分の日の黄経(太陽)と翌日の黄経(太陽)の中間点が
        ! 0度(360度)以上なら、春分点が午前と判断
        if ((lmd_a5 + lmd_a6 + 360.0_DP) / 2.0_DP >= 360.0_DP) then
          call put_array(4, zassetsu)
        end if
      end if
      ! [ 5日前 ]
      call jd2gc(jd - 5, gc)
      if (sekkis(y_idx(gc%year), gc%month, gc%day) == 0) then
        ! 春分の日の黄経(太陽)と翌日の黄経(太陽)の中間点が
        ! 0度(360度)未満なら、春分点が午後と判断
        if ((lmd_b4 + lmd_b5 + 360.0_DP) / 2.0_DP < 360.0_DP) then
          call put_array(4, zassetsu)
        end if
      end if
    end if
    ! 5:土用入（春） ( 黄経(太陽) = 27度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 27) then
      call put_array(5, zassetsu)
    end if
    ! 6:八十八夜 ( 立春から88日目(87日後) )
    call jd2gc(jd - 87, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 315) then
      call put_array(6, zassetsu)
    end if
    ! 7:入梅 ( 黄経(太陽) = 80度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 80) then
      call put_array(7, zassetsu)
    end if
    ! 8:半夏生  ( 黄経(太陽) = 100度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 100) then
      call put_array(8, zassetsu)
    end if
    ! 9:土用入（夏） ( 黄経(太陽) = 117度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 117) then
      call put_array(9, zassetsu)
    end if
    ! 10:二百十日 ( 立春から210日目(209日後) )
    call jd2gc(jd - 209, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 315) then
      call put_array(10, zassetsu)
    end if
    ! 11:二百二十日 ( 立春から220日目(219日後) )
    call jd2gc(jd - 219, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 315) then
      call put_array(11, zassetsu)
    end if
    ! 12:彼岸入（秋） ( 秋分の日の3日前 )
    call jd2gc(jd + 3, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
      call put_array(12, zassetsu)
    end if
    ! 13:彼岸（秋） ( 秋分の日 )
    call jd2gc(jd, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
      call put_array(13, zassetsu)
    end if
    ! 14:彼岸明（秋） ( 秋分の日の3日後 )
    call jd2gc(jd - 3, gc)
    if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
      call put_array(14, zassetsu)
    end if
    ! 15:社日（秋） ( 秋分の日に最も近い戊(つちのえ)の日 )
    ! * 計算対象日が戊の日の時、
    !   * 4日後までもしくは4日前までに秋分の日がある時、
    !       この日が社日
    !   * 5日後が秋分の日の時、
    !       * 秋分点(黄経180度)が午前なら
    !           この日が社日
    !       * 秋分点(黄経180度)が午後なら
    !           この日の10日後が社日
    if (mod(int(jd), 10) == 4) then
      ! [ 当日から4日後 ]
      do i = 0, 4
        call jd2gc(jd + i, gc)
        if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
          call put_array(15, zassetsu)
        end if
      end do
      ! [ 1日前から4日前 ]
      do i = 1, 4
        call jd2gc(jd - i, gc)
        if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
          call put_array(15, zassetsu)
        end if
      end do
      ! [ 5日後 ]
      call jd2gc(jd + 5, gc)
      if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
        ! 春分の日の黄経(太陽)と翌日の黄経(太陽)の中間点が
        ! 0度(360度)以上なら、春分点が午前と判断
        if ((lmd_a5 + lmd_a6) / 2.0_DP >= 180.0_DP) then
          call put_array(15, zassetsu)
        end if
      end if
      ! [ 5日前 ]
      call jd2gc(jd - 5, gc)
      if (sekkis(y_idx(gc%year), gc%month, gc%day) == 180) then
        ! 春分の日の黄経(太陽)と翌日の黄経(太陽)の中間点が
        ! 0度(360度)未満なら、春分点が午後と判断
        if ((lmd_b4 + lmd_b5) / 2.0_DP < 180.0_DP) then
          call put_array(15, zassetsu)
        end if
      end if
    end if
    ! 16:土用入（秋） ( 黄経(太陽) = 207度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 207) then
      call put_array(16, zassetsu)
    end if
    ! 17:土用入（冬） ( 黄経(太陽) = 297度 )
    if (lmd_today /= lmd_tomorrow .and. lmd_tomorrow == 297) then
      call put_array(17, zassetsu)
    end if
  end subroutine comp_zassetsu

  ! 太陽黄経・二十四節気一覧配列用のインデックス
  !
  ! :param(in) integer(4) y
  ! :return    integer(4) y_idx
  integer(SP) function y_idx(y)
    implicit none
    integer(SP), intent(in) :: y

    y_idx = y - Y_MIN + 2
  end function y_idx

  ! 配列格納
  !
  ! :param(in)    integer(4)           n: 雑節番号
  ! :param(inout) integer(4) zassetsu(2): 0-17の整数配列
  subroutine put_array(n, zassetsu)
    implicit none
    integer(SP), intent(in)    :: n
    integer(SP), intent(inout) :: zassetsu(2)

    if (zassetsu(1) == 99) then
      zassetsu(1) = n
    else
      if (zassetsu(2) == 99) zassetsu(2) = n
    end if
  end subroutine put_array
end program jpl_zassetsu

