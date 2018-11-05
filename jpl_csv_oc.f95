!****************************************************
! 旧暦一覧(CSV 出力)
!
!   Date          Author          Version
!   2018.10.31    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数: なし
! ---
! * 構造型 type(t_time) は time モジュール内で定義
!****************************************************
!
program jpl_oc
  use const, only : SP, DP, Y_MIN, Y_MAX, DAYS, JST_D
  use time
  implicit none
  ! 定数
  character(*), parameter :: F_CSV     = "csv/oc.csv"
  character(*), parameter :: F_CSV_S   = "csv/sekki_24.csv"
  character(*), parameter :: F_CSV_M   = "csv/moon.csv"
  integer(SP),  parameter :: UID_CSV   = 11  ! 旧暦一覧CSV用
  integer(SP),  parameter :: UID_CSV_S = 12  ! 二十四節気一覧CSV用
  integer(SP),  parameter :: UID_CSV_M = 13  ! 朔望一覧CSV用
  ! 構造型定義
  type :: t_chu                          ! 中気用配列
    real(DP)      :: jd        = 0.0_DP  ! (JD)
    integer(SP)   :: kokei     = 0       ! (黄経)
  end type t_chu
  type :: t_m                            ! 朔日用行列
    integer(SP)   :: month     = 0       ! (月)
    logical       :: flag_leap = .false. ! (閏月フラグ)
    integer(SP)   :: jd        = 0       ! (朔日のJD(小数切り捨て))
  end type t_m
  type :: t_oc                           ! 旧暦用配列
    integer(SP)   :: year      = 0       ! (年)
    integer(SP)   :: month     = 0       ! (月)
    integer(SP)   :: day       = 0       ! (日)
    integer(SP)   :: rokuyo    = 0       ! (六曜)
    logical       :: flag_leap = .false. ! (閏月フラグ)
  end type t_oc
  ! 変数
  character(34), allocatable :: nibuns(:)    ! 二分二至一覧（各行は文字列として取得）
  character(34), allocatable :: chus(:)      ! 中気一覧（各行は文字列として取得）
  character(34), allocatable :: sakus(:)     ! 朔一覧（各行は文字列として取得）
  character(34) :: tmp(5000)  ! 一覧（一時処理用）
  type(t_time) :: jst
  integer(SP)  :: len_n, len_c, len_s
  integer(SP)  :: y, m, d, days_m, ios, leap
  real(DP)     :: jd
  type(t_oc)   :: oc  ! 旧暦配列

  ! 二分二至一覧取得(CSV読み込み)
  ! (+二分二至一覧配列アロケート)
  call get_nibuns(tmp, len_n)
  allocate(nibuns(len_n))
  nibuns(1:len_n) = tmp(1:len_n)

  ! 中気一覧取得(CSV読み込み)
  ! (+中気一覧配列アロケート)
  call get_chus(tmp, len_c)
  allocate(chus(len_c))
  chus(1:len_c) = tmp(1:len_c)

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
        jd = jd - JST_D  ! JST -> UTC
        call calc_oc(y, m, d, nibuns, len_n, chus, len_c, sakus, len_s, oc)
        if (oc%flag_leap) then
          leap = 1
        else
          leap = 0
        end if
        print '("* ", I4, "-", I0.2, "-", I0.2, " - ", &
            & I4, ", ", I1, ", ", I2, ", ", I2, ", ", I1)', &
            & y, m, d, oc%year, leap, oc%month, oc%day, oc%rokuyo
        write (UID_CSV, &
            & '(I4, ",", I2, ",", I2, ",", &
            & I4, ",", I1, ",", I2, ",", I2, ",", I1)') &
            & y, m, d, oc%year, leap, oc%month, oc%day, oc%rokuyo
      end do
    end do
  end do

  ! 書き込み用 CSV ファイル CLOSE
  close(UID_CSV)

  ! 二分二至／中気／朔一覧配列デアロケート
  deallocate(nibuns)
  deallocate(chus)
  deallocate(sakus)

  stop
contains
  ! 二分二至一覧取得(CSV読み込み)
  ! * 二十四節気一覧から mod(黄経差, 90) == 0 のもの（二分二至）のみ取得
  !
  ! :param(out) character(34) nibuns(808): 二分二至一覧（一次処理用）
  ! :param(out) integer(4)          len_n: 件数
  subroutine get_nibuns(nibuns, len_n)
    implicit none
    character(*), intent(out) :: nibuns(808)
    integer(SP),  intent(out) :: len_n
    character(34) :: buf
    integer(SP)   :: ios, kokei

    ! 配列初期化
    nibuns = ""

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
    len_n = 0
    do
      read (UID_CSV_S, '(A)', iostat = ios) buf
      if (ios /= 0) exit
      read (buf(12:14), '(I3)') kokei
      if (mod(kokei, 90) /= 0) cycle
      len_n = len_n + 1
      nibuns(len_n) = buf
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV_S)
  end subroutine get_nibuns

  ! 中気一覧取得(CSV読み込み)
  ! * 二十四節気一覧から mod(黄経差, 30) == 0 のもの（中気）のみ取得
  !
  ! :param(out) character(34) chus(2424): 中気一覧（一次処理用）
  ! :param(out) integer(4)         len_c: 件数
  subroutine get_chus(chus, len_c)
    implicit none
    character(*), intent(out) :: chus(2424)
    integer(SP),  intent(out) :: len_c
    character(34) :: buf
    integer(SP)   :: ios, kokei

    ! 配列初期化
    chus = ""

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
    len_c = 0
    do
      read (UID_CSV_S, '(A)', iostat = ios) buf
      if (ios /= 0) exit
      read (buf(12:14), '(I3)') kokei
      if (mod(kokei, 30) /= 0) cycle
      len_c = len_c + 1
      chus(len_c) = buf
    end do

    ! 二十四節気一覧 CSV ファイル CLOSE
    close(UID_CSV_S)
  end subroutine get_chus

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

  ! 旧暦計算
  !
  ! :param(in)  integer(4)         year: 西暦年(GC)
  ! :param(in)  integer(4)        month: 月(GC)
  ! :param(in)  integer(4)          day: 日(GC)
  ! :param(in)  character(34) nibuns(*): 二分二至一覧
  ! :param(in)  integer(4)        len_n: 二分二至一覧件数
  ! :param(in)  character(34)   chus(*): 中気一覧
  ! :param(in)  integer(4)        len_c: 中気一覧件数
  ! :param(in)  character(34)  sakus(*): 朔一覧
  ! :param(in)  integer(4)        len_s: 朔一覧件数
  ! :param(out) type(t_oc)           oc: 旧暦(年,うるう月フラグ,月,日,六曜)
  subroutine calc_oc(&
    & year, month, day, nibuns, len_n, chus, len_c, sakus, len_s, oc)
    implicit none
    integer(SP),  intent(in)  :: year, month, day, len_n, len_c, len_s
    character(*), intent(in)  :: nibuns(*), chus(*), sakus(*)
    type(t_oc),   intent(out) :: oc
    type(t_chu)  :: chu(4), chu_tmp   ! 中気計算用配列
    type(t_m)    :: m(5)              ! 朔日計算用行列
    real(DP)     :: saku(5)           ! 朔時刻計算用配列
    type(t_time) :: gc
    integer(SP)  :: ye, mo, da, ho, mi, se, us
    integer(SP)  :: i, stat, idx
    real(DP)     :: jd, jd_saku
    logical      :: flag_leap

    ! 朔時刻計算用配列の初期化
    saku = 0.0_DP

    ! JD 計算
    gc = t_time(year, month, day, 0, 0, 0, 0)
    call gc2jd(gc, jd)
    jd = jd - JST_D  ! JST -> UTC

    ! 計算対象の直前にあたる二分二至の時刻を計算
    call last_nibun(year, month, day, nibuns, len_n, chu_tmp)
    chu(1) = chu_tmp

    ! 中気の時刻を計算 ( 3回計算する )
    do i = 2, 4
      call jd2gc(chu(i - 1)%jd + 32, gc)
      call last_chu(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
        & chus, len_c, chu_tmp)
      chu(i) = chu_tmp
    end do
    ! 計算対象の直前にあたる二分二至の直前の朔の時刻を求める
    call jd2gc(chu(1)%jd, gc)
    call last_saku(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
      & sakus, len_s, jd_saku)
    saku(1) = jd_saku
    ! 朔の時刻を求める
    do i = 2, 5
      jd_saku = saku(i - 1) + 30.0_DP
      call jd2gc(jd_saku, gc)
      call last_saku(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
        & sakus, len_s, jd_saku)
      saku(i) = jd_saku
      ! 前と同じ時刻を計算した場合( 両者の差が26日以内 )には、初期値を
      ! +35日にして再実行させる。
      if (abs(int(saku(i - 1)) - int(saku(i))) <= 26) then
        call jd2gc(saku(i - 1) + 35, gc)
        call last_saku(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
          & sakus, len_s, jd_saku)
        saku(i) = jd_saku
      end if
    end do
    ! saku[1]が二分二至の時刻以前になってしまった場合には、朔をさかのぼり過ぎ
    ! たと考えて、朔の時刻を繰り下げて修正する。
    ! その際、計算もれ（saku[4]）になっている部分を補うため、朔の時刻を計算
    ! する。（近日点通過の近辺で朔があると起こる事があるようだ...？）
    if (int(saku(2)) <= int(chu(1)%jd)) then
      do i = 1, 4
        saku(i) = saku(i + 1)
      end do
      call jd2gc(saku(4) + 35, gc)
      call last_saku(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
        & sakus, len_s, jd_saku)
      saku(5) = jd_saku
    ! saku[0]が二分二至の時刻以後になってしまった場合には、朔をさかのぼり足
    ! りないと見て、朔の時刻を繰り上げて修正する。
    ! その際、計算もれ（saku[0]）になっている部分を補うため、朔の時刻を計算
    ! する。（春分点の近辺で朔があると起こる事があるようだ...？）
    else if (int(saku(1)) > int(chu(1)%jd)) then
      do i = 5, 2, -1
        saku(i) = saku(i - 1)
      end do
      call jd2gc(saku(1) + 27, gc)
      call last_saku(gc%year, gc%month, gc%day, gc%hour, gc%minute, gc%second, &
        & sakus, len_s, jd_saku)
      saku(1) = jd_saku
    end if
    ! 閏月検索Flagセット
    ! （節月で４ヶ月の間に朔が５回あると、閏月がある可能性がある。）
    ! (flag_leap: T(閏月), F(平月))
    flag_leap = .false.
    if (int(saku(5)) <= int(chu(4)%jd)) flag_leap = .true.
    ! 朔日行列の作成
    ! m(i)%month     ... 月名 ( 1:正月 2:２月 3:３月 .... )
    ! m(i)%flag_leap ... 閏フラグ ( F:平月 T:閏月 )
    ! m(i)%jd        ... 朔日のjd
    m(1)%month = (chu(1)%kokei / 30) + 2
    if (m(1)%month > 12) m(1)%month = m(1)%month - 12
    m(1)%jd = int(saku(1))
    m(1)%flag_leap = .false.
    do i = 2, 5
      if (flag_leap .and. i /= 2) then
        if (int(chu(i - 1)%jd) <= int(saku(i - 1)) .or. &
          & int(chu(i - 1)%jd) >= int(saku(i))) then
          m(i - 1)%month     = m(i - 2)%month
          m(i - 1)%flag_leap = .true.
          m(i - 1)%jd        = int(saku(i - 1))
          flag_leap = .false.
        end if
      end if
      m(i)%month = m(i - 1)%month + 1
      if (m(i)%month > 12) m(i)%month = m(i)%month - 12
      m(i)%jd = int(saku(i))
      m(i)%flag_leap = .false.
    end do
    ! 朔日行列から旧暦を求める。
    stat = 0
    idx  = 0
    do i = 1, 5
      idx = i
      if (int(jd) < int(m(i)%jd)) then
        stat = 1
        exit
      else if (int(jd) == int(m(i)%jd)) then
        stat = 2
        exit
      end if
    end do
    if (stat == 1) idx = idx - 1
    oc%flag_leap = m(idx)%flag_leap
    oc%month     = m(idx)%month
    oc%day       = int(jd) - int(m(idx)%jd) + 1
    ! 旧暦年の計算
    ! （旧暦月が10以上でかつ新暦月より大きい場合には、
    !   まだ年を越していないはず...）
    call jd2gc(jd, gc)
    oc%year = gc%year
    !if (oc%month > 9 .and. oc%month > mo) oc%year = oc%year - 1
    if (oc%month > 9 .and. oc%month > gc%month) oc%year = oc%year - 1
    ! 六曜
    oc%rokuyo = mod(oc%month + oc%day, 6)
  end subroutine calc_oc

  ! 直近の二分ニ至（春分、秋分、夏至、冬至）取得
  !
  ! :param(in)  integer(4)         year: 西暦年(GC)
  ! :param(in)  integer(4)        month: 月(GC)
  ! :param(in)  integer(4)          day: 日(GC)
  ! :param(in)  character(34) nibuns(*): 二分二至一覧
  ! :param(in)  integer(4)        len_n: 二分二至一覧件数
  ! :param(out) type(t_chu)         chu: 直近の二分ニ至の[日時, 黄経]
  subroutine last_nibun(year, month, day, nibuns, len_s, chu)
    implicit none
    integer(SP),  intent(in)  :: year, month, day, len_s
    character(*), intent(in)  :: nibuns(*)
    type(t_chu),  intent(out) :: chu
    character(19) :: gc_t, gc
    type(t_time)  :: gc_tmp
    integer(SP)   :: i, kokei, ye, mo, da, ho, mi, se
    real(DP)      :: jd

    ! 対象の日時
    write (gc_t, '(I4, "-", I0.2, "-", I0.2, " 00:00:00")') &
      & year, month, day

    ! 逆順ループで取得
    do i = len_n, 1, -1
      gc = nibuns(i)(16:34)
      read (nibuns(i)(12:14), *) kokei
      if (gc < gc_t) exit
    end do
    read (gc( 1: 4), *) ye
    read (gc( 6: 7), *) mo
    read (gc( 9:10), *) da
    read (gc(12:13), *) ho
    read (gc(15:16), *) mi
    read (gc(18:19), *) se
    gc_tmp = t_time(ye, mo, da, ho, mi, se, 0)
    call gc2jd(gc_tmp, jd)
    jd = jd - JST_D
    chu = t_chu(jd, kokei)
  end subroutine last_nibun

  ! 直近の中気（mod(黄経, 30) == 0）取得
  !
  ! :param(in)  integer(4)       year: 西暦年(GC)
  ! :param(in)  integer(4)      month: 月(GC)
  ! :param(in)  integer(4)        day: 日(GC)
  ! :param(in)  integer(4)       hour: 時(GC)
  ! :param(in)  integer(4)     minute: 分(GC)
  ! :param(in)  integer(4)     second: 秒(GC)
  ! :param(in)  character(34) chus(*): 中気一覧
  ! :param(in)  integer(4)      len_c: 中気一覧件数
  ! :param(out) type(t_chu)       chu: 直近の二分ニ至の[日時, 黄経]
  subroutine last_chu(year, month, day, hour, minute, second, chus, len_c, chu)
    implicit none
    integer(SP),  intent(in)  :: year, month, day, hour, minute, second, len_c
    character(*), intent(in)  :: chus(*)
    type(t_chu),  intent(out) :: chu
    character(19) :: gc_t, gc
    type(t_time)  :: gc_tmp
    integer(SP)   :: i, kokei, ye, mo, da, ho, mi, se
    real(DP)      :: jd

    ! 対象の日時
    write (gc_t, &
      & '(I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)') &
      & year, month, day, hour, minute, second

    ! 逆順ループで取得
    do i = len_c, 1, -1
      gc = chus(i)(16:34)
      read (chus(i)(12:14), *) kokei
      if (gc < gc_t) exit
    end do
    read (gc( 1: 4), *) ye
    read (gc( 6: 7), *) mo
    read (gc( 9:10), *) da
    read (gc(12:13), *) ho
    read (gc(15:16), *) mi
    read (gc(18:19), *) se
    gc_tmp = t_time(ye, mo, da, ho, mi, se, 0)
    call gc2jd(gc_tmp, jd)
    jd = jd - JST_D
    chu = t_chu(jd, kokei)
  end subroutine last_chu

  ! 直近の朔（太陽・月黄経差 == 0）取得
  !
  ! :param(in)  integer(4)        year: 西暦年(GC)
  ! :param(in)  integer(4)       month: 月(GC)
  ! :param(in)  integer(4)         day: 日(GC)
  ! :param(in)  integer(4)        hour: 時(GC)
  ! :param(in)  integer(4)      minute: 分(GC)
  ! :param(in)  integer(4)      second: 秒(GC)
  ! :param(in)  character(34) sakus(*): 朔一覧
  ! :param(in)  integer(4)       len_s: 朔一覧件数
  ! :param(out) type(t_chu)         jd: 直近の朔のJD
  subroutine last_saku(year, month, day, hour, minute, second, sakus, len_s, jd)
    implicit none
    integer(SP),  intent(in)  :: year, month, day, hour, minute, second, len_s
    character(*), intent(in)  :: sakus(*)
    real(DP),     intent(out) :: jd
    character(19) :: gc_t, gc
    type(t_time)  :: gc_tmp
    integer(SP)   :: i, kokei, ye, mo, da, ho, mi, se

    ! 対象の日時
    write (gc_t, &
      & '(I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2)') &
      & year, month, day, hour, minute, second

    ! 逆順ループで取得
    do i = len_s, 1, -1
      gc = sakus(i)(16:34)
      if (gc < gc_t) exit
    end do
    read (gc( 1: 4), *) ye
    read (gc( 6: 7), *) mo
    read (gc( 9:10), *) da
    read (gc(12:13), *) ho
    read (gc(15:16), *) mi
    read (gc(18:19), *) se
    gc_tmp = t_time(ye, mo, da, ho, mi, se, 0)
    call gc2jd(gc_tmp, jd)
    jd = jd - JST_D - 0.125
  end subroutine last_saku
end program jpl_oc

