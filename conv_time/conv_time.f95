!*******************************************************************************
! 各種時刻換算
!
!   date          name            version
!   2018.10.14    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
! ---
! 引数 : JST（日本標準時）
!          YYYYMMDD[HHMMSS[MMM]]
!          無指定なら現在(システム日時)と判断。（上記最後の MMM はミリ秒）
! ---
! * 定数 DUT1 (= UT1 - UTC) の値は以下を参照。
!     [日本標準時プロジェクト Announcement of DUT1]
!     (http://jjy.nict.go.jp/QandA/data/dut1.html)
!   但し、値は 1.0 秒以下なので、精度を問わないなら 0.0 固定でもよい(?)
! * UTC - TAI（協定世界時と国際原子時の差）は、以下のとおりとする。
!   - 1972年07月01日より古い場合は一律で 10
!   - 2019年07月01日以降は一律で 37
!   - その他は、指定の値
!     [日本標準時プロジェクト　Information of Leap second]
!     (http://jjy.nict.go.jp/QandA/data/leapsec.html)
! * ΔT = TT - UT1 は、以下のとおりとする。
!   - 1972-01-01 以降、うるう秒挿入済みの年+2までは、以下で算出
!       ΔT = 32.184 - (UTC - TAI) - DUT1
!     UTC - TAI は
!     [うるう秒実施日一覧](http://jjy.nict.go.jp/QandA/data/leapsec.html)
!     を参照
!   - その他の期間は NASA 提供の略算式により算出
!     [NASA - Polynomial Expressions for Delta T]
!     (http://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html)
! * 構造型 type(t_time) は time モジュール内で定義
!*******************************************************************************
!
program conv_time
  use const, only : SP, DP, JST_UTC, FMT_DT_0
  use time
  use delta_t
  implicit none
  type(t_time) :: jst, utc, tai, ut1, tt, tcg, tcb, tdb
  integer(SP)  :: utc_tai
  real(DP)     :: jd, jd_tcb, t, dut1, dt

  ! コマンドライン引数（現在日時(JST)）, UTC 取得
  call get_arg(jst, utc)
  if (jst%year == 0) stop

  ! 各種時刻換算
  call gc2jd(utc, jd)              ! UTC -> JD
  call jd2jc(jd, t)                ! JD  -> JC
  call utc2utc_tai(utc, utc_tai)   ! UTC -> UTC - TAI
  call utc2dut1(utc, dut1)         ! UTC -> DUT1
  call utc2dt(utc, utc_tai, dt)    ! UTC -> delta T
  call utc2tai(utc, utc_tai, tai)  ! UTC -> TAI
  call utc2ut1(utc, dut1, ut1)     ! UTC -> UT1
  call tai2tt(tai, tt)             ! TAI -> TT
  call tt2tcg(tt, jd, tcg)         ! TT  -> TCG
  call tt2tcb(tt, jd, tcb)         ! TT  -> TCB
  call gc2jd(tcb, jd_tcb)          ! TCB -> JD(TCB)
  call tcb2tdb(tcb, jd_tcb, tdb)   ! TCB -> TDB

  ! 結果出力
  print '("      JST: ", A)',                date_fmt(jst)
  print '("      UTC: ", A)',                date_fmt(utc)
  print '("JST - UTC: ", I7, 12X,   "(hours)")',   JST_UTC
  print '("       JD: ", F18.10, X, "(days)")',         jd
  print '("        T: ", F18.10, X, "(centuries)")',     t
  print '("UTC - TAI: ", I7, 12X,   "(seconds)")', utc_tai
  print '("     DUT1: ", F9.1, 10X, "(seconds)")',    dut1
  print '("  delta T: ", F11.3, 8X, "(seconds)")',      dt
  print '("      TAI: ", A)',                date_fmt(tai)
  print '("      UT1: ", A)',                date_fmt(ut1)
  print '("       TT: ", A)',                date_fmt(tt )
  print '("      TCG: ", A)',                date_fmt(tcg)
  print '("      TCB: ", A)',                date_fmt(tcb)
  print '("   JD_TCB: ", F18.10, X, "(days)")',     jd_tcb
  print '("      TDB: ", A)',                date_fmt(tdb)

  stop
contains
  ! コマンドライン引数取得
  ! * YYYYMMDD[HHMMSS[MMM]] 形式
  ! * 17桁超入力された場合は、18桁目以降の部分は切り捨てる
  ! * コマンドライン引数がなければ、システム日付を JST とする
  ! * 日時の整合性チェックは行わない
  !
  ! :param(out) type(t_time) jst
  ! :param(out) type(t_time) utc
  subroutine get_arg(jst, utc)
    implicit none
    type(t_time), intent(out) :: jst, utc
    character(17) :: gc
    integer(SP)   :: dt(8)
    integer(SP)   :: len_gc

    if (iargc() == 0) then
      call date_and_time(values=dt)
      jst = t_time(dt(1), dt(2), dt(3), dt(5), dt(6), dt(7), dt(8))
    else
      call getarg(1, gc)
      len_gc = len(trim(gc))
      if (len_gc /= 8 .and. len_gc /= 14 .and. len_gc /= 17) then
        print *, "Format: YYYYMMDD[HHMMSS[MMM]"
        return
      end if
      read (gc, FMT_DT_0) jst
    end if
    if (jst%year /= 0) call jst2utc(jst, utc)
  end subroutine get_arg
end program conv_time

