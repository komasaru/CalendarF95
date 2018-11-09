!*******************************************************************************
! 定数モジュール
!
!   date          name            version
!   2018.10.13    mk-mode.com     1.00 新規作成
!   2018.11.09    mk-mode.com     1.01 時刻の取扱変更(マイクロ秒 => ミリ秒)
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!*******************************************************************************
!
module const
  implicit none

  ! SP: 単精度(4), DP: 倍精度(8)
  integer,      parameter :: SP = kind(1.0)
  integer(SP),  parameter :: DP = selected_real_kind(2 * precision(1.0_SP))
  character(*), parameter :: BIN_PATH   = "JPLEPH"
  character(*), parameter :: F_LEAP_SEC = "LEAP_SEC.txt"
  character(*), parameter :: F_DUT1     = "DUT1.txt"
  integer(SP),  parameter :: DAYS(1:12) = &
    & (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)      ! Days per month
  integer(SP),  parameter :: JST_UTC    = 9                   ! JST - UTC (hour)
  integer(SP),  parameter :: J2000      = 2451545             ! Reference epoch (J2000.0)
  integer(SP),  parameter :: DAY_JC     = 36525               ! Days per Julian century
  integer(SP),  parameter :: SEC_DAY    = 86400               ! Seconds per a day
  real(DP),     parameter :: TT_TAI     = 32.184_DP           ! TT - TAI
  real(DP),     parameter :: L_G        = 6.969290134e-10_DP  ! for TCG
  real(DP),     parameter :: L_B        = 1.550519768e-8_DP   ! for TCB, TDB
  real(DP),     parameter :: T_0        = 2443144.5003725_DP  ! for TCG, TDB, TCB
  real(DP),     parameter :: TDB_0      = -6.55e-5_DP         ! for TDB
  character(*), parameter :: FMT_DT_0   = &
    & '(I4I2I2I2I2I2I3)'
  character(*), parameter :: FMT_DT_1   = &
    & '(I4, I0.2, I0.2, I0.2, I0.2, I0.2, I0.3)'
  character(*), parameter :: FMT_DT_2   = &
    & '(I4, "-", I0.2, "-", I0.2, " ", I0.2, ":", I0.2, ":", I0.2, ".", I0.3)'
end module const

