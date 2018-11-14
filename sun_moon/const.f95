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
  character(*), parameter :: F_LEAP_SEC = "LEAP_SEC.txt"
  character(*), parameter :: F_DUT1     = "DUT1.txt"
  integer(SP),  parameter :: DAYS(1:12) = &
    & (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)         ! Days per month
  integer(SP),  parameter :: JST_UTC    = 9                      ! JST - UTC (hour)
  integer(SP),  parameter :: SEC_DAY    = 86400                  ! Seconds per a day
  real(DP),     parameter :: PI         = atan(1.0_DP) * 4.0_DP  ! 円周率
  real(DP),     parameter :: PI_180     = PI / 180.0_DP          ! 円周率/180
  real(DP),     parameter :: EPS        = 0.5e-4_DP              ! 逐次近似計算収束判定値(ε)
  real(DP),     parameter :: A_REF      = 0.585556_DP            ! 大気差(astro refract)
  real(DP),     parameter :: TT_TAI     = 32.184_DP              ! TT - TAI
end module const

