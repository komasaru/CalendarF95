!*******************************************************************************
! Module for constant variables
!
!   date          name            version
!   2018.10.21    mk-mode.com     1.00 新規作成
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
  character(*), parameter :: F_BIN    = "JPLEPH"
  integer(SP),  parameter :: KSIZE    = 2036
  integer(SP),  parameter :: NRECL    = 4
  integer(SP),  parameter :: UID      = 10
  character(*), parameter :: FMT_DT_0 = '(I4I2I2I2I2I2I3)'
  character(*), parameter :: FMT_DT_1 = '(I4, I0.2, I0.2, I0.2, I0.2, I0.2, I0.3)'
end module const

