!****************************************************
! 定数モジュール
!
! date          name            version
! 2018.10.17    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module const
  implicit none

  ! SP: 単精度(4), DP: 倍精度(8)
  integer,     parameter :: SP = kind(1.0)
  integer(SP), parameter :: DP = selected_real_kind(2 * precision(1.0_SP))
  real(DP),    parameter :: PI = atan(1.0_DP) * 4.0_DP  ! 円周率
end module const

