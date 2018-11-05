!****************************************************
! Modules for trigonometric calculation
!
! date          name            version
! 2018.10.25    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module trigonometric
  use const
  implicit none

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! λ の計算
  ! * λ = arctan((sinδ sinε + cosδ sinα cosε ) / cosδ cosα)
  !
  ! :param(in) real(DP) alp: α
  ! :param(in) real(DP) dlt: δ
  ! :param(in) real(DP) eps: ε
  ! :return    real(DP) lmd: λ
  function comp_lambda(alp, dlt, eps) result(lmd)
    implicit none
    real(DP), intent(in) :: alp, dlt, eps
    real(DP) :: lmd
    real(DP) :: a, b

    a = sin(dlt) * sin(eps) + cos(dlt) * sin(alp) * cos(eps)
    b = cos(dlt) * cos(alp)
    lmd = atan2(a, b)
    do while (lmd < 0.0_DP)
      lmd = lmd + PI * 2.0_DP
    end do
  end function comp_lambda

  ! β の計算
  ! * β = arcsisn((sinδ cosε - cosδ sinα sinε)
  !
  ! :param(in) real(DP) alp: α
  ! :param(in) real(DP) dlt: δ
  ! :param(in) real(DP) eps: ε
  ! :return    real(DP) bet: β
  function comp_beta(alp, dlt, eps) result(bet)
    implicit none
    real(DP), intent(in) :: alp, dlt, eps
    real(DP) :: bet
    real(DP) :: a

    a = sin(dlt) * cos(eps) - cos(dlt) * sin(alp) * sin(eps)
    bet = asin(a)
  end function comp_beta

  ! α の計算
  ! * α = arctan((-sinβ sinε + cosβ sinλ cosε ) / cosβ cosλ)
  !
  ! :param(in) real(DP) lmd: λ
  ! :param(in) real(DP) bet: β
  ! :param(in) real(DP) eps: ε
  ! :return    real(DP) alp: α
  function comp_alpha(lmd, bet, eps) result(alp)
    implicit none
    real(DP), intent(in) :: lmd, bet, eps
    real(DP) :: alp
    real(DP) :: a, b

    a = -sin(bet) * sin(eps) +  cos(bet) * sin(lmd) * cos(eps)
    b =  cos(bet) * cos(lmd)
    alp = atan2(a, b)
    do while (alp < 0.0_DP)
      alp = alp + PI * 2.0_DP
    end do
  end function comp_alpha

  ! δ の計算
  ! * δ = arcsisn((sinβ cosε + cosβ sinλ sinε)
  !
  ! :param(in) real(DP) lmd: λ
  ! :param(in) real(DP) bet: β
  ! :param(in) real(DP) eps: ε
  ! :return    real(DP) dlt: δ
  function comp_delta(lmd, bet, eps) result(dlt)
    implicit none
    real(DP), intent(in) :: lmd, bet, eps
    real(DP) :: dlt
    real(DP) :: a, b

    a = sin(bet) * cos(eps) + cos(bet) * sin(lmd) * sin(eps)
    dlt = asin(a)
  end function comp_delta
end module trigonometric

