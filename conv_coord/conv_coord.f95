!****************************************************
! 赤道・黄道座標変換（テスト）
!
!   date          name            version
!   2018.10.17    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
program conv_coord
  use const
  use coord
  implicit none
  ! 黄道傾斜角(単位: rad)
  real(DP), parameter :: EPS = 23.43929_DP * PI / 180.0_DP
  ! 元の赤道直交座標
  ! (ある日の地球重心から見た太陽重心の位置(単位: AU))
  real(DP), parameter :: POS(3) = (/ &
    &  0.994436592207009972_DP, &
    & -0.038162917689578337_DP, &
    & -0.016551776709600584_DP/)
  real(DP) :: rect_ec(3), rect_eq(3), rect_eq_2(3)
  real(DP) :: pol_ec(3),  pol_eq(3),  pol_eq_2(3)

  ! 座標変換
  call rect_eq2ec(POS, EPS, rect_ec)
  call rect_ec2eq(rect_ec, EPS, rect_eq)
  call rect2pol(rect_eq, pol_eq)
  call pol_eq2ec(pol_eq, EPS, pol_ec)
  call pol_ec2eq(pol_ec, EPS, pol_eq_2)
  call pol2rect(pol_eq_2, rect_eq_2)

  ! 結果出力
  print *, "元の赤道直交座標(x, y, z):"
  print '(2X, 3F22.18)', POS
  print *, "黄道直交座標に変換(x, y, z):"
  print '(2X, 3F22.18)', rect_ec
  print *, "赤道直交座標に戻す(x, y, z):"
  print '(2X, 3F22.18)', rect_eq
  print *, "赤道極座標に変換(lambda, phi, r):"
  print '(2X, 3F22.18)', pol_eq
  print *, "黄道極座標に変換(lambda, beta, r):"
  print '(2X, 3F22.18)', pol_ec
  print *, "赤道極座標に戻す(alpha, delta, r):"
  print '(2X, 3F22.18)', pol_eq_2
  print *, "赤道直交座標に戻す(x, y, z):"
  print '(2X, 3F22.18)', rect_eq_2

  stop
end program conv_coord

