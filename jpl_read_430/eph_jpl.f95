!****************************************************
! Modules for JPL ephemeris
!
! date          name            version
! 2018.10.21    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module eph_jpl
  use const
  implicit none
  private
  public :: get_bin
  type, public :: t_bin
    character(84) :: ttl(3)
    character(6)  :: cnam(1000)
    integer(SP)   :: ncon, ipt(3, 13), numde
    real(DP)      :: ss(3), au, emrat, cval(1000), coeff(1500)
  end type t_bin

contains
  ! ============================
  ! Public subroutines/functions
  ! ============================

  ! Binary record getting
  ! * cnam(1:400) と cnam(401:), ipt(3, 1: 12) と ipt(3, 13) は
  !   連続していないので注意！
  !
  ! :param(in)  real(8)      jd: Julian Day
  ! :param(out) type(t_bin) bin: バイナリデータ
  subroutine get_bin(jd, bin)
    implicit none
    real(DP),    intent(in)  :: jd
    type(t_bin), intent(out) :: bin
    integer(SP) :: ios, i, j, idx
    integer(SP) :: n_coeff = KSIZE / 2

    open (unit   = UID,           &
      &   iostat = ios,           &
      &   file   = F_BIN,         &
      &   form   = "unformatted", &
      &   access = "direct",      &
      &   recl   = NRECL * KSIZE, &
      &   status = "old")
    if (ios /= 0) then
      print *, "[ERROR] Failed to open file: " // F_BIN
      stop
    end if

    ! ヘッダ: 1レコード目
    read (UID, rec=1) &
      & bin%ttl, (bin%cnam(i),i=1,400), bin%ss, bin%ncon, &
      & bin%au, bin%emrat, ((bin%ipt(i,j),i=1,3),j=1,12), &
      & bin%numde, (bin%ipt(i,13),i=1,3), &
      & (bin%cnam(i),i=401,bin%ncon)
    ! ヘッダ: 2レコード目
    read (UID, rec=2) bin%cval
    ! 係数の読み込みインデックス計算
    idx = int((jd - bin%ss(1)) / bin%ss(3))
    ! 係数
    read (UID, rec=idx+3) (bin%coeff(i),i=1,n_coeff)

    close(UID)
  end subroutine get_bin
end module eph_jpl

