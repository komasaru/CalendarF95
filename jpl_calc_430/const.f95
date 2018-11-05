!****************************************************
! Module for constant variables
!
! date          name            version
! 2018.10.21    mk-mode.com     1.00 新規作成
!
! Copyright(C) 2018 mk-mode.com All Rights Reserved.
!****************************************************
!
module const
  implicit none

  ! SP: 単精度(4), DP: 倍精度(8)
  integer,      parameter :: SP = kind(1.0)
  integer(SP),  parameter :: DP = selected_real_kind(2 * precision(1.0_SP))
  character(*), parameter :: USAGE(8) = (/ &
    & "[USAGE] ./jpl_calc_430 TARGET CENTER [JD]                            ", &
    & "* Astro No. (TARGET: 1 - 15, CENTER: 0 - 13)                         ", &
    & "   1: Mercury,  2: Venus,  3: Earth,  4: Mars,  5: Jupiter,          ", &
    & "   6: Saturn,  7: Uranus,  8: Neptune,  9: Pluto, 10: Moon,  11: Sun,", &
    & "  12: Solar System Barycenter,  13: Earth-Moon Barycenter,           ", &
    & "  14: Earth Nutations,  15: Lunar Mantle Librations,                 ", &
    & "   0: if TARGET = 14 or 15                                           ", &
    & "  *** Must be TARGET /= CENTER ***                                   "  &
  & /)
  character(*), parameter :: ASTRS(15) = (/ &
    & "Mercury                ", "Venus                  ", &
    & "Earth                  ", "Mars                   ", &
    & "Jupiter                ", "Saturn                 ", &
    & "Uranus                 ", "Neptune                ", &
    & "Pluto                  ", "Moon                   ", &
    & "Sun                    ", "Solar system Barycenter", &
    & "Earth-Moon barycenter  ", "Earth Nutations        ", &
    & "Lunar mantle Librations" &
  & /)
  character(*), parameter :: F_BIN = "JPLEPH"
  integer(SP),  parameter :: KSIZE = 2036
  integer(SP),  parameter :: NRECL = 4
  logical,      parameter :: KM    = .false.  ! 単位フラグ（T: km, km/sec, F: AU, AU/day）
  integer(SP),  parameter :: UID   = 10       ! Unit ID of binary file
  character(*), parameter :: FMT_DT_0 = &
    & '(I4I2I2I2I2I2I6)'
  character(*), parameter :: FMT_DT_1 = &
    & '(I4, I0.2, I0.2, I0.2, I0.2, I0.2, I0.6)'
end module const

