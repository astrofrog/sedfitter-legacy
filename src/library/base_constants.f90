module base_constants

  use base_types
  implicit none
  save

  real(sp),parameter :: zero_sp = 0._sp
  real(dp),parameter :: zero_dp = 0._dp


  real(dp),parameter :: zero = 0._dp
  real(dp),parameter :: half = 0.5_dp
  real(dp),parameter :: one = 1._dp
  real(dp),parameter :: two = 2._dp

  
  ! Physical constants  
  
  real(dp),parameter :: G_cgs = 6.67d-08 ! FIX THIS
  real(dp),parameter :: G_si  = 6.67d-11 ! 
  
!  N = kg.m/s^2 = [G]*kg^2/m**2
!  [G] = m^3/s^2/kg

  real(dp),parameter :: k_cgs = 1.3806503d-16 ! erg/K
  real(dp),parameter :: k_si  = 1.3806503d-23 !   J/K

  real(dp),parameter :: h_cgs = 6.626075d-27   ! ergs.s  
  real(dp),parameter :: h_si  = 6.626068e-34_dp !    J.s

  real(dp),parameter :: c_si  = 2.99792458e08_dp ! m / s
  real(dp),parameter :: c_cgs = 2.99792458e10_dp ! cm / s
  ! speed of light
  
  real(dp),parameter :: kpc_si  = 3.08568025e19_dp ! m
  real(dp),parameter :: kpc_cgs = 3.08568025e21_dp ! cm
  ! kiloparsec
  
  real(dp),parameter  :: pi = 3.14159265358979323846_dp
  real(sp),parameter :: pi_sp = 3.14159265358979323846_sp
  real(dp),parameter :: pi_dp = 3.14159265358979323846_dp
  
  real(dp),parameter :: twopi = 2._dp * pi
  real(sp),parameter :: twopi_sp = pi_sp + pi_sp
  real(dp),parameter :: twopi_dp = pi_dp + pi_dp
  
  real(dp),parameter  :: deg2rad = pi / 180._dp
  real(dp),parameter  :: rad2deg = 180._dp / pi
  real(sp),parameter :: deg2rad_sp = pi_sp / 180._sp
  real(sp),parameter :: rad2deg_sp = 180._sp / pi_sp
  real(dp),parameter :: deg2rad_dp = pi_dp / 180._dp
  real(dp),parameter :: rad2deg_dp = 180._dp / pi_dp
  
  real(dp),parameter :: lsun_cgs = 3.845e33_dp ! erg/s
  
  real(dp),parameter :: rsun_cgs = 6.96e10_dp ! cm
  
  real(dp),parameter :: au_cgs = 1.49598e13_dp ! cm
  
  real(dp),parameter :: year_cgs = 3600._dp * 24._dp * 365.25_dp
  
  real(dp),parameter :: msun_cgs = 1.989e33_dp ! g
  ! Conversions
  
  real(dp),parameter :: ergs2mJy = 1.e26_dp
  real(dp),parameter :: microns2cm = 1.e-4_dp
  real(dp),parameter :: microns2m  = 1.e-6_dp
  
  real(dp),parameter :: stef_boltz = 5.671e-5_dp
  
end module base_constants
