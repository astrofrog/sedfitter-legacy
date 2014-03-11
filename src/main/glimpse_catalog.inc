! ### GLIMPSE/SAGE CATALOG VARIABLES ### !

character(len=26) :: desig = ''
! GLIMPSE designation

integer(8) :: twomid = 0
! 2MASS ID

real(dp) :: l,b,dl,db,ra,dec,dra,ddec
  ! Galactic and Equatorial coordinates

integer(4) :: conf = 0
! confusion flag

real(4),dimension(7) :: mag=0.,dmag=0.,f=0.,df=0.
! magnitudes and fluxes

real(4),dimension(7) :: rmsf=0.,sky=0.,sn=0.,srcdens=0.
! various information

integer(2),dimension(7) :: m=0,n=0,mf=0
integer(8),dimension(7) :: sqf=0
! number of detections and passes, source quality flag, and flux calculation method

character(len=200) :: catalog_format='(A26,1x,i10,2F11.6,2F4.1,2F11.6,2F4.1,I3,7(F7.3,F7.3),14(E11.3),4(E11.3),&
       &4(E11.3),7(1X,F6.2),4(1X,F6.1),4(I3),4I3,7(1X,I11),4I5)'
