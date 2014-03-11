module fitting_routines

  use base_types
  implicit none
  save
  
contains
  
  subroutine linear_regression(n,valid,data,weight,pattern1,pattern2,p1,p2)

    implicit none
    
    ! Input
    integer,intent(in) :: n
    integer,intent(in) :: valid(n)
    real(sp),intent(in)    :: pattern1(n),pattern2(n),data(n),weight(n)
    ! Output
    real(sp),intent(out)   :: p1,p2
    ! Local variables
    real    :: C(2),M(2,2),inv_det
    integer :: i
    
  M=0.
  C=0.
  
  do i=1,n
     if(valid(i)==1.or.valid(i)==4) then
        C(1)  =C(1)  +pattern1(i)*data(i)    *weight(i)
        C(2)  =C(2)  +pattern2(i)*data(i)    *weight(i)
        M(1,1)=M(1,1)+pattern1(i)*pattern1(i)*weight(i)
        M(1,2)=M(1,2)+pattern1(i)*pattern2(i)*weight(i)
        M(2,2)=M(2,2)+pattern2(i)*pattern2(i)*weight(i)
     end if
  end do
  M(2,1)=M(1,2)
  
  inv_det=1./(M(1,1)*M(2,2)-M(1,2)*M(2,1))
  
  p1=(M(2,2)*C(1)-M(1,2)*C(2))*inv_det
  p2=(M(1,1)*C(2)-M(2,1)*C(1))*inv_det
  
end subroutine linear_regression
  

  
  subroutine optimal_scaling(n,valid,data,weight,pattern1,p1)
    
    implicit none
    
    ! Input
    integer,intent(in) :: n
    integer,intent(in) :: valid(n)
    real(sp),intent(in)    :: pattern1(n),data(n),weight(n)
    ! Output
    real(sp),intent(out)   :: p1
    ! Local variables
    real(sp) :: top,bot
    integer :: j

    top = 0.
    bot = 0.

     do j=1,n
        if(valid(j)==1.or.valid(j)==4) then
           top = top + pattern1(j)*data(j)*weight(j)
           bot = bot + pattern1(j)*pattern1(j)*weight(j)
        end if
     end do

     p1 = top / bot
     
   end subroutine optimal_scaling
   
  pure real function opt_scaling(valid,flux,weight,model)

   implicit none

   ! --- Input --- !

   real(sp),intent(in) :: flux(:),weight(:)
   integer,intent(in) :: valid(:)
       ! the input data

   real(sp),dimension(:),intent(in) :: model
   ! the pattern to fit

   ! --- Local variables --- !

   real(sp) :: top,bot
   ! temporary variables

   integer :: j

   top = 0.
   bot = 0.

   do j=1,size(valid)
      if(valid(j)==1.or.valid(j)==4) then
         top = top + model(j)*flux(j) *weight(j)
         bot = bot + model(j)*model(j)*weight(j)
      end if
   end do

   opt_scaling = top / bot

 end function opt_scaling

 !#############################################################################
 ! Find the reduced chi-squared for the fit
 !#############################################################################

 real function chi_squared(valid,flux,flux_err,weight,model)

   implicit none

   ! --- Input --- !

   real(sp),   intent(in) :: flux(:),flux_err(:),weight(:)
   integer,intent(in) :: valid(:)
       ! the input data and model

   real(sp),dimension(:),intent(in) :: model
   ! the pattern to fit

   ! --- Local variable --- !

   integer :: j

   chi_squared = 0.
   do j=1,size(valid)
      select case(valid(j))
      case(1,4)
         chi_squared = chi_squared + ( flux(j) - model(j) )**2 * weight(j)
      case(2)
         if(flux(j) > model(j)) then
           if(flux_err(j)<1.) then
             chi_squared = chi_squared - 2.*log(1.-flux_err(j))
           else
             chi_squared = huge(1.) ; exit
           end if
         end if
      case(3)
         if(flux(j) < model(j)) then
           if(flux_err(j)<1.) then
             chi_squared = chi_squared - 2.*log(1.-flux_err(j))
           else
             chi_squared = huge(1.) ; exit
           end if
         end if
      end select

   end do

 end function chi_squared
 
 end module fitting_routines