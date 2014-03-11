!**********************************************************************!
! This module 
!
! Versions:
!
! Original code : TR
!
!**********************************************************************!

module base_array

  use base_types
  implicit none
  save

  !########################!
  private
  public :: integral
  public :: integral_log
  public :: integrate
  public :: interpolate_linear
  public :: interpolate_log
  public :: interpolate_log_old 
  public :: interpolate_bilinear
  public :: locate
  public :: ipos
  public :: xval
  public :: list2hist
  public :: list2array
  public :: invert_matrix
  public :: print_matrix
  public :: bin_array
  public :: smooth_2d
  public :: index_array_1d,sort
  !########################!

  interface index_array_1d
     module procedure index_array_1d_sp,index_array_1d_dp
  end interface
  
  interface sort
     module procedure sort_integer,sort_sp,sort_dp
  end interface

    interface integral
      module procedure integral_sp,integral_dp
      module procedure integral_all_sp,integral_all_dp
    end interface
    
    interface integral_log
      module procedure integral_log_sp,integral_log_dp
      module procedure integral_all_log_sp,integral_all_log_dp
    end interface

  interface locate
     module procedure locate_sp,locate_dp
  end interface

    interface interpolate_linear
       module procedure interpolate_linear_sp,interpolate_linear_dp
    end interface
    
    interface interpolate_bilinear
       module procedure interpolate_bilinear_sp,interpolate_bilinear_dp
    end interface
    
      interface interpolate_log
         module procedure interpolate_log_sp,interpolate_log_dp
      end interface

      interface interpolate_log_old
         module procedure interpolate_log_old_sp,interpolate_log_old_dp
      end interface

  interface list2hist
     module procedure list2hist_sp,list2hist_dp
  end interface

  interface list2array
     module procedure list2array_sp,list2array_dp
  end interface

  interface ipos
     module procedure ipos_sp,ipos_dp
  end interface

  interface xval
     module procedure xval_sp,xval_dp
  end interface

contains
  
  !#############################################################################################
  !#                                         INDEXING                                          #
  !#############################################################################################
  
   !********************************************************************************
    ! Index a 1D array of real numbers
    !********************************************************************************

  
    subroutine sort_dp(x,y)
      implicit none
      real(dp),dimension(:) :: x
      real(dp),dimension(:),optional :: y
      integer,dimension(size(x)) :: order
      call index_array_1d_dp(size(x),x,order)
      x = x(order)
      if(present(y)) y = y(order)
    end subroutine sort_dp

    subroutine index_array_1d_dp(n,arr,indexx)

      ! subroutine to index an array
      ! thomas robitaille - november 2005
      ! based on the nr subroutine

      implicit none

      integer,intent(in) :: n
      ! number of elements in the array

      real(dp),dimension(n),intent(in) :: arr
      ! the array to index

      integer,dimension(n),intent(out) :: indexx
      ! the index of the array

      integer,parameter :: nn=15,nstack=50
      ! limit at which to switch over to normal sort, size of stack

      integer,dimension(nstack) :: istack

      integer :: i,j

      integer :: indext,jstack,l,r,k

      real(dp) ::a

      integer :: itemp

      ! Set up preliminary index

      do j=1,n
         indexx(j)=j
      end do

      ! ORIGINAL SECTION:

      jstack=0
      l=1
      r=n
      do
         if (r-l < nn) then
            do j=l+1,r
               indext=indexx(j)
               a=arr(indext)
               do i=j-1,l,-1
                  if (arr(indexx(i)) <= a) exit
                  indexx(i+1)=indexx(i)
               end do
               indexx(i+1)=indext
            end do
            if (jstack == 0) return
            r=istack(jstack)
            l=istack(jstack-1)
            jstack=jstack-2
         else
            k=(l+r)/2

            ! SWAP indexx(k) and indexx(l+1):
            itemp=indexx(k)
            indexx(k)=indexx(l+1)
            indexx(l+1)=itemp

            ! SWAP indexx(l) and indexx(r)
            if(arr(indexx(l)).gt.arr(indexx(r)))then
               itemp=indexx(l)
               indexx(l)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l+1) and indexx(r)
            if(arr(indexx(l+1)).gt.arr(indexx(r)))then
               itemp=indexx(l+1)
               indexx(l+1)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l) and indexx(l+1)
            if(arr(indexx(l)).gt.arr(indexx(l+1)))then
               itemp=indexx(l)
               indexx(l)=indexx(l+1)
               indexx(l+1)=itemp
            endif

            ! ORIGINAL SECTION:

            i=l+1
            j=r
            indext=indexx(l+1)
            a=arr(indext)
            do
               do
                  i=i+1
                  if (arr(indexx(i)) >= a) exit
               end do
               do
                  j=j-1
                  if (arr(indexx(j)) <= a) exit
               end do
               if (j < i) exit

               ! SWAP indexx(i) AND indexx(j):

               itemp=indexx(i)
               indexx(i)=indexx(j)
               indexx(j)=itemp

               ! ORIGINAL SECTION:

            end do
            indexx(l+1)=indexx(j)
            indexx(j)=indext
            jstack=jstack+2
            if (jstack > nstack) print *,'indexx: nstack too small'
            if (r-i+1 >= j-l) then
               istack(jstack)=r
               istack(jstack-1)=i
               r=j-1
            else
               istack(jstack)=j-1
               istack(jstack-1)=l
               l=i
            end if
         end if
      end do

    end subroutine index_array_1d_dp

  
    subroutine sort_sp(x,y)
      implicit none
      real(sp),dimension(:) :: x
      real(sp),dimension(:),optional :: y
      integer,dimension(size(x)) :: order
      call index_array_1d_sp(size(x),x,order)
      x = x(order)
      if(present(y)) y = y(order)
    end subroutine sort_sp

    subroutine index_array_1d_sp(n,arr,indexx)

      ! subroutine to index an array
      ! thomas robitaille - november 2005
      ! based on the nr subroutine

      implicit none

      integer,intent(in) :: n
      ! number of elements in the array

      real(sp),dimension(n),intent(in) :: arr
      ! the array to index

      integer,dimension(n),intent(out) :: indexx
      ! the index of the array

      integer,parameter :: nn=15,nstack=50
      ! limit at which to switch over to normal sort, size of stack

      integer,dimension(nstack) :: istack

      integer :: i,j

      integer :: indext,jstack,l,r,k

      real(sp) ::a

      integer :: itemp

      ! Set up preliminary index

      do j=1,n
         indexx(j)=j
      end do

      ! ORIGINAL SECTION:

      jstack=0
      l=1
      r=n
      do
         if (r-l < nn) then
            do j=l+1,r
               indext=indexx(j)
               a=arr(indext)
               do i=j-1,l,-1
                  if (arr(indexx(i)) <= a) exit
                  indexx(i+1)=indexx(i)
               end do
               indexx(i+1)=indext
            end do
            if (jstack == 0) return
            r=istack(jstack)
            l=istack(jstack-1)
            jstack=jstack-2
         else
            k=(l+r)/2

            ! SWAP indexx(k) and indexx(l+1):
            itemp=indexx(k)
            indexx(k)=indexx(l+1)
            indexx(l+1)=itemp

            ! SWAP indexx(l) and indexx(r)
            if(arr(indexx(l)).gt.arr(indexx(r)))then
               itemp=indexx(l)
               indexx(l)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l+1) and indexx(r)
            if(arr(indexx(l+1)).gt.arr(indexx(r)))then
               itemp=indexx(l+1)
               indexx(l+1)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l) and indexx(l+1)
            if(arr(indexx(l)).gt.arr(indexx(l+1)))then
               itemp=indexx(l)
               indexx(l)=indexx(l+1)
               indexx(l+1)=itemp
            endif

            ! ORIGINAL SECTION:

            i=l+1
            j=r
            indext=indexx(l+1)
            a=arr(indext)
            do
               do
                  i=i+1
                  if (arr(indexx(i)) >= a) exit
               end do
               do
                  j=j-1
                  if (arr(indexx(j)) <= a) exit
               end do
               if (j < i) exit

               ! SWAP indexx(i) AND indexx(j):

               itemp=indexx(i)
               indexx(i)=indexx(j)
               indexx(j)=itemp

               ! ORIGINAL SECTION:

            end do
            indexx(l+1)=indexx(j)
            indexx(j)=indext
            jstack=jstack+2
            if (jstack > nstack) print *,'indexx: nstack too small'
            if (r-i+1 >= j-l) then
               istack(jstack)=r
               istack(jstack-1)=i
               r=j-1
            else
               istack(jstack)=j-1
               istack(jstack-1)=l
               l=i
            end if
         end if
      end do

    end subroutine index_array_1d_sp

  
    subroutine sort_integer(x,y)
      implicit none
      integer,dimension(:) :: x
      integer,dimension(:),optional :: y
      integer,dimension(size(x)) :: order
      call index_array_1d_integer(size(x),x,order)
      x = x(order)
      if(present(y)) y = y(order)
    end subroutine sort_integer

    subroutine index_array_1d_integer(n,arr,indexx)

      ! subroutine to index an array
      ! thomas robitaille - november 2005
      ! based on the nr subroutine

      implicit none

      integer,intent(in) :: n
      ! number of elements in the array

      integer,dimension(n),intent(in) :: arr
      ! the array to index

      integer,dimension(n),intent(out) :: indexx
      ! the index of the array

      integer,parameter :: nn=15,nstack=50
      ! limit at which to switch over to normal sort, size of stack

      integer,dimension(nstack) :: istack

      integer :: i,j

      integer :: indext,jstack,l,r,k

      integer ::a

      integer :: itemp

      ! Set up preliminary index

      do j=1,n
         indexx(j)=j
      end do

      ! ORIGINAL SECTION:

      jstack=0
      l=1
      r=n
      do
         if (r-l < nn) then
            do j=l+1,r
               indext=indexx(j)
               a=arr(indext)
               do i=j-1,l,-1
                  if (arr(indexx(i)) <= a) exit
                  indexx(i+1)=indexx(i)
               end do
               indexx(i+1)=indext
            end do
            if (jstack == 0) return
            r=istack(jstack)
            l=istack(jstack-1)
            jstack=jstack-2
         else
            k=(l+r)/2

            ! SWAP indexx(k) and indexx(l+1):
            itemp=indexx(k)
            indexx(k)=indexx(l+1)
            indexx(l+1)=itemp

            ! SWAP indexx(l) and indexx(r)
            if(arr(indexx(l)).gt.arr(indexx(r)))then
               itemp=indexx(l)
               indexx(l)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l+1) and indexx(r)
            if(arr(indexx(l+1)).gt.arr(indexx(r)))then
               itemp=indexx(l+1)
               indexx(l+1)=indexx(r)
               indexx(r)=itemp
            endif

            ! SWAP indexx(l) and indexx(l+1)
            if(arr(indexx(l)).gt.arr(indexx(l+1)))then
               itemp=indexx(l)
               indexx(l)=indexx(l+1)
               indexx(l+1)=itemp
            endif

            ! ORIGINAL SECTION:

            i=l+1
            j=r
            indext=indexx(l+1)
            a=arr(indext)
            do
               do
                  i=i+1
                  if (arr(indexx(i)) >= a) exit
               end do
               do
                  j=j-1
                  if (arr(indexx(j)) <= a) exit
               end do
               if (j < i) exit

               ! SWAP indexx(i) AND indexx(j):

               itemp=indexx(i)
               indexx(i)=indexx(j)
               indexx(j)=itemp

               ! ORIGINAL SECTION:

            end do
            indexx(l+1)=indexx(j)
            indexx(j)=indext
            jstack=jstack+2
            if (jstack > nstack) print *,'indexx: nstack too small'
            if (r-i+1 >= j-l) then
               istack(jstack)=r
               istack(jstack-1)=i
               r=j-1
            else
               istack(jstack)=j-1
               istack(jstack-1)=l
               l=i
            end if
         end if
      end do

    end subroutine index_array_1d_integer

  

  !**********************************************************************!
  ! integrate : trapezium integration (with index limits)
  !**********************************************************************!

  real(dp) function integrate(array_x,array_y,i_start,i_end)

    implicit none

    ! --- Input --- !

    real(dp),dimension(:),intent(in) :: array_x,array_y
    ! the array to integrate (x and y components)

    integer,intent(in) :: i_start,i_end
    ! starting and ending indices for the integration

    ! --- Local variables --- !

    integer :: i
    ! loop variable

    real(dp) :: sum
    ! summation variable

    sum = 0._dp

    do i=i_start,i_end-1

       sum = sum + 0.5_dp * ( array_y(i+1)+array_y(i) ) * ( array_x(i+1) - array_x(i) )

    end do

    integrate = sum

  end function integrate


  real(dp) function integral_all_dp(x,y)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_dp = integral_dp(x,y,x1,x2)
  end function integral_all_dp

  real(dp) function integral_dp(x,y,x1,x2)

    implicit none

    real(dp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(dp) :: f1,f2,sum
    integer :: j
    ! loop variable

    ! ### Find position in x array under x1 ### !

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_dp=0._dp
       return
    end if

    if(x1.gt.x(1)) then
      i1=locate(x,x1)
      f1 = interpolate_linear(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interpolate_linear(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+0.5_dp*(y(j)+y(j+1))*(x(j+1)-x(j))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+0.5_dp*(f1+y(i1+1))*(x(i1+1)-x1)
       if(x2.lt.x(size(x))) sum=sum+0.5_dp*(f2+y(i2))*(x2-x(i2))

       integral_dp=real(sum)

    else

       integral_dp=0.5_dp*(f1+f2)*(x2-x1)

    end if

  end function integral_dp
  
  real(dp) function integral_all_log_dp(x,y)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_log_dp = integral_log_dp(x,y,x1,x2)
  end function integral_all_log_dp
  

  real(dp) function integral_log_dp(x,y,x1,x2)

    implicit none

    real(dp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(dp) :: f1,f2,sum
    integer :: j
    ! loop variable

    ! ### Find position in x array under x1 ### !

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_log_dp=0._dp
       return
    end if

    if(x1.gt.x(1)) then
      i1=locate(x,x1)
      f1 = interpolate_log(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interpolate_log(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+trapezium_log(x(j),y(j),x(j+1),y(j+1))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+trapezium_log(x1,f1,x(i1+1),y(i1+1))
       if(x2.lt.x(size(x))) sum=sum+trapezium_log(x(i2),y(i2),x2,f2)

       integral_log_dp=real(sum)

    else

       integral_log_dp=trapezium_log(x1,f1,x2,f2)

    end if
    
  contains
    
    real(dp) function trapezium_log(x1,y1,x2,y2)
      implicit none
      real(dp),intent(in) :: x1,y1,x2,y2
      real(dp) :: b
      if(x1==x2) then
        trapezium_log = 0.
      else
        b = log10(y1/y2) / log10(x1/x2)
        trapezium_log = y1 * (x2*(x2/x1)**b-x1) / (b+1)
      end if
    end function trapezium_log
    
  end function integral_log_dp
      
  !**********************************************************************!
  ! This function is used to interpolate
  !
  ! Versions: 
  !
  ! Original code : TR
  !**********************************************************************!


  real(dp) function interpolate_log_dp(x,y,xval)
    
    use base_messages
    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    character(len=100) :: text
    ! error message

    real(dp) :: frac
    ! temporary fraction
    
    real(dp) :: x1,x2,y1,y2
    
    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
       call error("interpolate",text)
    end if
       
    if( ipos < n .and. ipos > 0) then
      x1 = log10(x(ipos))
      x2 = log10(x(ipos+1))
      y1 = log10(y(ipos))
      y2 = log10(y(ipos+1))
       frac = ( log10(xval) - x1 ) / ( x2 - x1 )
       interpolate_log_dp = y1 + frac * ( y2 - y1 )
       interpolate_log_dp = 10._dp**(interpolate_log_dp)
    else if(ipos == n) then
       interpolate_log_dp = y(n)
    else if(ipos == 0) then
       interpolate_log_dp = y(1)
    else
       write(text,'("Unexpected value of ipos : ",I0)') ipos
       call error("interpolate",text)
    end if
    
  end function interpolate_log_dp
  
  real(dp) function interpolate_log_old_dp(x,y,xval)
  
    implicit none
    
    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y
    
    real(dp),dimension(size(x)) :: x_temp,y_temp
    ! the x and y arrays

    real(dp) :: xval_temp,yval_temp
    ! the value at which to interpolate y

    x_temp = log10(x)
    xval_temp = log10(xval)

    where(y > 0.)
      y_temp = log10(y)
    elsewhere
      y_temp = -huge(y_temp)
    end where
    
    yval_temp = interpolate_linear_dp(x_temp,y_temp,xval_temp)
    
    interpolate_log_old_dp = 10._dp**(yval_temp)
    
  end function interpolate_log_old_dp

  real(dp) function interpolate_linear_dp(x,y,xval)
    
    use base_messages
    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    character(len=100) :: text
    ! error message

    real(dp) :: frac
    ! temporary fraction
    
    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
       call error("interpolate",text)
    end if
       
    if( ipos < n .and. ipos > 0) then
       frac = ( xval - x(ipos) ) / ( x(ipos+1) - x(ipos) )
       interpolate_linear_dp = y(ipos) + frac * ( y(ipos+1) - y(ipos) )
    else if(ipos == n) then
       interpolate_linear_dp = y(n)
    else if(ipos == 0) then
       interpolate_linear_dp = y(1)
    else
       write(text,'("Unexpected value of ipos : ",I0)') ipos
       call error("interpolate",text)
    end if
    
  end function interpolate_linear_dp
  
  function interpolate_bilinear_dp(x,y,array,x0,y0) result(value)

    use base_messages
    implicit none

    real(dp),intent(in) :: x(:),y(:),array(:,:),x0,y0
    real(dp) :: value,norm
    integer :: i1,i2,j1,j2
    character(len=100) :: text

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
       call error("interpolate",text)
    end if

    if(j1==-1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
       call error("interpolate",text)
    end if
    
    norm = 1._dp / (x(i2) - x(i1)) / (y(j2)-y(j1))
    
    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
    &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
    &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
    &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interpolate_bilinear_dp

  !**********************************************************************!
  ! This function is used to locate the position in an array
  !
  ! Versions: 
  !
  ! Original code : Numerical Recipes
  ! April 2007    : TR - changed variable types
  !                      added out of bounds clause
  !**********************************************************************!

  integer function locate_dp(xx,x)

    implicit none
    real(dp), dimension(:), intent(in) :: xx
    real(dp), intent(in) :: x
    integer :: n,jl,jm,ju
    logical :: ascnd
    n=size(xx)
    ascnd = (xx(n) >= xx(1))
    jl=0
    ju=n+1
    do
       if (ju-jl <= 1) exit
       jm=(ju+jl)/2
       if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
       else
          ju=jm
       end if
    end do

    if (x == xx(1)) then
       locate_dp = 1
    else if (x == xx(n)) then
       locate_dp = n-1
    else if(x > xx(n) .or. x < xx(1)) then
       locate_dp = -1
    else
       locate_dp = jl
    end if

  end function locate_dp


  !**********************************************************************!
  ! ipos : find bin corresponding to value x
  !**********************************************************************!

  integer pure function ipos_dp(xmin,xmax,x,nbin)

    implicit none

    real(dp),intent(in) :: xmin,xmax
    ! range of values

    real(dp),intent(in) :: x
    ! the value to bin

    integer,intent(in) :: nbin
    ! number of bins

    real(dp) :: frac
    
    if(xmax > xmin) then

    if(x < xmin) then
       ipos_dp = 0
    else if(x > xmax) then
       ipos_dp = nbin+1
    else
       frac=(x-xmin)/(xmax-xmin)
       ipos_dp=int(frac*real(nbin))+1
    end if
    
    else
      
      if(x < xmax) then
         ipos_dp = 0
      else if(x > xmin) then
         ipos_dp = nbin+1
      else
         frac=(x-xmin)/(xmax-xmin)
         ipos_dp=int(frac*real(nbin))+1
      end if 
      
    end if

  end function ipos_dp

  !**********************************************************************!
  ! Find central value of bin i
  !**********************************************************************!

  real(dp) pure function xval_dp(xmin,xmax,i,nbin)

    implicit none

    real(dp),intent(in) :: xmin,xmax
    ! range of values

    integer,intent(in) :: i
    ! the bin number

    integer,intent(in) :: nbin
    ! number of bins

    real(dp) :: frac

    frac=(real(i-1)+0.5)/real(nbin)

    xval_dp=frac*(xmax-xmin)+xmin

  end function xval_dp

 
    !********************************************************************************
  ! Count up items in 1D list into 1D array
  !********************************************************************************

  pure subroutine list2hist_dp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)

    implicit none

    real(dp),dimension(:),intent(in) :: array
    real(dp),dimension(:),intent(in),optional :: weights
    ! the array of values to bin

    real(dp),intent(in) :: xmin,xmax
    ! the range of the histogram

    integer,intent(in) :: nbin
    ! number of bins

    real(dp),dimension(nbin),intent(out) :: hist_x,hist_y
    ! the histogram

    integer :: i,ibin
    ! binning variables

    logical,optional,intent(in) :: mask(:)
    logical,allocatable:: keep(:)

    allocate(keep(size(array)))

    if(present(mask)) then
       keep = mask
    else
       keep = .true.
    end if

    hist_x=0._dp ; hist_y=0._dp

    do i=1,size(array)
       if(keep(i)) then
          ibin=ipos(xmin,xmax,array(i),nbin)
          if(ibin.ge.1.and.ibin.le.nbin) then
            if(present(weights)) then
             hist_y(ibin)=hist_y(ibin)+weights(i)
           else
             hist_y(ibin)=hist_y(ibin)+1._dp
           end if
          end if
       end if
    end do

    do ibin=1,nbin
       hist_x(ibin)=xval(xmin,xmax,ibin,nbin)
    end do

    deallocate(keep)

  end subroutine list2hist_dp


  subroutine list2array_dp(n,x,y,xmin,xmax,ymin,ymax,nx,ny,array,mask_in)

    implicit none

    integer,intent(in) :: n
    ! number of values to bin

    real(dp),dimension(n),intent(in) :: x,y
    logical,dimension(n),intent(in),optional :: mask_in
    ! the array of values to bin

    real(dp),intent(in) :: xmin,xmax,ymin,ymax
    ! the range of the histogram

    integer,intent(in) :: nx,ny
    ! number of bins

    real(dp),dimension(nx,ny),intent(out) :: array
    ! the histogram

    integer :: i,ix,iy
    ! binning variables

    logical,dimension(n) :: mask

    if(present(mask_in)) then
       mask = mask_in
    else
       mask = .true.
    end if

    array = 0._dp

    do i=1,n

       if(mask(i)) then

          ix=ipos(xmin,xmax,x(i),nx)
          iy=ipos(ymin,ymax,y(i),ny)

          if(ix.ge.1.and.ix.le.nx) then
             if(iy.ge.1.and.iy.le.ny) then
                array(ix,iy) = array(ix,iy) + 1.
             end if
          end if

       end if

    end do

  end subroutine list2array_dp


  real(sp) function integral_all_sp(x,y)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_sp = integral_sp(x,y,x1,x2)
  end function integral_all_sp

  real(sp) function integral_sp(x,y,x1,x2)

    implicit none

    real(sp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(sp) :: f1,f2,sum
    integer :: j
    ! loop variable

    ! ### Find position in x array under x1 ### !

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_sp=0._sp
       return
    end if

    if(x1.gt.x(1)) then
      i1=locate(x,x1)
      f1 = interpolate_linear(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interpolate_linear(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+0.5_sp*(y(j)+y(j+1))*(x(j+1)-x(j))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+0.5_sp*(f1+y(i1+1))*(x(i1+1)-x1)
       if(x2.lt.x(size(x))) sum=sum+0.5_sp*(f2+y(i2))*(x2-x(i2))

       integral_sp=real(sum)

    else

       integral_sp=0.5_sp*(f1+f2)*(x2-x1)

    end if

  end function integral_sp
  
  real(sp) function integral_all_log_sp(x,y)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_log_sp = integral_log_sp(x,y,x1,x2)
  end function integral_all_log_sp
  

  real(sp) function integral_log_sp(x,y,x1,x2)

    implicit none

    real(sp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(sp) :: f1,f2,sum
    integer :: j
    ! loop variable

    ! ### Find position in x array under x1 ### !

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_log_sp=0._sp
       return
    end if

    if(x1.gt.x(1)) then
      i1=locate(x,x1)
      f1 = interpolate_log(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interpolate_log(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+trapezium_log(x(j),y(j),x(j+1),y(j+1))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+trapezium_log(x1,f1,x(i1+1),y(i1+1))
       if(x2.lt.x(size(x))) sum=sum+trapezium_log(x(i2),y(i2),x2,f2)

       integral_log_sp=real(sum)

    else

       integral_log_sp=trapezium_log(x1,f1,x2,f2)

    end if
    
  contains
    
    real(sp) function trapezium_log(x1,y1,x2,y2)
      implicit none
      real(sp),intent(in) :: x1,y1,x2,y2
      real(sp) :: b
      if(x1==x2) then
        trapezium_log = 0.
      else
        b = log10(y1/y2) / log10(x1/x2)
        trapezium_log = y1 * (x2*(x2/x1)**b-x1) / (b+1)
      end if
    end function trapezium_log
    
  end function integral_log_sp
      
  !**********************************************************************!
  ! This function is used to interpolate
  !
  ! Versions: 
  !
  ! Original code : TR
  !**********************************************************************!


  real(sp) function interpolate_log_sp(x,y,xval)
    
    use base_messages
    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    character(len=100) :: text
    ! error message

    real(sp) :: frac
    ! temporary fraction
    
    real(sp) :: x1,x2,y1,y2
    
    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
       call error("interpolate",text)
    end if
       
    if( ipos < n .and. ipos > 0) then
      x1 = log10(x(ipos))
      x2 = log10(x(ipos+1))
      y1 = log10(y(ipos))
      y2 = log10(y(ipos+1))
       frac = ( log10(xval) - x1 ) / ( x2 - x1 )
       interpolate_log_sp = y1 + frac * ( y2 - y1 )
       interpolate_log_sp = 10._sp**(interpolate_log_sp)
    else if(ipos == n) then
       interpolate_log_sp = y(n)
    else if(ipos == 0) then
       interpolate_log_sp = y(1)
    else
       write(text,'("Unexpected value of ipos : ",I0)') ipos
       call error("interpolate",text)
    end if
    
  end function interpolate_log_sp
  
  real(sp) function interpolate_log_old_sp(x,y,xval)
  
    implicit none
    
    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y
    
    real(sp),dimension(size(x)) :: x_temp,y_temp
    ! the x and y arrays

    real(sp) :: xval_temp,yval_temp
    ! the value at which to interpolate y

    x_temp = log10(x)
    xval_temp = log10(xval)

    where(y > 0.)
      y_temp = log10(y)
    elsewhere
      y_temp = -huge(y_temp)
    end where
    
    yval_temp = interpolate_linear_sp(x_temp,y_temp,xval_temp)
    
    interpolate_log_old_sp = 10._sp**(yval_temp)
    
  end function interpolate_log_old_sp

  real(sp) function interpolate_linear_sp(x,y,xval)
    
    use base_messages
    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    character(len=100) :: text
    ! error message

    real(sp) :: frac
    ! temporary fraction
    
    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
       call error("interpolate",text)
    end if
       
    if( ipos < n .and. ipos > 0) then
       frac = ( xval - x(ipos) ) / ( x(ipos+1) - x(ipos) )
       interpolate_linear_sp = y(ipos) + frac * ( y(ipos+1) - y(ipos) )
    else if(ipos == n) then
       interpolate_linear_sp = y(n)
    else if(ipos == 0) then
       interpolate_linear_sp = y(1)
    else
       write(text,'("Unexpected value of ipos : ",I0)') ipos
       call error("interpolate",text)
    end if
    
  end function interpolate_linear_sp
  
  function interpolate_bilinear_sp(x,y,array,x0,y0) result(value)

    use base_messages
    implicit none

    real(sp),intent(in) :: x(:),y(:),array(:,:),x0,y0
    real(sp) :: value,norm
    integer :: i1,i2,j1,j2
    character(len=100) :: text

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
       call error("interpolate",text)
    end if

    if(j1==-1) then
       write(text,'("Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
       call error("interpolate",text)
    end if
    
    norm = 1._sp / (x(i2) - x(i1)) / (y(j2)-y(j1))
    
    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
    &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
    &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
    &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interpolate_bilinear_sp

  !**********************************************************************!
  ! This function is used to locate the position in an array
  !
  ! Versions: 
  !
  ! Original code : Numerical Recipes
  ! April 2007    : TR - changed variable types
  !                      added out of bounds clause
  !**********************************************************************!

  integer function locate_sp(xx,x)

    implicit none
    real(sp), dimension(:), intent(in) :: xx
    real(sp), intent(in) :: x
    integer :: n,jl,jm,ju
    logical :: ascnd
    n=size(xx)
    ascnd = (xx(n) >= xx(1))
    jl=0
    ju=n+1
    do
       if (ju-jl <= 1) exit
       jm=(ju+jl)/2
       if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
       else
          ju=jm
       end if
    end do

    if (x == xx(1)) then
       locate_sp = 1
    else if (x == xx(n)) then
       locate_sp = n-1
    else if(x > xx(n) .or. x < xx(1)) then
       locate_sp = -1
    else
       locate_sp = jl
    end if

  end function locate_sp


  !**********************************************************************!
  ! ipos : find bin corresponding to value x
  !**********************************************************************!

  integer pure function ipos_sp(xmin,xmax,x,nbin)

    implicit none

    real(sp),intent(in) :: xmin,xmax
    ! range of values

    real(sp),intent(in) :: x
    ! the value to bin

    integer,intent(in) :: nbin
    ! number of bins

    real(sp) :: frac
    
    if(xmax > xmin) then

    if(x < xmin) then
       ipos_sp = 0
    else if(x > xmax) then
       ipos_sp = nbin+1
    else
       frac=(x-xmin)/(xmax-xmin)
       ipos_sp=int(frac*real(nbin))+1
    end if
    
    else
      
      if(x < xmax) then
         ipos_sp = 0
      else if(x > xmin) then
         ipos_sp = nbin+1
      else
         frac=(x-xmin)/(xmax-xmin)
         ipos_sp=int(frac*real(nbin))+1
      end if 
      
    end if

  end function ipos_sp

  !**********************************************************************!
  ! Find central value of bin i
  !**********************************************************************!

  real(sp) pure function xval_sp(xmin,xmax,i,nbin)

    implicit none

    real(sp),intent(in) :: xmin,xmax
    ! range of values

    integer,intent(in) :: i
    ! the bin number

    integer,intent(in) :: nbin
    ! number of bins

    real(sp) :: frac

    frac=(real(i-1)+0.5)/real(nbin)

    xval_sp=frac*(xmax-xmin)+xmin

  end function xval_sp

 
    !********************************************************************************
  ! Count up items in 1D list into 1D array
  !********************************************************************************

  pure subroutine list2hist_sp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)

    implicit none

    real(sp),dimension(:),intent(in) :: array
    real(sp),dimension(:),intent(in),optional :: weights
    ! the array of values to bin

    real(sp),intent(in) :: xmin,xmax
    ! the range of the histogram

    integer,intent(in) :: nbin
    ! number of bins

    real(sp),dimension(nbin),intent(out) :: hist_x,hist_y
    ! the histogram

    integer :: i,ibin
    ! binning variables

    logical,optional,intent(in) :: mask(:)
    logical,allocatable:: keep(:)

    allocate(keep(size(array)))

    if(present(mask)) then
       keep = mask
    else
       keep = .true.
    end if

    hist_x=0._sp ; hist_y=0._sp

    do i=1,size(array)
       if(keep(i)) then
          ibin=ipos(xmin,xmax,array(i),nbin)
          if(ibin.ge.1.and.ibin.le.nbin) then
            if(present(weights)) then
             hist_y(ibin)=hist_y(ibin)+weights(i)
           else
             hist_y(ibin)=hist_y(ibin)+1._sp
           end if
          end if
       end if
    end do

    do ibin=1,nbin
       hist_x(ibin)=xval(xmin,xmax,ibin,nbin)
    end do

    deallocate(keep)

  end subroutine list2hist_sp


  subroutine list2array_sp(n,x,y,xmin,xmax,ymin,ymax,nx,ny,array,mask_in)

    implicit none

    integer,intent(in) :: n
    ! number of values to bin

    real(sp),dimension(n),intent(in) :: x,y
    logical,dimension(n),intent(in),optional :: mask_in
    ! the array of values to bin

    real(sp),intent(in) :: xmin,xmax,ymin,ymax
    ! the range of the histogram

    integer,intent(in) :: nx,ny
    ! number of bins

    real(sp),dimension(nx,ny),intent(out) :: array
    ! the histogram

    integer :: i,ix,iy
    ! binning variables

    logical,dimension(n) :: mask

    if(present(mask_in)) then
       mask = mask_in
    else
       mask = .true.
    end if

    array = 0._sp

    do i=1,n

       if(mask(i)) then

          ix=ipos(xmin,xmax,x(i),nx)
          iy=ipos(ymin,ymax,y(i),ny)

          if(ix.ge.1.and.ix.le.nx) then
             if(iy.ge.1.and.iy.le.ny) then
                array(ix,iy) = array(ix,iy) + 1.
             end if
          end if

       end if

    end do

  end subroutine list2array_sp


  subroutine invert_matrix(n,a,c)

    implicit none

    integer,intent(in) :: n
    ! size of the matrix

    real(dp),dimension(n,n),intent(in)  :: a
    real(dp),dimension(n,n),intent(out) :: c
    ! the input and output matrices

    real(dp),dimension(n,n) :: b

    integer :: i,j
    real(dp) :: scale

    ! a is the input matrix

    ! b is initially set to a

    b = a

    ! c is initially an identity matrix

    c = 0._dp
    forall(i=1:n) c(i,i) = 1._dp

    ! Go through each row, and sutract from all subsequent rows

    do i=1,n
       do j=i+1,n
          scale = -b(i,j)/b(i,i)
          b(:,j) = b(:,j)+scale*b(:,i)
          c(:,j) = c(:,j)+scale*c(:,i)
       end do
    end do

    do j=1,n-1
       do i=j+1,n
          scale = -b(i,j)/b(i,i)
          b(:,j) = b(:,j)+scale*b(:,i)
          c(:,j) = c(:,j)+scale*c(:,i)
       end do
    end do

    forall(i=1:n,j=1:n) c(i,j) = c(i,j)/b(j,j)
    forall(i=1:n,j=1:n) b(i,j) = b(i,j)/b(j,j)

  end subroutine invert_matrix

  subroutine print_matrix(a)

    implicit none

    integer :: nx,ny
    real(dp),dimension(:,:),intent(in) :: a
    
    integer :: i

    character(len=20) :: fmt

    nx = size(a,1)
    ny = size(a,2)

    write(fmt,'("(",I3.3,"(F9.5,1X))")') nx

    do i=1,ny
       write(*,fmt) a(:,i)
    end do

end subroutine print_matrix

subroutine bin_array(n,x,y,n_bin,xmin,xmax,x_bin,y_bin)

  implicit none

  integer,intent(in) :: n,n_bin
  real(dp),dimension(n),intent(in) :: x,y
  real(dp),dimension(n_bin),intent(out) :: x_bin,y_bin
  
  real(dp),dimension(n_bin) :: s,c

  real(dp) :: xmin,xmax
  
  integer :: i,ix

  s = 0.
  c = 0.

  do i=1,n
     
     ix = ipos(xmin,xmax,x(i),n_bin)

     if(ix.ge.1.and.ix.le.n_bin) then
        c(ix)  = c(ix) + 1.
        s(ix)  = s(ix) + y(i)
     end if

  end do

  do i=1,n_bin
     x_bin(i) = xval(xmin,xmax,i,n_bin)
  end do

  y_bin = s / c

end subroutine bin_array

subroutine smooth_2d(array,sigma)

  implicit none

  real(sp),intent(inout) :: array(:,:)

  integer :: nx,ny
  real(sp),allocatable :: array_orig(:,:),array_count(:,:)

  real(sp),intent(in) :: sigma

  integer :: i,j,ii,jj,imin,imax,jmin,jmax,w

  real(sp) :: dx,dy,d

  nx = size(array,1)
  ny = size(array,2)

  allocate(array_orig(nx,ny),array_count(nx,ny))
  
  array_orig  = array
  array       = 0.
  array_count = 0.

  w = nint(sigma * 5.)

  do i=1,nx
     do j=1,ny

        imin = max( 1,i-w)
        imax = min(nx,i+w)
        jmin = max( 1,j-w)
        jmax = min(ny,j+w)

        do ii=imin,imax
           do jj=jmin,jmax

              dx = real(ii-i) / sigma
              dy = real(jj-j) / sigma

              d = dx*dx+dy*dy

              array(i,j) = array(i,j) + array_orig(ii,jj) * exp(-d/2.)
              array_count(i,j) = array_count(i,j) + exp(-d/2.)

           end do
        end do

     end do
  end do

  array = array / array_count

end subroutine smooth_2d

end module base_array
