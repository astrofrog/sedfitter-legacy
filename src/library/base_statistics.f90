!#############################################################################################
!#                                         SYNOPSIS                                          #
!#############################################################################################


!#############################################################################################

module base_statistics

  use base_array

  implicit none
  save

  integer,parameter,private :: sp = selected_real_kind(6,30)
  integer,parameter,private :: dp = selected_real_kind(15,100)

  private
  public :: mean_1d,clip_mean_1d,median_1d,quantile_1d,variance_1d
  

  real(dp),private :: summ
  ! summation variable

  interface mean_1d
     module procedure mean_1d_real8
     module procedure mean_1d_real4
  end interface

  interface clip_mean_1d
     module procedure clip_mean_1d_real8
     module procedure clip_mean_1d_real4
  end interface

  interface median_1d
     module procedure median_1d_real8
     module procedure median_1d_real4
  end interface

  interface quantile_1d
     module procedure quantile_1d_real8
     module procedure quantile_1d_real4
  end interface

  interface variance_1d
     module procedure variance_1d_real8
     module procedure variance_1d_real4
  end interface


contains


 
  !#############################################################################################
  !#                                        STATISTICS                                         #
  !#############################################################################################

  !********************************************************************************
  ! Mean
  !********************************************************************************


  real(dp) function mean_1d_real8(n,x)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(dp),dimension(n),intent(in) :: x
    ! the array

    integer :: i
    ! loop variable

    summ=0.d0

    do i=1,n
       summ=summ+dble(x(i))
    end do

    mean_1d_real8=real(summ)/real(n)

  end function mean_1d_real8

  !********************************************************************************
  ! Median
  !********************************************************************************

  real(dp) function median_1d_real8(n,x)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(dp),dimension(n),intent(in) :: x
    ! the array

    real(dp),dimension(n) :: x_sorted
    ! temporary sorted array

    integer,dimension(n) :: sort_index
    ! temporary index array for sorting

    call index_array_1d(n,x,sort_index)
    x_sorted=x(sort_index)

    if (mod(n,2).eq.0) then
       median_1d_real8=(x_sorted(n/2)+x_sorted(n/2+1))/2.
    else
       median_1d_real8=x_sorted((n-1)/2+1)
    end if

  end function median_1d_real8

  !********************************************************************************
  ! Quantile
  !********************************************************************************

  real(dp) function quantile_1d_real8(n,x,fraction)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(dp),dimension(n),intent(in) :: x
    ! the array

    real(dp),dimension(n) :: x_sorted
    ! temporary sorted array

    integer,dimension(n) :: sort_index
    ! temporary index array for sorting

    real(dp),intent(in) :: fraction
    ! the fraction to take the quantile at

    integer :: ipos
    ! temporary position in the array

    call index_array_1d(n,x,sort_index)
    x_sorted=x(sort_index)

    ipos=nint(fraction*real(n-1))+1

    quantile_1d_real8=x_sorted(ipos)

  end function quantile_1d_real8

  !********************************************************************************
  ! Variance
  !********************************************************************************

  real(dp) function variance_1d_real8(n,x,xbar)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(dp),dimension(n),intent(in) :: x
    ! the array

    real(dp),intent(in) :: xbar
    ! the average value of the array

    integer :: i
    ! loop variable

    summ=0.d0

    do i=1,n
       summ=summ+(dble(x(i))-dble(xbar))**2.d0
    end do

    variance_1d_real8=real(summ)/real(n-1)

  end function variance_1d_real8

  !********************************************************************************
  ! Clipped mean
  !********************************************************************************

  real(dp) function clip_mean_1d_real8(n,x,n_iter)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(dp),dimension(n),intent(in) :: x
    ! the array

    integer :: i
    ! loop variable

    integer :: iter
    integer,dimension(n) :: valid

    real(dp) :: sigma

    real(dp),dimension(n) :: temp
    integer :: c

    integer,optional :: n_iter
    integer :: ni

    if(present(n_iter)) then
       ni = n_iter
    else
       ni = 10
    end if
    
    valid=1

    do iter=1,ni

       summ=0.d0

       temp=0
       c=0

       do i=1,n
          if(valid(i)==1) then
             summ=summ+dble(x(i))
             c=c+1
             temp(c)=x(i)
          end if
       end do

       clip_mean_1d_real8=real(summ)/real(sum(real(valid(:))))

       sigma=sqrt(variance_1d(c,temp(1:c),clip_mean_1d_real8))

       do i=1,n
          if(x(i).gt.clip_mean_1d_real8+3.*sigma) valid(i)=0
          if(x(i).lt.clip_mean_1d_real8-3.*sigma) valid(i)=0
       end do

    end do

  end function clip_mean_1d_real8


  real(sp) function mean_1d_real4(n,x)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(sp),dimension(n),intent(in) :: x
    ! the array

    integer :: i
    ! loop variable

    summ=0.d0

    do i=1,n
       summ=summ+dble(x(i))
    end do

    mean_1d_real4=real(summ)/real(n)

  end function mean_1d_real4

  !********************************************************************************
  ! Median
  !********************************************************************************

  real(sp) function median_1d_real4(n,x)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(sp),dimension(n),intent(in) :: x
    ! the array

    real(sp),dimension(n) :: x_sorted
    ! temporary sorted array

    integer,dimension(n) :: sort_index
    ! temporary index array for sorting

    call index_array_1d(n,x,sort_index)
    x_sorted=x(sort_index)

    if (mod(n,2).eq.0) then
       median_1d_real4=(x_sorted(n/2)+x_sorted(n/2+1))/2.
    else
       median_1d_real4=x_sorted((n-1)/2+1)
    end if

  end function median_1d_real4

  !********************************************************************************
  ! Quantile
  !********************************************************************************

  real(sp) function quantile_1d_real4(n,x,fraction)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(sp),dimension(n),intent(in) :: x
    ! the array

    real(sp),dimension(n) :: x_sorted
    ! temporary sorted array

    integer,dimension(n) :: sort_index
    ! temporary index array for sorting

    real(sp),intent(in) :: fraction
    ! the fraction to take the quantile at

    integer :: ipos
    ! temporary position in the array

    call index_array_1d(n,x,sort_index)
    x_sorted=x(sort_index)

    ipos=nint(fraction*real(n-1))+1

    quantile_1d_real4=x_sorted(ipos)

  end function quantile_1d_real4

  !********************************************************************************
  ! Variance
  !********************************************************************************

  real(sp) function variance_1d_real4(n,x,xbar)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(sp),dimension(n),intent(in) :: x
    ! the array

    real(sp),intent(in) :: xbar
    ! the average value of the array

    integer :: i
    ! loop variable

    summ=0.d0

    do i=1,n
       summ=summ+(dble(x(i))-dble(xbar))**2.d0
    end do

    variance_1d_real4=real(summ)/real(n-1)

  end function variance_1d_real4

  !********************************************************************************
  ! Clipped mean
  !********************************************************************************

  real(sp) function clip_mean_1d_real4(n,x,n_iter)

    implicit none

    integer,intent(in) :: n
    ! the size of the input array

    real(sp),dimension(n),intent(in) :: x
    ! the array

    integer :: i
    ! loop variable

    integer :: iter
    integer,dimension(n) :: valid

    real(sp) :: sigma

    real(sp),dimension(n) :: temp
    integer :: c

    integer,optional :: n_iter
    integer :: ni

    if(present(n_iter)) then
       ni = n_iter
    else
       ni = 10
    end if
    
    valid=1

    do iter=1,ni

       summ=0.d0

       temp=0
       c=0

       do i=1,n
          if(valid(i)==1) then
             summ=summ+dble(x(i))
             c=c+1
             temp(c)=x(i)
          end if
       end do

       clip_mean_1d_real4=real(summ)/real(sum(real(valid(:))))

       sigma=sqrt(variance_1d(c,temp(1:c),clip_mean_1d_real4))

       do i=1,n
          if(x(i).gt.clip_mean_1d_real4+3.*sigma) valid(i)=0
          if(x(i).lt.clip_mean_1d_real4-3.*sigma) valid(i)=0
       end do

    end do

  end function clip_mean_1d_real4


end module base_statistics
