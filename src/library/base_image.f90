module base_image

  use base_types

  implicit none
  save

  private
  public :: delete_image
  public :: initialize_image
  public :: add_line,add_segment
  public :: plot_image
  public :: clip_image
  public :: add_disk
  public :: image
  public :: resample_image

  type image
     integer :: nx,ny
     real(sp),allocatable :: value(:,:)
     real(sp) :: xmin,xmax,ymin,ymax
     real(sp) :: vxmin,vxmax,vymin,vymax
  end type image

contains

  subroutine add_line(im,n,x,y)
    implicit none
    type(image),intent(inout) :: im
    integer,intent(in) :: n
    real(sp),intent(in) :: x(n),y(n)
    integer :: i
    do i=1,n-1
       call add_segment(im,x(i),y(i),x(i+1),y(i+1))
    end do
  end subroutine add_line

  subroutine delete_image(im)
    implicit none
    type(image),intent(inout) :: im
    if(allocated(im%value)) deallocate(im%value)
    im%nx = 0
    im%ny = 0
  end subroutine delete_image

  subroutine initialize_image(im,nxt,nyt)
    implicit none
    type(image),intent(inout) :: im
    integer,intent(in)     :: nxt,nyt
    call pgqwin(im%xmin,im%xmax,im%ymin,im%ymax)
    call pgqvp(1,im%vxmin,im%vxmax,im%vymin,im%vymax)
    call delete_image(im)
    allocate(im%value(nxt,nyt))
    im%value = 0. ; im%nx = nxt ; im%ny = nyt
  end subroutine initialize_image

  subroutine clip_image(im,level)
    implicit none
    type(image),intent(inout) :: im
    real(sp),intent(in) :: level
    where(im%value > level)
       im%value = level
    end where
  end subroutine clip_image

  subroutine plot_image(im,min_level,max_level)

    use base_pgplot
    implicit none
    type(image),intent(in) :: im
    real(sp),intent(in) :: min_level,max_level
    real(sp) :: tr(6)

    call find_tr_matrix(im%nx,im%xmin,im%xmax,im%ny,im%ymin,im%ymax,tr)
    call pggray(im%value,im%nx,im%ny,1,im%nx,1,im%ny,min_level,max_level,tr)

  end subroutine plot_image

  subroutine resample_image(im,im_new,resample)
    use base_messages
    implicit none
    type(image),intent(in) :: im
    type(image),intent(out) :: im_new
    integer,intent(in) :: resample
    integer :: nxr,nyr
    integer :: i,j,inew,jnew
    if(mod(im%nx,resample).ne.0.or.mod(im%ny,resample).ne.0) then
       call error("plot_image","nx and ny should be multiples of resample")
    end if

    nxr = im%nx / resample ; nyr = im%ny / resample

    im_new = im
    call delete_image(im_new)
    allocate(im_new%value(nxr,nyr))
    im_new%nx = nxr
    im_new%ny = nyr
    im_new%value = 0.

    do inew=1,nxr
       do jnew=1,nyr
             
          do i=resample*(inew-1)+1,resample*inew
             do j=resample*(jnew-1)+1,resample*jnew
                im_new%value(inew,jnew) = im_new%value(inew,jnew) + im%value(i,j)
             end do
          end do
          
       end do
    end do
    
    im_new%value = im_new%value / real(resample)**2.

  end subroutine resample_image

  subroutine add_disk(im,x,y,r)
    implicit none
    type(image),intent(inout) :: im
    real(sp),intent(in) :: x,y,r
    real(sp) :: xpix,ypix,rpix,di,dj
    integer :: imin,imax,jmin,jmax
    integer :: i,j

    ! Convert world to pixel
    xpix = (x-im%xmin)/(im%xmax-im%xmin)*real(im%nx)
    ypix = (y-im%ymin)/(im%ymax-im%ymin)*real(im%ny)
    ! Convert inches to pixel
    rpix = r / ((im%vxmax-im%vxmin) / real(im%nx))
    ! Add all pixels
    imin = floor(xpix-rpix) ; imax = ceiling(xpix+rpix)
    jmin = floor(ypix-rpix) ; jmax = ceiling(ypix+rpix)
    do i=imin,imax
       di = xpix - real(i)
       if(abs(di) < rpix+1.) then 
          do j=jmin,jmax
             dj = ypix - real(j)
             if(abs(dj) < rpix + 1.) then
                if(di*di+dj*dj < rpix*rpix) call add_pixel(im,i,j,1.)
             end if
          end do
       end if
    end do
    
  end subroutine add_disk

  subroutine add_segment(im,x1,y1,x2,y2)
    use base_messages
    implicit none
    type(image),intent(inout) :: im
    real(sp),intent(in) :: x1,x2,y1,y2
    real(sp) :: xpix1,xpix2,ypix1,ypix2

    if(.not.allocated(im%value)) call error("plot_line","image not initialized")

    xpix1 = real(im%nx) * (x1 - im%xmin) / (im%xmax - im%xmin)
    ypix1 = real(im%ny) * (y1 - im%ymin) / (im%ymax - im%ymin)
    xpix2 = real(im%nx) * (x2 - im%xmin) / (im%xmax - im%xmin)
    ypix2 = real(im%ny) * (y2 - im%ymin) / (im%ymax - im%ymin)

    call add_segment_wu(im,xpix1,ypix1,xpix2,ypix2)
    
  end subroutine add_segment

  subroutine add_segment_wu(im,x1o,y1o,x2o,y2o)

    ! Based on pseudo-code from wikipedia on Xiaolin Wu's line algorithm

    use base_array
    implicit none

    type(image),intent(inout) :: im
    real(sp),intent(in) :: x1o,y1o,x2o,y2o
    real(sp) :: x1,y1,x2,y2


    !    real(sp) :: x,y,ypix
    !    integer :: j1,j2,i,jprev
    !    real(sp) :: frac,l1,l2
    !    integer :: nx,ny

    real(sp) :: dx,dy,gradient

    real(sp) :: xend,yend,xgap,intery,ygap,interx

    integer :: xpxl1,ypxl1
    integer :: xpxl2,ypxl2
    integer :: x,y
    x1 = x1o ; y1 = y1o ; x2 = x2o ; y2 = y2o

    !   nx = size(image,1)
    !   ny = size(image,2)

    dx = x2 - x1
    dy = y2 - y1

    if(abs(dx) > abs(dy)) then

       ! = Horizontal line =

       if(x2 < x1) then
          call swap(x1,x2)
          call swap(y1,y2)
       end if
       gradient = dy / dx

       ! Check if both start and end positions are in the same pixel

       if(round(x1)==round(x2)) then

          xend = round(x1)
          yend = y1 + gradient * (xend-x1)
          xgap = x2-x1
          xpxl1 = nint(xend)
          ypxl1 = ipart(yend)
          call add_pixel(im,xpxl1,ypxl1,  rfpart(yend)*xgap)
          call add_pixel(im,xpxl1,ypxl1+1, fpart(yend)*xgap)

       else

          ! First end-point

          xend = round(x1)
          yend = y1 + gradient * (xend-x1)
          xgap = rfpart(x1+0.5)
          xpxl1 = nint(xend)
          ypxl1 = ipart(yend)
          call add_pixel(im,xpxl1,ypxl1,  rfpart(yend)*xgap)
          call add_pixel(im,xpxl1,ypxl1+1, fpart(yend)*xgap)
          intery = yend + gradient

          ! Second end-point

          xend = round(x2)
          yend = y2 + gradient * (xend-x2)
          xgap = fpart(x2+0.5)
          xpxl2 = nint(xend)
          ypxl2 = ipart(yend)
          call add_pixel(im,xpxl2,ypxl2,  rfpart(yend)*xgap)
          call add_pixel(im,xpxl2,ypxl2+1, fpart(yend)*xgap)

          do x = xpxl1+1,xpxl2-1
             call add_pixel(im,x,ipart(intery),  rfpart(intery))
             call add_pixel(im,x,ipart(intery)+1, fpart(intery))
             intery = intery + gradient
          end do

       end if

    else

       ! = Vertical line =

       if(y2 < y1) then
          call swap(x1,x2)
          call swap(y1,y2)
       end if
       gradient = dx / dy

       ! Check if both start and end positions are in the same pixel

       if(round(y1)==round(y2)) then

          yend = round(y1)
          xend = x1 + gradient * (yend-y1)
          ygap = y2-y1
          ypxl1 = nint(yend)
          xpxl1 = ipart(xend)
          call add_pixel(im,xpxl1  ,ypxl1,rfpart(xend)*ygap)
          call add_pixel(im,xpxl1+1,ypxl1, fpart(xend)*ygap)

       else

          ! First end-point

          yend = round(y1)
          xend = x1 + gradient * (yend-y1)
          ygap = rfpart(y1+0.5)
          ypxl1 = nint(yend)
          xpxl1 = ipart(xend)
          call add_pixel(im,xpxl1,  ypxl1,rfpart(xend)*ygap)
          call add_pixel(im,xpxl1+1,ypxl1, fpart(xend)*ygap)
          interx = xend + gradient

          ! Second end-point

          yend = round(y2)
          xend = x2 + gradient * (yend-y2)
          ygap = fpart(y2+0.5)
          ypxl2 = nint(yend)
          xpxl2 = ipart(xend)
          call add_pixel(im,xpxl2,  ypxl2,rfpart(xend)*ygap)
          call add_pixel(im,xpxl2+1,ypxl2, fpart(xend)*ygap)

          do y = ypxl1+1,ypxl2-1
             call add_pixel(im,ipart(interx),  y,rfpart(interx))
             call add_pixel(im,ipart(interx)+1,y, fpart(interx))
             interx = interx + gradient
          end do

       end if


    end if

  end subroutine add_segment_wu

  subroutine add_pixel(im,i,j,c)
    implicit none
    type(image),intent(inout) :: im
    integer,intent(in) :: i,j
    real(sp),intent(in) :: c
    if(i < 1 .or. i > im%nx) return
    if(j < 1 .or. j > im%ny) return
    im%value(i,j) = im%value(i,j)+c
  end subroutine add_pixel

  pure real function round(a)
    implicit none
    real(sp),intent(in) :: a
    round = nint(a)
  end function round

  pure integer function ipart(a)
    implicit none
    real(sp),intent(in) :: a
    ipart = int(a)
  end function ipart

  pure real function fpart(a)
    implicit none
    real(sp),intent(in)  :: a
    fpart = a - aint(a)
  end function fpart

  pure real function rfpart(a)
    implicit none
    real(sp),intent(in)  :: a
    rfpart = aint(a) + 1. - a
  end function rfpart

  subroutine swap(a,b)
    implicit none
    real(sp),intent(inout) :: a,b
    real(sp) :: c
    c = b
    b = a
    a = c
  end subroutine swap

end module base_image

