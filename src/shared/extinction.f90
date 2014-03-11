module extinction

  use base_types
  use base_io
  use base_array

  implicit none
  save

  private
  public :: set_av_file
  public :: av_law
  public :: load_av_law

  character(len=1000) :: av_file
  ! av file
  
  real(sp),allocatable :: av_wav(:)
  ! wavelengths at which the extinction law is defined

  real(sp),allocatable :: av_law(:)
  ! The extinction law

contains
  
  subroutine set_av_file(filename)
    use base_messages
    implicit none
    character(len=*),intent(in) :: filename
    av_file=filename
    
    call message_section("Interstellar extinction")          

    write(*,'(" Extinction file : ")',advance='no')
    write(*,*) trim(av_file)

  end subroutine set_av_file

  subroutine load_av_law(wav)
    implicit none

    real(sp),intent(in) :: wav(:)

    real(sp),allocatable :: kappa(:)
    real(sp) :: kappa_v
    ! the opacity values, and the opacity at V band

    real(sp),allocatable :: wavelength_orig(:),kappa_orig(:)
    ! wavelength and opacity from file

    integer :: i
    ! loop variable

    if(allocated(av_wav)) then
      if(size(wav)==size(av_wav)) then
        if(all(abs(wav-av_wav)/wav<0.0001)) return
      end if
    end if
    
    write(*,*) "-> updating extinction using "//trim(av_file)

      if(allocated(av_wav)) deallocate(av_wav)
      if(allocated(av_law)) deallocate(av_law)

      allocate(kappa(size(wav)))
      allocate(av_wav(size(wav)))
      allocate(av_law(size(wav)))

      call read_column(av_file,1,wavelength_orig)
      call read_column(av_file,4,kappa_orig)

      do i=1,size(wav)
        if(wav(i) < wavelength_orig(1)) then
          kappa(i) = kappa_orig(1)
        else if(wav(i) > wavelength_orig(size(wavelength_orig))) then
          kappa(i) = kappa_orig(size(kappa_orig))
        else
          kappa(i) = interpolate_linear(wavelength_orig,kappa_orig,wav(i))
        end if
      end do

      kappa_v = interpolate_linear(wavelength_orig,kappa_orig,0.550)

      av_wav(:) = wav(:)
      av_law(:) = -0.4*kappa(:)/kappa_v

      deallocate(kappa)

    end subroutine load_av_law

  end module extinction