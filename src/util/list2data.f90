program list2data
  
  use base_io
  use base_source
  use base_messages
  
  implicit none
  
  character(len=100) :: in_file,out_file

  type(source) :: s
  integer :: j
  
  type(filter),allocatable :: f(:)
  
  integer :: flux_type,imin
  
  character(len=20) :: fmt
  character(len=100) :: models_dir
  
  character(len=5),allocatable :: m_filters(:)
  real,allocatable             :: m_wav(:)
  
  in_file  = char_arg('input')
  if(present_arg('models')) then
    models_dir = char_arg('models')
    call read_column(trim(models_dir)//'/monochromatic_filters.txt',1,m_filters)
    call read_column(trim(models_dir)//'/monochromatic_filters.txt',2,m_wav)
  end if
  
  s%name   = char_arg('name')
  s%x      = 0.
  s%y      = 0.
  
  call set_source_size(s,file_n_lines(in_file))
  call check_source_allocation(s)
  allocate(f(s%n_wav))
  
  open(unit=10,file=in_file)
  do j=1,s%n_wav
    read(10,*) flux_type
    backspace(10)
    if(flux_type==1) then
      read(10,*) flux_type,f(j)%name,s%valid(j),s%flux(j),s%flux_error(j),f(j)%aperture_arcsec
    else
      if(.not.present_arg('models')) call error("list2data","need to specify models=models_dir if monochromatic fluxes are present")
      read(10,*) flux_type,f(j)%wavelength_microns,s%valid(j),s%flux(j),s%flux_error(j),f(j)%aperture_arcsec
      imin = minloc(abs(f(j)%wavelength_microns-m_wav(:)),1)
      f(j)%name = m_filters(imin)
    end if
  end do
  close(unit=10)
  
  call print_source(s)
  
  open(unit=10,file='data_'//trim(s%name))
  call write_source_ascii(10,s)
  close(unit=10)
  
  open(unit=10,file='config_'//trim(s%name))
  write(10,'("###")')
  write(10,*) s%n_wav

  write(fmt,'("(",I3.3,"(A5,1X))")') s%n_wav
  write(10,fmt) f%name

  write(fmt,'("(",I3.3,"(F10.3,1X))")') s%n_wav
  write(10,fmt) f%aperture_arcsec
  
  close(unit=10)
  
end program list2data
  