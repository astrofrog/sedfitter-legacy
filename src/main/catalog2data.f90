program catalog2data

  use base_types
  use base_io
  use base_messages
  use base_source

  implicit none

  include 'glimpse_catalog.inc'

  character(len=1000) :: in_catalog,out_data
  ! input and output file

  integer,parameter :: n_wav = 7
  ! number of wavelengths

  type(source) :: s
  ! source variable

  integer :: source_id,n_sources
  ! number of sources and loop variable

  integer :: j
  ! loop variable

  character(len=3) :: coord_type

  logical :: constrain_coords,keep,reject_single
  real(sp) :: xmin,xmax,ymin,ymax

  integer :: unit_in,unit_out
  ! file units

  if(.not.present_arg('input'))  call display_usage
  if(.not.present_arg('output')) call display_usage
  if(.not.present_arg('coords')) call display_usage

  if(present_arg('box')) then
     if(.not.present_arg('xmin')) call display_usage
     if(.not.present_arg('xmax')) call display_usage
     if(.not.present_arg('ymin')) call display_usage
     if(.not.present_arg('ymax')) call display_usage
  end if

  ! ### GET COMMAND-LINE ARGUMENTS ### !

  in_catalog = char_arg("input")
  out_data   = char_arg("output")
  coord_type = char_arg("coords")

  if(trim(coord_type).ne.'equ'.and.trim(coord_type).ne.'gal') then
     call error("catalog2data","coordinate system should be gal or equ")
  end if

  if(present_arg('box')) then
     constrain_coords = logical_arg('box')
     if(constrain_coords) then
        xmin = real_arg('xmin')
        xmax = real_arg('xmax')
        ymin = real_arg('ymin')
        ymax = real_arg('ymax')
     end if
  else
     constrain_coords = .false.
  end if

  if(present_arg('rejectsingle')) then
     reject_single = logical_arg('rejectsingle')
  else
     reject_single = .false.
  end if

  call set_source_size(s,n_wav)

  n_sources = file_n_lines(in_catalog)

  write(*,'("---------------------------------------------------------")')
  write(*,'(" Input file  : ",A50)'),in_catalog
  write(*,'(" Output file : ",A50)'),out_data
  write(*,'("---------------------------------------------------------")')

  if(constrain_coords) then

     write(*,'(" Constraining coordinates:")')
     select case(coord_type)
     case("gal")
        write(*,'("  l min = ",F10.5)') xmin
        write(*,'("  l max = ",F10.5)') xmax
        write(*,'("  b min = ",F10.5)') ymin
        write(*,'("  b max = ",F10.5)') ymax
     case("equ")
        write(*,'("  RA min = ",F10.5)')  xmin
        write(*,'("  RA max = ",F10.5)')  xmax
        write(*,'("  Dec min = ",F10.5)') ymin
        write(*,'("  Dec max = ",F10.5)') ymax
     end select
     write(*,'("---------------------------------------------------------")')

  end if

  if(file_exists(out_data)) then
     call delete_file(out_data)
     print '("---------------------------------------------------------")'
  end if

  call open_safe(unit_in, file=in_catalog, status='old')
  call open_safe(unit_out,file=out_data,   status='new')

  do source_id=1,n_sources

     read(unit_in,catalog_format) desig,&
          &twomid,l,b,dl,db,ra,dec,dra,ddec,conf,&
          &(mag(j),dmag(j),j=1,7),(f(j),df(j),j=1,7),&
          &(rmsf(j),j=4,7),(sky(j),j=4,7),(sn(j),j=1,7),(srcdens(j),j=4,7),&
          &(m(j),j=4,7),(n(j),j=4,7),(sqf(j),j=1,7),(mf(j),j=4,7)

     s%name = adjustl(desig)

     select case(coord_type)
     case('gal')
        s%x = l ; s%y = b
     case('equ')
        s%x = ra ; s%y = dec
     end select

     where(f < 0 .or. df < 0.)
        s%valid = 0.
        s%flux  = 0.
        s%flux_error = 0.
     elsewhere
        s%valid = 1
        s%flux  = f
        s%flux_error = df
     end where

     if(reject_single) then
        where(n > 0 .and. m == 1)
           s%valid = 0.
           s%flux  = 0.
           s%flux_error = 0.
        end where
     end if

     keep=.true.

     if(constrain_coords) then
        if(s%x.gt.xmax.or.s%x.lt.xmin) keep=.false.
        if(s%y.gt.ymax.or.s%y.lt.ymin) keep=.false.
     end if

     if(keep) call write_source_ascii(unit_out,s)

  end do

  ! ### CLOSE FILES ### !

  close(unit_in)
  close(unit_out)

contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: catalog2data [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   input=filename   - the GLIMPSE catalog or archive file")')
    write(*,'("   output=filename    - the output fitter file")')
    write(*,'("   coords=value       - the coordinate system (gal/equ)")')
    write(*,*)
    write(*,'(" Optional arguments :")')
    write(*,'("   box=yes/no          - whether to constrain coordinates (default no)")')
    write(*,'("   xmin=value          - the lower x coordinate of the box")')
    write(*,'("   xmax=value          - the upper x coordinate of the box")')
    write(*,'("   ymin=value          - the lower y coordinate of the box")')
    write(*,'("   ymax=value          - the upper y coordinate of the box")')
    write(*,'("   rejectsingle=yes/no - reject single detections (default no)")')
    write(*,*)
    stop

  end subroutine display_usage

end program catalog2data
