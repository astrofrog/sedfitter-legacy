program filter_output

  use base_types
  use base_fitter_output
  use base_source
  use base_io
  use base_cfitsio
  use base_messages

  implicit none

  character(len=1000) :: in_file,out_file_good,out_file_bad
  
  type(source) :: s
  type(filter),allocatable :: filters(:)
  integer,allocatable :: model_id(:)
  character(len=30),allocatable :: model_names(:)
  real(sp),allocatable :: av(:),sc(:),chi(:),model_fluxes(:,:)
!  logical(1),allocatable :: used(:)
  character(len=1000) :: models_dir,ex_law
  character(len=1) :: out_format
  real(sp) :: out_number
  
  integer :: unit_in,unit_good,unit_bad

  logical :: present_model_fluxes

  integer :: source_id,p

  logical :: use_chi,use_cpd
  real(sp) :: limit_chi,limit_cpd

  real(sp) :: chibest,cpdbest

  integer :: n_good = 0
  integer :: n_bad  = 0
  
  integer :: n_wav
  
  if(.not.present_arg('input')) call display_usage
  
  in_file = char_arg('input')
  p = index(in_file,'.fits')
  out_file_good = in_file(1:p-1)//'_good.fits'
  out_file_bad  = in_file(1:p-1)//'_bad.fits'

  use_chi=present_arg('chi')
  use_cpd=present_arg('cpd')

  if(use_chi.eqv.use_cpd) call display_usage

  if(use_chi) limit_chi = real_arg('chi')
  if(use_cpd) limit_cpd = real_arg('cpd')

  call open_output_file_read(in_file,unit_in,n_wav,models_dir,ex_law,filters,present_model_fluxes,out_format,out_number)
  call open_output_file_new(out_file_good,unit_good,models_dir,ex_law,filters,present_model_fluxes,out_format,out_number)
  call open_output_file_new(out_file_bad, unit_bad, models_dir,ex_law,filters,present_model_fluxes,out_format,out_number)

  call set_source_size(s,n_wav)

  do source_id = 1,number_sources(unit_in)

     if(present_model_fluxes) then
        call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,"A",0.,model_fluxes=model_fluxes)
     else
        call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,"A",0.)
     end if
     
!     call read_models_used(unit_in,source_id,used)
     
     chibest = minval(chi)
     cpdbest = chibest / real(s%n_data)

     if((use_chi.and.chibest < limit_chi).or.(use_cpd.and.cpdbest < limit_cpd)) then
        n_good = n_good + 1
        if(present_model_fluxes) then
           call write_output_file(unit_good,s,model_id,model_names,av,sc,chi,model_fluxes)
        else
           call write_output_file(unit_good,s,model_id,model_names,av,sc,chi)
        end if
!        call write_models_used(unit_good,used)
     else
        n_bad = n_bad + 1
        if(present_model_fluxes) then
           call write_output_file(unit_bad, s,model_id,model_names,av,sc,chi,model_fluxes)
        else
           call write_output_file(unit_bad, s,model_id,model_names,av,sc,chi)
        end if
!        call write_models_used(unit_bad,used)
     end if
  end do

  write(*,'(" Well fit sources  : ",I7)') n_good
  write(*,'(" Badly fit sources : ",I7)') n_bad
  
  call close_output_file(unit_in)
  call close_output_file(unit_good)
  call close_output_file(unit_bad)
  
contains
  
  subroutine display_usage

    implicit none

    write(*,'(" Usage: filter_output [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   input=filename     - the input file")')
    write(*,*)
    write(*,'(" then one and only one of the following:")')
    write(*,'("   chi=value          - constrain using total chisquared")')
    write(*,'("   cpd=value          - constrain using chisquared per datapoint")')
    write(*,*)
    stop

  end subroutine display_usage

end program filter_output
