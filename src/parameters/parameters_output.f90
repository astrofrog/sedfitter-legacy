module parameters_output

  use base_types  
  use base_messages
  use base_parfile
  use base_io
  
  implicit none
  save
  
  private
  public :: read_output_parameters
  public :: check_output_parameters_set
  
  logical,private :: set = .false.
  ! whether the parameters have been set
  
  character(len=1000),public :: out_file
  ! output filename
  
  character(len=1),public :: out_format
  real(sp),public             :: out_number
  logical,public          :: output_model_fluxes = .false.
  
contains
  
  subroutine read_output_parameters(filename)
    implicit none
    character(len=*),intent(in) :: filename
    call message_section("Output parameters")
    call load_par_file(filename)
    if(present_arg('output')) then
      out_file = char_arg('output')
    else
      call read_par('ofile',out_file)
    end if
    call delete_file(out_file)
    call read_par('oform',out_format)
    call read_par('onumb',out_number)
    if(present_arg('best')) then
      if(logical_arg('best')) then
        out_format = 'N'
        out_number = 1
      end if
    end if
    call read_par('oconv',output_model_fluxes)
    write(*,'(1X,A19)',advance='no') "File   : "
    write(*,*) trim(out_file)
    write(*,'(1X,A19,A1)') "Format : ",out_format
    if(out_format=='N') then
      write(*,'(1X,A19,I0)') "Number : ",nint(out_number)
    else
      write(*,'(1X,A19,F9.5)') "Number : ",out_number
    end if
    set = .true.
  end subroutine read_output_parameters
  
  subroutine check_output_parameters_set(origin)
    implicit none
    character(len=*),intent(in) :: origin
    if(.not.set) call error(origin,"output parameters not set")
  end subroutine check_output_parameters_set
  
end module parameters_output
