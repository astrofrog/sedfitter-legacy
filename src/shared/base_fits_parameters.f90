module base_fits_parameters

  use base_types
  use base_cfitsio
  use base_messages
  use base_string
  
  implicit none
  save
  
  private
  public :: get_parameter_values
  public :: get_parameter_values_all
  public :: get_parameter_name
  public :: number_parameters
  
contains
    
  subroutine get_parameter_values(filename,parameter_name,model_id_req,model_names_req,parameter_values)
    
    implicit none
    
    character(len=*),intent(in) :: filename,model_names_req(:),parameter_name
    real(sp),intent(out),allocatable :: parameter_values(:)
    integer,intent(in) :: model_id_req(:)
    
    integer :: colnum1,colnum2,unit
    
    character(len=30) :: model_name
    real(sp) :: value
    
    integer :: n_rows,i,j
    
    allocate(parameter_values(size(model_names_req)))
    
    call fits_open_read(unit,filename)
    call fits_move_hdu(unit,2)
    
    call fits_read_keyword(unit,"NAXIS2",n_rows)
    
    colnum1 = fits_table_column_number(unit,'MODEL_NAME')
    colnum2 = fits_table_column_number(unit,parameter_name)

    do i=1,size(model_id_req)
      
      j = model_id_req(i)
      
      call fits_table_read_column(unit,colnum1,model_name,row=j)
      call fits_table_read_column(unit,colnum2,value,row=j)

       if(model_names_req(i)==model_name) then
          parameter_values(i) = value
       else
          call error ("get_parameter_values","model names do not match with those in parameter file")
       end if

    end do

    call fits_close(unit)

  end subroutine get_parameter_values
  
  integer function number_parameters(filename)
    implicit none
    character(len=*),intent(in) :: filename
    integer :: unit
    call fits_open_read(unit,filename)
    call fits_move_hdu(unit,2)
    number_parameters = fits_table_number_columns(unit)
    call fits_close(unit)
  end function number_parameters
  
  subroutine get_parameter_name(filename,colnum,parameter_name)
    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(in) :: colnum
    character(len=*),intent(out) :: parameter_name
    integer :: unit
    call fits_open_read(unit,filename)
    call fits_move_hdu(unit,2)
    call fits_read_keyword(unit,concat("TTYPE",colnum,""),parameter_name)
    call fits_close(unit)
  end subroutine get_parameter_name
  
  subroutine get_parameter_values_all(filename,parameter_name,parameter_values)
    
    implicit none
    
    character(len=*),intent(in) :: filename,parameter_name
    real(sp),allocatable,intent(out) :: parameter_values(:)
    integer :: colnum,unit
    integer :: n_rows
        
    call fits_open_read(unit,filename)
    call fits_move_hdu(unit,2)
    call fits_read_keyword(unit,"NAXIS2",n_rows)

    allocate(parameter_values(n_rows))
        
    colnum = fits_table_column_number(unit,parameter_name)
    call fits_table_read_column(unit,colnum,n_rows,parameter_values)
    
    call fits_close(unit)

  end subroutine get_parameter_values_all
  
end module base_fits_parameters
