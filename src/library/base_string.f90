module base_string

  use base_types
  use base_messages

  implicit none

  private
  public :: read_from_string
  public :: concat
  public :: clean_string
  
  interface read_from_string
     module procedure read_int_from_string
     module procedure read_real4_from_string
     module procedure read_real8_from_string
     module procedure read_char_from_string
     module procedure read_logical_from_string
  end interface

contains
  
  subroutine read_int_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    integer,intent(out) :: value
    read(string,*) value
  end subroutine read_int_from_string

  subroutine read_real4_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    real(sp),intent(out) :: value
    read(string,*) value
  end subroutine read_real4_from_string

  subroutine read_real8_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    real(dp),intent(out) :: value
    read(string,*) value
  end subroutine read_real8_from_string

  subroutine read_char_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    character(len=*),intent(out) :: value
    value = trim(adjustl(string))
  end subroutine read_char_from_string

  subroutine read_logical_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    logical,intent(out) :: value
    select case(trim(adjustl(string)))
    case('Y','y','yes','YES','Yes','true',"'YES'")
       value=.true.
    case('N','n','no','NO','No','false',"'NO'")
       value=.false.
    case default
       call error("read_logical_from_string","unknown logical: "//trim(string))
    end select
  end subroutine read_logical_from_string
    
  pure character(len=100) function concat(prefix,number,suffix)
    implicit none
    character(len=*),intent(in) :: prefix,suffix
    integer,intent(in) :: number
    character(len=10) :: c_number
    write(c_number,'(I0)') number
    concat = trim(prefix)//trim(adjustl(c_number))//trim(suffix)
  end function concat
  
  subroutine clean_string(filename)
    implicit none
    character(len=*),intent(inout) :: filename
    integer :: i
    forall(i=1:len_trim(filename),filename(i:i)==" ") filename(i:i) = "_"
  end subroutine clean_string

end module base_string
