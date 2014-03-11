module base_types
  
  implicit none
  save
  
  integer,parameter,public :: idp = selected_int_kind(13)
  integer,parameter,public :: sp = selected_real_kind(p=6,r=37)
  integer,parameter,public :: dp = selected_real_kind(p=15,r=307)
  integer,parameter,public :: path_length = 1000

end module base_types