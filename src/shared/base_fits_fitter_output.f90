module base_fitter_output

  use base_types
  use base_messages

  implicit none

  private

  public :: open_output_file_new
  public :: write_output_file
  public :: write_models_used
  public :: close_output_file

  public :: open_output_file_read
  public :: read_output_file
  public :: read_models_used
  public :: read_output_file_source

  public :: number_sources

  integer,parameter,private :: bitpix = -32
  logical,parameter,private :: extend = .true.
  integer,parameter,private :: naxis=1,naxes(1)=(/0/)

contains

  subroutine open_output_file_new(filename,unit,models_dir,ex_law,filters,output_model_fluxes,out_format,out_number)

    use base_cfitsio
    use base_string, only : concat
    use base_source, only : filter

    implicit none

    character(len=*),intent(in) :: filename,models_dir,ex_law
    type(filter),intent(in)     :: filters(:)
    logical,intent(in)          :: output_model_fluxes
    character(len=*),intent(in) :: out_format
    real(sp),intent(in)             :: out_number
    integer,intent(out)         :: unit

    integer :: n_wav

    ! Table 1
    character(len=20),parameter :: extname_t1 = "SOURCES_BINARY"
    integer,parameter :: n_cols_t1 = 11
    character(len=20),dimension(n_cols_t1) :: title_t1,form_t1,units_t1

    ! Table 2
    character(len=20),parameter :: extname_t2 = "FITS_BINARY"
    integer :: n_cols_t2
    character(len=10),allocatable,dimension(:) :: title_t2,form_t2,units_t2

    integer :: j

    n_wav = size(filters)

    call fits_open_new(unit,'mem://'//filename)
    call fits_write_primary_header(unit,bitpix,naxis,naxes,extend)

    if(len_trim(models_dir) > 75) call error("write_output_file","path to models is too long to include in FITS header")
    call fits_write_keyword(unit,'MODELDIR',models_dir)
    if(len_trim(ex_law) > 75) call error("write_output_file","path to extinction law is too long to include in FITS header")
    call fits_write_keyword(unit,'EXLAW',ex_law)
    call fits_write_keyword(unit,'OUT_FORM',out_format)
    call fits_write_keyword(unit,'OUT_NUMB',out_number)

    ! Source information table

    title_t1(1) = "SOURCE_NAME"      ; units_t1(1) = ""     ; form_t1(1) = "30A"
    title_t1(2) = "X"                ; units_t1(2) = "deg"  ; form_t1(2) = "1D"
    title_t1(3) = "Y"                ; units_t1(3) = "deg"  ; form_t1(3) = "1D"
    title_t1(4) = "SOURE_ID"         ; units_t1(4) = ""     ; form_t1(4) = "1J"
    title_t1(5) = "FIRST_ROW"        ; units_t1(5) = ""     ; form_t1(5) = "1J"
    title_t1(6) = "NUMBER_FITS"      ; units_t1(6) = ""     ; form_t1(6) = "1J"
    title_t1(7) = "VALID"            ; units_t1(7) = ""     ; write(form_t1(7),'(I0,"J")') n_wav    
    title_t1(8) = "FLUX"             ; units_t1(8) = "mJy"  ; write(form_t1(8),'(I0,"E")') n_wav
    title_t1(9) = "FLUX_ERROR"       ; units_t1(9) = "mJy"  ; form_t1(9)  = form_t1(8)
    title_t1(10) = "LOG10FLUX"       ; units_t1(10) = "mJy" ; form_t1(10) = form_t1(8)
    title_t1(11) = "LOG10FLUX_ERROR" ; units_t1(11) = "mJy" ; form_t1(11) = form_t1(8)

    call fits_create_hdu(unit,2)
    call fits_table_write_header(unit,0,n_cols_t1,title_t1,form_t1,units_t1,extname_t1)

    call fits_write_keyword(unit,"NWAV",n_wav,"Number of wavelengths")
    do j=1,n_wav
       call fits_write_keyword(unit,trim(concat("FILT",j,"")),filters(j)%name,"Filter code")
       call fits_write_keyword(unit,trim(concat("WAV",j,"")),filters(j)%wavelength_microns,"Wavelength (microns)")
       call fits_write_keyword(unit,trim(concat("AP",j,"")),filters(j)%aperture_arcsec,"Aperture (arcsec)")
    end do

    ! Fit information table

    if(output_model_fluxes) then
       n_cols_t2 = 8
    else
       n_cols_t2 = 7
    end if

    allocate(title_t2(n_cols_t2),form_t2(n_cols_t2),units_t2(n_cols_t2))

    title_t2(1) = "SOURCE_ID"   ; units_t2(1) = ""    ; form_t2(1) = "1J"
    title_t2(2) = "FIT_ID"      ; units_t2(2) = ""    ; form_t2(2) = "1J"
    title_t2(3) = "MODEL_ID"    ; units_t2(3) = ""    ; form_t2(3) = "1J"
    title_t2(4) = "MODEL_NAME"  ; units_t2(4) = ""    ; form_t2(4) = "30A"
    title_t2(5) = "CHI2"        ; units_t2(5) = ""    ; form_t2(5) = "1E"
    title_t2(6) = "AV"          ; units_t2(6) = "MAG" ; form_t2(6) = "1E"
    title_t2(7) = "SCALE"       ; units_t2(7) = ""    ; form_t2(7) = "1E"

    if(output_model_fluxes) then
       title_t2(8) = "MODEL_LOG10FLUX" ; units_t2(8) = "mJy" ; write(form_t2(8),'(I0,"E")') n_wav  
    end if

    call fits_create_hdu(unit,3)
    call fits_table_write_header(unit,0,n_cols_t2,title_t2,form_t2,units_t2,extname_t2)

    call fits_write_keyword(unit,"MODELFLX",output_model_fluxes)

  end subroutine open_output_file_new

  subroutine close_output_file(unit)
    use base_cfitsio
    implicit none
    integer,intent(in) :: unit
    call fits_mem_to_file(unit)
    call fits_close(unit)
  end subroutine close_output_file

  subroutine write_output_file(unit,s,model_id,model_name,av,sc,chi,model_fluxes)

    use base_cfitsio
    use base_array
    use base_messages
    use base_source

    implicit none

    integer,intent(in)            :: unit
    type(source),intent(in)       :: s
    real(sp),intent(inout)            :: chi(:),av(:),sc(:)
    real(sp),intent(in),optional      :: model_fluxes(:,:)
    integer,intent(in)            :: model_id(:)
    integer,dimension(size(chi))  :: source_id,fit_id,order
    character(len=30),intent(in)  :: model_name(:)
    integer,save                  :: current_source = 0

    integer :: next_row_t1,next_row_t2
    integer :: n_fits
    integer :: i

    character(len=1) :: out_format
    real             :: out_number

    logical :: output_model_fluxes

    current_source = current_source + 1
    source_id = current_source

    forall(i=1:size(fit_id)) fit_id(i)=i

    where(.not.(chi >= 0. .and. chi < huge(1.)))
       chi = huge(1.)
    end where

    call index_array_1d(size(chi),chi,order)

    call fits_move_hdu(unit,1)

    call fits_read_keyword(unit,'OUT_FORM',out_format)
    call fits_read_keyword(unit,'OUT_NUMB',out_number)

    n_fits = number_fits(chi(order),s%n_data,out_format,out_number)
    select case(out_format)
    case('C','D','E','F')
       n_fits = min(n_fits + 1,size(chi))
    end select

    call fits_move_hdu(unit,3)
    call fits_read_keyword(unit,"MODELFLX",output_model_fluxes)

    if(output_model_fluxes.and..not.present(model_fluxes)) then
       call error("write_output_file","model fluxes requested but not specified")
    else if(.not.output_model_fluxes.and.present(model_fluxes)) then
       call error("write_output_file","model fluxes not requested but specified")
    end if

    next_row_t2 = fits_table_number_rows(unit) + 1
    call fits_table_write_column(unit,1,n_fits,source_id,row=next_row_t2)
    call fits_table_write_column(unit,2,n_fits,fit_id,row=next_row_t2)
    call fits_table_write_column(unit,3,n_fits,model_id(order),row=next_row_t2)
    call fits_table_write_column(unit,4,n_fits,model_name(order),row=next_row_t2)
    call fits_table_write_column(unit,5,n_fits,chi(order),row=next_row_t2)
    call fits_table_write_column(unit,6,n_fits,av(order),row=next_row_t2)
    call fits_table_write_column(unit,7,n_fits,sc(order),row=next_row_t2)
    if(output_model_fluxes) then
       call fits_table_write_column(unit,8,n_fits*s%n_wav,model_fluxes(:,order),row=next_row_t2)
    end if

    call fits_move_hdu(unit,2)
    next_row_t1 = fits_table_number_rows(unit) + 1
    call fits_table_write_column(unit,1,1,(/s%name/),row=next_row_t1)
    call fits_table_write_column(unit,2,1,(/s%x/),row=next_row_t1)
    call fits_table_write_column(unit,3,1,(/s%y/),row=next_row_t1)
    call fits_table_write_column(unit,4,1,(/current_source/),row=next_row_t1)
    call fits_table_write_column(unit,5,1,(/next_row_t2/),row=next_row_t1)
    call fits_table_write_column(unit,6,1,(/n_fits/),row=next_row_t1)
    call fits_table_write_column(unit,7,s%n_wav,s%valid(:),row=next_row_t1)
    call fits_table_write_column(unit,8,s%n_wav,s%flux(:),row=next_row_t1)
    call fits_table_write_column(unit,9,s%n_wav,s%flux_error(:),row=next_row_t1)
    call fits_table_write_column(unit,10,s%n_wav,s%log_flux(:),row=next_row_t1)
    call fits_table_write_column(unit,11,s%n_wav,s%log_flux_error(:),row=next_row_t1)

  end subroutine write_output_file

  subroutine write_models_used(unit,used)

    use base_cfitsio
    use base_array
    use base_messages
    use base_source

    implicit none

    integer,intent(in)           :: unit
    logical(1),intent(in)        :: used(:)
    integer                      :: source_id
    integer                      :: n_bytes,n_models
    character(len=1),allocatable :: bytes(:)
    integer                      :: i,j
    logical(1)                   :: bits(8)

    if(fits_number_hdu(unit)==3) then
       call fits_move_hdu(unit,3)
       call fits_create_hdu(unit,4)
       call fits_write_keyword(unit,"XTENSION",'IMAGE')
       call fits_write_keyword(unit,"BITPIX",8)
       call fits_write_keyword(unit,"NAXIS",2)
       call fits_write_keyword(unit,"NAXIS1",0)
       call fits_write_keyword(unit,"NAXIS2",0)
       call fits_write_keyword(unit,"PCOUNT",0)
       call fits_write_keyword(unit,"GCOUNT",1)
       call fits_write_keyword(unit,"EXTNAME",'MODEL_USAGE')
    end if

    call fits_move_hdu(unit,4)

    if(fits_exists_keyword(unit,'NMODELS')) then
       call fits_read_keyword(unit,"NAXIS1",n_bytes)
       call fits_read_keyword(unit,"NMODELS",n_models)
       if(n_models.ne.size(used)) call error("write_models_used","varying number of models")
    else
       n_bytes  = ceiling(size(used)/8.)
       n_models = size(used)
       call fits_write_keyword(unit,"NAXIS1",n_bytes)
       call fits_write_keyword(unit,"NMODELS",n_models)
    end if

    allocate(bytes(n_bytes))

    call fits_read_keyword(unit,"NAXIS2",source_id)
    source_id = source_id + 1
    call fits_write_keyword(unit,"NAXIS2",source_id)

    bytes = ''
    do i=1,n_bytes
       bits = .false.
       do j=1,8
          if((i-1)*8+j>size(used)) exit
          bits(j) = used((i-1)*8+j)
       end do
       bytes(i) = char(sum([1,2,4,8,16,32,64,128],bits))
    end do

    call fits_write_array_line(unit,source_id,n_bytes,bytes)

  end subroutine write_models_used

  subroutine read_models_used(unit,source_requested,used)

    use base_cfitsio
    use base_array
    use base_messages
    use base_source

    implicit none

    integer,intent(in)                 :: unit,source_requested
    logical(1),intent(out),allocatable :: used(:)
    integer                            :: n_bytes,n_models,n_sources
    character(len=1),allocatable       :: bytes(:)
    integer :: i,j
    logical(1) :: bits(8)

    if(fits_number_hdu(unit)==3) then 
       call error("read_models_used","models used missing from FITS file")
    end if

    call fits_move_hdu(unit,4)

    if(fits_exists_keyword(unit,'NMODELS')) then
       call fits_read_keyword(unit,"NAXIS1",n_bytes)
       call fits_read_keyword(unit,"NMODELS",n_models)
       allocate(bytes(n_bytes))
       allocate(used(n_models)) ; used = .false.
    else
       call error("read_models_used","models used missing from FITS file")
    end if

    call fits_read_keyword(unit,"NAXIS2",n_sources)
    if(source_requested > n_sources) call error("read_models_used","source does not exist")

    call fits_read_array_line(unit,source_requested,n_bytes,bytes)

    do i=1,n_bytes
       bits = char2bin(bytes(i))
       do j=1,8
          if((i-1)*8+j>n_models) exit
          used((i-1)*8+j) = bits(j)
       end do
    end do

  contains

    function char2bin(c) result(b)
      implicit none
      character(len=1),intent(in) :: c
      logical(1)                  :: b(8)
      integer                     :: p
      integer                     :: value
      integer,parameter           :: pow(8) = [1,2,4,8,16,32,64,128]
      value = ichar(c)
      do p=8,1,-1
         b(p) = value >= pow(p)
         if(b(p)) value = value - pow(p)
      end do
    end function char2bin

  end subroutine read_models_used

  integer function number_sources(unit)
    use base_cfitsio
    implicit none
    integer,intent(in) :: unit
    call fits_move_hdu(unit,2)
    call fits_read_keyword(unit,'NAXIS2',number_sources)
  end function number_sources

  subroutine open_output_file_read(filename,unit,n_wav,models_dir,ex_law,filters,present_model_fluxes,out_format,out_number)

    use base_source
    use base_cfitsio
    use base_string

    implicit none

    character(len=*),intent(in) :: filename
    integer,intent(out)         :: unit
    integer,intent(out)         :: n_wav

    type(filter),allocatable,intent(out),optional :: filters(:) 
    character(len=*),intent(out),optional         :: models_dir,ex_law

    character(len=*),intent(out),optional         :: out_format
    real(sp),intent(out),optional                     :: out_number
    logical,intent(out),optional                  :: present_model_fluxes

    integer :: j

    call fits_open_read(unit,filename)    

    if(present(out_format)) call fits_read_keyword(unit,'OUT_FORM',out_format)
    if(present(out_number)) call fits_read_keyword(unit,'OUT_NUMB',out_number)
    if(present(models_dir)) call fits_read_keyword(unit,'MODELDIR',models_dir)
    if(present(ex_law)) call fits_read_keyword(unit,'EXLAW',ex_law)

    call fits_move_hdu(unit,2)
    call fits_read_keyword(unit,"NWAV",n_wav)

    if(present(filters)) then
       allocate(filters(n_wav))
       do j=1,n_wav
          call fits_read_keyword(unit,trim(concat("FILT",j,"")),filters(j)%name)
          call fits_read_keyword(unit,trim(concat("WAV",j,"")),filters(j)%wavelength_microns)
          call fits_read_keyword(unit,trim(concat("AP",j,"")),filters(j)%aperture_arcsec)
       end do
    end if

    call fits_move_hdu(unit,3)

    if(present(present_model_fluxes)) call fits_read_keyword(unit,"MODELFLX",present_model_fluxes)

  end subroutine open_output_file_read

  subroutine read_output_file_source(unit,source_requested,s)

    use base_source
    use base_cfitsio

    integer,intent(in) :: unit
    integer,intent(in) :: source_requested
    type(source),intent(inout) :: s
    integer :: n_sources

    call check_source_allocation(s)

    call fits_move_hdu(unit,2)

    n_sources = fits_table_number_rows(unit)
    if(source_requested > n_sources) call error("read_output_file","source ID too large")

    call fits_table_read_column(unit,1,s%name,row=source_requested)
    call fits_table_read_column(unit,2,s%x,row=source_requested)
    call fits_table_read_column(unit,3,s%y,row=source_requested)
    call fits_table_read_column(unit,7,s%n_wav,s%valid(:),row=source_requested)
    call fits_table_read_column(unit,8,s%n_wav,s%flux(:),row=source_requested)
    call fits_table_read_column(unit,9,s%n_wav,s%flux_error(:),row=source_requested)
    call fits_table_read_column(unit,10,s%n_wav,s%log_flux(:),row=source_requested)
    call fits_table_read_column(unit,11,s%n_wav,s%log_flux_error(:),row=source_requested)

    s%n_data = count(s%valid==1.or.s%valid==4)

  end subroutine read_output_file_source

  subroutine read_output_file(unit,source_requested,s,model_id,model_names,av,sc,chi,out_format,out_number,model_fluxes)

    use base_cfitsio
    use base_array
    use base_messages

    use base_source
    use base_string

    implicit none

    integer,intent(in) :: unit
    integer,intent(in)                    :: source_requested
    character(len=*),intent(in)           :: out_format
    real(sp),intent(in)                       :: out_number

    type(source),intent(inout)            :: s

    real(sp),allocatable,intent(out)          :: chi(:),av(:),sc(:)
    integer,allocatable,intent(out)       :: model_id(:)
    character(len=30),allocatable,intent(out) :: model_names(:)
    real(sp),allocatable,intent(out),optional :: model_fluxes(:,:)

    integer,allocatable :: source_id(:),fit_id(:)
    integer :: first_row
    integer :: n_fits_max,n_fits
    integer :: source_id_check

    character(len=1)           :: out_format_file
    real                       :: out_number_file

    integer :: n_sources

    logical :: present_model_fluxes

    call check_source_allocation(s)

    call fits_move_hdu(unit,1)

    call fits_read_keyword(unit,'OUT_FORM',out_format_file)
    call fits_read_keyword(unit,'OUT_NUMB',out_number_file)

    call fits_move_hdu(unit,2)

    n_sources = fits_table_number_rows(unit)
    if(source_requested > n_sources) call error("read_output_file","source ID too large")

    call fits_table_read_column(unit,1,s%name,row=source_requested)
    call fits_table_read_column(unit,2,s%x,row=source_requested)
    call fits_table_read_column(unit,3,s%y,row=source_requested)
    call fits_table_read_column(unit,4,source_id_check,row=source_requested)
    call fits_table_read_column(unit,5,first_row,row=source_requested)
    call fits_table_read_column(unit,6,n_fits_max,row=source_requested)
    call fits_table_read_column(unit,7,s%n_wav,s%valid(:),row=source_requested)
    call fits_table_read_column(unit,8,s%n_wav,s%flux(:),row=source_requested)
    call fits_table_read_column(unit,9,s%n_wav,s%flux_error(:),row=source_requested)
    call fits_table_read_column(unit,10,s%n_wav,s%log_flux(:),row=source_requested)
    call fits_table_read_column(unit,11,s%n_wav,s%log_flux_error(:),row=source_requested)

    s%n_data = count(s%valid==1.or.s%valid==4)

    call fits_move_hdu(unit,3)

    ! Read in chi^2 first temporarily to figure out how many fits to read

    allocate(chi(n_fits_max))
    call fits_table_read_column(unit,5,n_fits_max,chi(:),row=first_row)
    n_fits = number_fits(chi,s%n_data,out_format,out_number)
    deallocate(chi)

    if(trim(out_format).eq.trim(out_format_file) .or. n_fits > n_fits_max) then
       if(out_number > out_number_file) call error("read_output_file","number of fits requested exceeds number available")
    end if

    allocate(chi(n_fits),av(n_fits),sc(n_fits),source_id(n_fits),fit_id(n_fits))
    allocate(model_id(n_fits),model_names(n_fits))

    if(n_fits > 0) then

       call fits_read_keyword(unit,"MODELFLX",present_model_fluxes)

       if(.not.present_model_fluxes.and.present(model_fluxes)) then
          call error("read_output_file","model fluxes not present in file")
       end if

       call fits_table_read_column(unit,1,n_fits,source_id,row=first_row)
       call fits_table_read_column(unit,2,n_fits,fit_id,row=first_row)
       call fits_table_read_column(unit,3,n_fits,model_id,row=first_row)
       call fits_table_read_column(unit,4,n_fits,model_names,row=first_row)
       call fits_table_read_column(unit,5,n_fits,chi(:),row=first_row)
       call fits_table_read_column(unit,6,n_fits,av(:),row=first_row)
       call fits_table_read_column(unit,7,n_fits,sc(:),row=first_row)

       if(present(model_fluxes)) then
          allocate(model_fluxes(s%n_wav,n_fits))
          call fits_table_read_column(unit,8,n_fits*s%n_wav,model_fluxes(:,:),row=first_row)
       end if

    end if

  end subroutine read_output_file

  integer function number_fits(chi,n_wav,out_form,out_number)

    implicit none
    real(sp),intent(in)    :: chi(:)
    integer,intent(in) :: n_wav
    character(len=*),intent(in) :: out_form
    real(sp),intent(in)             :: out_number

    select case(trim(out_form))
    case('A','a')
       number_fits = size(chi)
    case('N','n')
       number_fits = nint(out_number)
       if(number_fits>size(chi)) call error("number_fits","too few fits available")
    case('C','c')
       number_fits = count(chi < out_number)
       if(number_fits==size(chi)) call warning("number_fits","there may be too few fits available")
    case('D','d')
       number_fits = count(chi-chi(1) < out_number)
       if(number_fits==size(chi)) call warning("number_fits","there may be too few fits available")
    case('E','e')
       number_fits = count(chi/real(n_wav) < out_number)
       if(number_fits==size(chi)) call warning("number_fits","there may be too few fits available")
    case('F','f')
       number_fits = count((chi-chi(1))/real(n_wav) < out_number)
       if(number_fits==size(chi)) call warning("number_fits","there may be too few fits available")
    case default
       call error("number_fits","unknown format specifier : "//trim(out_form))  
    end select

  end function number_fits

end module base_fitter_output
