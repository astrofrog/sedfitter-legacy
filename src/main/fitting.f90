 module fitting_procedures
  
  use base_types
  
  implicit none
  save
  
  private
  public :: fit_data
  
contains
  
  subroutine fit_data(s,unit)

    use base_source, only        : source
    
    use parameters_data, only    : n_wav
    use parameters_fitting, only : av_min,av_max,keep_extended
    use parameters_output, only  : output_model_fluxes

    use SED_models, only         : model_fluxes,extended,log_distances_kpc,model_names,model_id
    use base_fitter_output, only : write_output_file,write_models_used
    use extinction, only         : av_law
    use fitting_routines, only   : chi_squared,optimal_scaling
    
    implicit none
    
    type(source),intent(in) :: s
    ! the data
    
    integer,intent(in) :: unit
    ! the file unit to write the fit(s) info to
    
    real(sp),allocatable       :: av(:,:),av_best(:)
    real(sp),allocatable       :: ch(:,:),ch_best(:)
    logical(1),allocatable :: us(:,:),us_best(:)
    real(sp),allocatable       :: sc_best(:)
    integer,allocatable    :: ibest(:)
    ! the scale-factors and chisquared values
    
    integer :: m,n_models
    integer :: d,n_distances
    ! loop variables and number of models and distances
    
    real(sp),allocatable    :: residual(:)
!    logical,allocatable :: extended(:)
    real(sp),allocatable    :: best_model_fluxes(:,:)
    ! temporary container variables
    
    integer :: j
    ! loop variable
            
   n_models    = size(model_fluxes,1)
   n_distances = size(model_fluxes,3)

   allocate(av_best(n_models))
   allocate(ch_best(n_models))
   allocate(us_best(n_models))
   allocate(sc_best(n_models))

   allocate(av(n_models,n_distances))
   allocate(ch(n_models,n_distances))
   allocate(us(n_models,n_distances))

   allocate(ibest(n_models))

   allocate(residual(s%n_wav))
   
   if(output_model_fluxes) allocate(best_model_fluxes(n_wav,n_models))
   
    do m=1,n_models
      do d=1,n_distances
        residual(:) = s%log_flux(:) - model_fluxes(m,:,d)
        if(any(extended(m,:,d).and.s%valid(:).ne.0).and..not.keep_extended) then
          ch(m,d) = huge(1.)
          av(m,d) = 0.
          us(m,d) = .false.
        else
          call optimal_scaling(s%n_wav,s%valid,residual,s%weight,av_law,av(m,d))
          av(m,d) = max(min(av(m,d),av_max),av_min)
          ch(m,d) = chi_squared(s%valid,residual,s%log_flux_error,s%weight,av_law*av(m,d))
          us(m,d) = .not.any(s%valid(:).ne.0.and.model_fluxes(m,:,d)<-1.e30)
        end if
      end do
    end do

    ibest = minloc(ch,2)

    forall(m=1:n_models)
      sc_best(m) = log_distances_kpc(ibest(m))
      av_best(m) = av(m,ibest(m))
      ch_best(m) = ch(m,ibest(m))
      us_best(m) = us(m,ibest(m))
    end forall
            
    if(output_model_fluxes) then
      do m=1,n_models
        do j=1,s%n_wav
          best_model_fluxes(j,m) = model_fluxes(m,j,ibest(m)) + av_law(j)*av_best(m)
        end do
      end do
      call write_output_file(unit,s,model_id,model_names,av_best,sc_best,ch_best,best_model_fluxes)
    else
      call write_output_file(unit,s,model_id,model_names,av_best,sc_best,ch_best)
    end if
    
    call write_models_used(unit,us_best)
    
    deallocate(residual)
     if(output_model_fluxes) deallocate(best_model_fluxes)
    
  end subroutine fit_data
  
end module fitting_procedures
