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
    use parameters_fitting, only : av_min,av_max
    use parameters_output, only  : output_model_fluxes

    use SED_models, only         : model_fluxes,model_names,model_id
    use base_fitter_output, only : write_output_file,write_models_used
    use extinction, only         : av_law
    use fitting_routines, only   : chi_squared,linear_regression,optimal_scaling
    
    implicit none
    
    type(source),intent(in) :: s
    ! the data
    
    integer,intent(in) :: unit
    ! the file unit to write the fit(s) info to
    
    real(sp),allocatable       :: av_best(:)
    real(sp),allocatable       :: ch_best(:)
    logical(1),allocatable :: us_best(:)
    real(sp),allocatable       :: sc_best(:)
    ! the scale-factors and chisquared values
    
    integer :: m,n_models
    ! loop variables and number of models and distances
    
    real(sp),allocatable    :: residual(:)
    real(sp),allocatable    :: best_model_fluxes(:,:)
    ! temporary container variables
    
    integer :: j
    ! loop variable
    
    real(sp),allocatable :: sc_law(:)
    
   n_models    = size(model_fluxes,1)

   allocate(av_best(n_models))
   allocate(ch_best(n_models))
   allocate(us_best(n_models))
   allocate(sc_best(n_models))
   
   allocate(sc_law(s%n_wav)) ; sc_law = -2.
   
   if(output_model_fluxes) allocate(best_model_fluxes(n_wav,n_models))

   allocate(residual(s%n_wav))

    do m=1,n_models

      residual(:) = s%log_flux(:) - model_fluxes(m,:)
      
        call linear_regression(s%n_wav,s%valid,residual,s%weight,av_law,sc_law,av_best(m),sc_best(m))
                  
        if(av_best(m) >= av_min.and.av_best(m) <= av_max) then
          
          ch_best(m) = chi_squared(s%valid,residual,s%log_flux_error,s%weight,av_best(m)*av_law+sc_best(m)*sc_law)
          
          else
          
            if(av_best(m) < av_min) av_best(m) = av_min
            if(av_best(m) > av_max) av_best(m) = av_max
          
            residual(:) = s%log_flux(:) - model_fluxes(m,:) - av_best(m)*av_law(:)
        
            call optimal_scaling(s%n_wav,s%valid,residual,s%weight,sc_law,sc_best(m))

            ch_best(m) = chi_squared(s%valid,residual,s%log_flux_error,s%weight,sc_best(m)*sc_law)

        end if
      
      us_best(m) = .not.any(s%valid(:).ne.0.and.model_fluxes(m,:)<-1.e30)
          
    end do
        
    if(output_model_fluxes) then
      forall(m=1:n_models,j=1:s%n_wav)
        best_model_fluxes(j,m) = model_fluxes(m,j) + av_law(j)*av_best(m) + sc_law(j)*sc_best(m)
      end forall
      call write_output_file(unit,s,model_id,model_names,av_best,sc_best,ch_best,best_model_fluxes)
    else
      call write_output_file(unit,s,model_id,model_names,av_best,sc_best,ch_best)
    end if
    
    call write_models_used(unit,us_best)
    
    deallocate(residual,sc_law)
    if(output_model_fluxes) deallocate(best_model_fluxes)
        
  end subroutine fit_data
  
end module fitting_procedures
