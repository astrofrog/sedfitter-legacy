POSIX = src/library/posix_default.f90

ifeq ($(FITTER_COMPILER),ifort)
  OPT = -module src/modules -heap-arrays
endif

ifeq ($(FITTER_COMPILER),g95)
  OPT = -fmod=src/modules
endif

ifeq ($(FITTER_COMPILER),gfortran)
  OPT = -Jsrc/modules/ -Isrc/modules/
endif

ifeq ($(FITTER_COMPILER),f95)
	OPT = -Isrc/modules -mdir src/modules -f2003 -colour -ieee=full
	POSIX = src/library/posix_nag.f90
endif

FC = $(FITTER_COMPILER) $(OPT) $(FITTER_FLAGS)

# DEBUGGING

DEBUGGFORTRAN = -W -Wall -fbounds-check -pedantic-errors \
                -Wunderflow -O -fbacktrace \
                -ffpe-trap=zero,overflow,underflow -g

DEBUGG95 = 	-Wall -pedantic -fbounds-check -ftrace=full

DEBUGIFORT = 	-check all -warn all,nodec,interfaces \
		-gen_interfaces -traceback -g


#FC = gfortran $(DEBUGGFORTRAN)
#FC = g95 $(DEBUGG95)
#FC = ifort -heap-arrays  $(DEBUGIFORT)

LIB = $(FITTER_PGPLOT) $(FITTER_CFITSIO)

LIBRARY = $(POSIX) \
    src/library/base_types.f90 \
   	src/library/base_messages.f90 \
		src/library/base_string.f90 \
		src/library/base_io.f90 \
		src/library/base_cfitsio.f90 \
		src/library/base_config.f90 \
		src/library/base_parfile.f90 \
		src/library/base_array.f90 \
		src/library/base_constants.f90 \
		src/library/base_pgplot.f90 \
		src/library/base_image.f90 \
		src/library/base_statistics.f90


SHARED = 	src/shared/base_fits_convolved_fluxes.f90 \
		src/shared/base_fits_sed.f90 \
		src/shared/base_source.f90 \
		src/shared/base_fits_fitter_output.f90 \
		src/shared/base_fits_parameters.f90 \
		src/shared/extinction.f90 \
		src/shared/linear_regression.f90 \
		src/shared/performance.f90

PARAMETERS = 	src/parameters/parameters_models.f90 \
		src/parameters/parameters_data.f90 \
		src/parameters/parameters_fitting.f90 \
		src/parameters/parameters_output.f90 \
		src/parameters/parameters_plotting.f90

COMMON =	$(LIBRARY) $(SHARED) $(PARAMETERS)

OBJECTS1 = 	src/main/models.f90 src/main/fitting.f90 src/main/main.f90
OBJECTS2 = 	src/main/models_noapdep.f90 src/main/fitting_noapdep.f90 src/main/main.f90
OBJECTS3 =	src/main/plotting_routines_shared.f90 \
                src/main/plotting_routines.f90 src/main/plot.f90
OBJECTS4 = 	src/main/plotting_routines_shared.f90 src/main/plot_params_1d.f90
OBJECTS5 = 	src/main/plotting_routines_shared.f90 src/main/plot_params_2d.f90
OBJECTS6 = 	src/main/filter_output.f90
OBJECTS7 = 	src/main/sn_filter.f90
OBJECTS8 = 	src/main/reset_errors.f90
OBJECTS9 = 	src/main/catalog2data.f90
OBJECTS10 = 	src/main/fits2data.f90
OBJECTS11 = 	src/custom/generic_ex1.f90
OBJECTS12 = 	src/custom/generic_ex2.f90
OBJECTS13 = 	src/custom/generic_web.f90
OBJECTS14 = 	src/custom/generic_web_s.f90

OBJECTS15 =	src/util/filters.f90 src/util/convolve.f90
OBJECTS16 =	src/util/make_monochromatic.f90
OBJECTS17 =	src/util/list2data.f90

ALL = 		$(COMMON) \
		$(OBJECTS1) $(OBJECTS2) $(OBJECTS3) \
		$(OBJECTS4) $(OBJECTS5) $(OBJECTS6) \
		$(OBJECTS7) $(OBJECTS8) $(OBJECTS9) \
		$(OBJECTS10) $(OBJECTS11) $(OBJECTS12) \
		$(OBJECTS13) $(OBJECTS14)

UTIL = $(COMMON) $(OBJECTS15) $(OBJECTS16) $(OBJECTS17)

all:	$(ALL)
	$(FC) $(COMMON) $(OBJECTS1) $(LIB) -o bin/fit
	$(FC) $(COMMON) $(OBJECTS2) $(LIB) -o bin/fit_stellar
	$(FC) $(COMMON) $(OBJECTS3) $(LIB) -o bin/plot
	$(FC) $(COMMON) $(OBJECTS4) $(LIB) -o bin/plot_params_1d
	$(FC) $(COMMON) $(OBJECTS5) $(LIB) -o bin/plot_params_2d
	$(FC) $(COMMON) $(OBJECTS6) $(LIB) -o bin/filter_output
	$(FC) $(COMMON) $(OBJECTS7) $(LIB) -o bin/sn_filter
	$(FC) $(COMMON) $(OBJECTS8) $(LIB) -o bin/reset_errors
	$(FC) $(COMMON) $(OBJECTS9) $(LIB) -o bin/catalog2data
	$(FC) $(COMMON) $(OBJECTS10) $(LIB) -o bin/fits2data
	$(FC) $(COMMON) $(OBJECTS11) $(LIB) -o bin/generic_ex1
	$(FC) $(COMMON) $(OBJECTS12) $(LIB) -o bin/generic_ex2
	$(FC) $(COMMON) $(OBJECTS13) $(LIB) -o bin/generic_web
	$(FC) $(COMMON) $(OBJECTS14) $(LIB) -o bin/generic_web_s

util:	$(CONVOLVE)
	$(FC) $(COMMON) $(OBJECTS15) $(LIB) -o bin/convolve
	$(FC) $(COMMON) $(OBJECTS16) $(LIB) -o bin/make_monochromatic
	$(FC) $(COMMON) $(OBJECTS17) $(LIB) -o bin/list2data


clean:;		rm src/modules/*.mod bin/*
