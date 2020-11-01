# Use gfortran unless already defined
F90 ?= mpif90

ifeq ($(F90), mpif90)
	FFLAGS	?= -O2 -g -std=f2008 -Wall -Wextra -fall-intrinsics
else ifeq ($(F90), ifort)
	FFLAGS	:= -O2 -stand f08 -warn all
endif

# Path th HDF5 library (serial)
#HDF_INSTALL = /usr/local/Cellar/hdf5/1.12.0_1
# Path th HDF5 library (parallel)
HDF_INSTALL = /usr/local/phdf5-1.10.7

# Make Directory command
MKDIR_P := mkdir -p

# Program subfolders
APP_DIR = src
EXE_DIR = build
INC_DIR = include

# Library Path 
LIBHDF5 = -I$(HDF_INSTALL)/include -L$(HDF_INSTALL)/lib -lhdf5_fortran

# Compilations options
OPTIONS = -cpp -D_USE_DOUBLE_PRECISION

# List of programs to be compiled:
COMPILE := 	createSample_binData_serial \
			createSample_binFields_serial \
			bin2hdf5_data_serial \
			bin2hdf5_fields_serial \
			createSample_binData_parallel \
			createSample_binFields_parallel \
			bin2hdf5_data_parallel \
			bin2hdf5_fields_parallel \
			MPI-IO_parallel_write \
			MPI-IO_parallel_read

.PHONY:	all test clean

all: $(EXE_DIR)	$(COMPILE)

# Create Build directory
$(EXE_DIR):
	${MKDIR_P} $(EXE_DIR) 

# How to get .o object files from .f90 source files
$(INC_DIR)/%.o: $(INC_DIR)/%.f90 
	$(F90) -c -o $@ $< $(LIBHDF5) $(FFLAGS) $(OPTIONS)

$(APP_DIR)/%.o: $(APP_DIR)/%.f90 
	$(F90) -c -o $@ $< $(LIBHDF5) $(FFLAGS) $(OPTIONS)

# Programs dependency information (the order is important!)
$(COMPILE): $(INC_DIR)/s_writeGeometry.o \
			$(INC_DIR)/s_writeXDMF.o  

# How to get executables from .o object files
%: $(APP_DIR)/%.o
	$(F90) -o $(EXE_DIR)/$@.run $^ $(LIBHDF5) $(FFLAGS) $(OPTIONS) 

clean:
	$(RM) $(EXE_DIR)/*.run $(MOD_DIR)/*.o $(APP_DIR)/*.o $(EXE_DIR)/*.mod \
	$(INC_DIR)/*.o $(EXE_DIR)/*.bin $(EXE_DIR)/*.dat $(EXE_DIR)/*.h5 $(EXE_DIR)/*.xmf