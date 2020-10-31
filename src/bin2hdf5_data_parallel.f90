program bin2hdf5_parallel

    ! Created by Manuel A. Diaz, ENSMA 2020

    USE HDF5 ! This module contains all necessary modules 

    implicit none

    ! USE MPI ! This module should contains all necessary modules, but ...
    include "mpif.h" ! This module contains all necessary modules.

    ! IMPORTANT NOTE:
    ! Number of processes and size of data in x-direction should be multiples of 2 (1,2,4,8)

    !-------- initialize variables -------------
    character(len=256) :: input_file
    character(len=256) :: output_file
    character(len=10)  :: Nx_char
    character(len=10)  :: Ny_char
    character(len=10)  :: Nz_char
    character(len=3)   :: data_extension="h5"

    integer :: Nx, Ny, Nz       ! Array dimensions
    integer :: pointPosition    ! '.' location

    ! MPI definitions and calls.
    integer :: info = MPI_INFO_NULL
    integer :: comm = MPI_COMM_WORLD
    integer :: MPIsize, MPIrank, MPIerror
    integer :: input_file_id
    integer :: new_type, data_type = MPI_REAL8;
    integer (kind = MPI_OFFSET_KIND) :: disp=0
    integer, dimension(3) :: dims   ! dimensions full array
    integer, dimension(3) :: count  ! dimensions subarray
    integer, dimension(3) :: start  ! start coordinates

    ! hdf5 subroutines parameters
    integer(HID_T) :: output_file_id  ! File identifier 
    integer(HID_T) :: dataset_id      ! Dataset identifier 
    integer(HID_T) :: filespace       ! Dataspace identifier in file 
    integer(HID_T) :: memspace        ! Dataspace identifier in memory
    integer(HID_T) :: proplist_id     ! Property list identifier 
    integer        :: HDFerror        ! Error flags
    
    ! Data buffer and its properties
    integer,   parameter  :: fp=8          ! float data presicion
    real(fp), allocatable :: data (:,:,:)  ! Data to write
    integer               :: rank = 3      ! Dataset rank 

    ! Hyperslab parameters
    integer(HSIZE_T), dimension(3) :: HDFdims   ! dimensions full array
    integer(HSIZE_T), dimension(3) :: HDFstart  ! dimensions subarray (hyperslab)
    integer(HSIZE_T), dimension(3) :: HDFcount  ! start coordinates of subarray 
    
    !--------  Parse arguments from command --------------------
    if ( command_argument_count() .NE. 4 ) then
        print*, "Mode of use: mpirun -n # bin2hdf5_parallel.run [*.bin] [nx] [ny] [nz]"; stop
    else 
        call get_command_argument(1,input_file)
        call get_command_argument(2,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(3,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(4,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to read ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

    !-------- Set the name of the output files -----------------
    pointPosition = scan(trim(input_file),".", BACK=.true.)
    if (pointPosition>0) output_file = input_file(1:pointPosition)//data_extension

    ! --------------- Initialize MPI ---------------------------
    CALL MPI_INIT(MPIerror)
    CALL MPI_COMM_SIZE(comm, MPIsize, MPIerror)
    CALL MPI_COMM_RANK(comm, MPIrank, MPIerror) 
    
    !-----define file space and local rank memory space --------
    ! Set dimensions filespace
    dims(1) = Nx
    dims(2) = Ny
    dims(3) = Nz
    ! Set dimensions of the memory space in each rank
    count(1) = dims(1)/MPIsize  ! dims(1)          ! dims(1)
    count(2) = dims(2)          ! dims(2)/MPIsize  ! dims(2)
    count(3) = dims(3)          ! dims(3)          ! dims(3)/MPIsize
    ! Define hyperslab starting point
    start(1) = MPIrank*count(1) ! 0                ! 0
    start(2) = 0                ! MPIrank*count(2) ! 0
    start(3) = 0                ! 0                ! MPIrank*count(3)
    ! Translate for HDF parameters
    HDFdims(1)=dims(1);  HDFcount(1)=count(1);  HDFstart(1)=start(1)
    HDFdims(2)=dims(2);  HDFcount(2)=count(2);  HDFstart(2)=start(2)
    HDFdims(3)=dims(3);  HDFcount(3)=count(3);  HDFstart(3)=start(3)
    
    !----------- Allocate the rank's memory space  -------------
    allocate ( data(count(1),count(2),count(3)) )

    !----------------- Initialize data_buffer ------------------
    ! fill buffer with data from the input_file 
    call MPI_FILE_OPEN(MPI_COMM_WORLD, input_file, MPI_MODE_CREATE + MPI_MODE_RDWR, MPI_INFO_NULL, input_file_id, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_TYPE_CREATE_SUBARRAY(rank, dims, count, start, MPI_ORDER_FORTRAN, data_type, new_type, MPIerror)
    call MPI_TYPE_COMMIT(new_type, MPIerror)
    call MPI_FILE_SET_VIEW(input_file_id, disp, data_type, new_type, 'native', MPI_INFO_NULL, MPIerror)
    call MPI_FILE_READ_ALL(input_file_id, data, count(1)*count(2)*count(3), data_type, MPI_STATUS_IGNORE, MPIerror)
    call MPI_TYPE_FREE(new_type, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_FILE_CLOSE(input_file_id, MPIerror)

    !----------- Write the data_buffer to an HDF5 file ---------
    ! Initialize FORTRAN predefined datatypes
    CALL h5open_f(HDFerror)

    ! Setup file access property list with parallel I/O access.
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, proplist_id, HDFerror)
    CALL h5pset_fapl_mpio_f(proplist_id, comm, info, HDFerror)

    ! Create the file collectively. 
    CALL h5fcreate_f(output_file, H5F_ACC_TRUNC_F, output_file_id, HDFerror, access_prp = proplist_id)
    CALL h5pclose_f(proplist_id, HDFerror)

    ! Create the data space for the  dataset. 
    CALL h5screate_simple_f(rank, HDFdims, filespace, HDFerror)

    ! Create the dataset with default properties.
    CALL h5dcreate_f(output_file_id, "IntArray", H5T_NATIVE_DOUBLE, filespace, dataset_id, HDFerror)
    CALL h5sclose_f(filespace, HDFerror)

    ! Each process defines dataset in memory and writes it to the hyperslab in the file. 
    CALL h5screate_simple_f(rank, HDFcount, memspace, HDFerror) 

    ! Select hyperslab in the file.
    CALL h5dget_space_f(dataset_id, filespace, HDFerror)
    CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, HDFstart, HDFcount, HDFerror)

    ! Create property list for collective dataset write
    CALL h5pcreate_f(H5P_DATASET_XFER_F, proplist_id, HDFerror) 
    CALL h5pset_dxpl_mpio_f(proplist_id, H5FD_MPIO_COLLECTIVE_F, HDFerror)
    
    ! Write the dataset collectively. 
    CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, data, HDFdims, HDFerror, &
            file_space_id = filespace, mem_space_id = memspace, xfer_prp = proplist_id)

    ! Write the dataset independently. 
    ! CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, data, HDFdims, HDFerror, &
    !         file_space_id = filespace, mem_space_id = memspace)

    CALL h5sclose_f(filespace, HDFerror)        ! close file space
    CALL h5sclose_f(memspace, HDFerror)         ! close memory space
    CALL h5dclose_f(dataset_id, HDFerror)       ! Close dataset
    CALL h5pclose_f(proplist_id, HDFerror)      ! Close property list
    CALL h5fclose_f(output_file_id, HDFerror)   ! Close the file.
    CALL h5close_f(HDFerror)                    ! Close FORTRAN predefined datatypes.

    ! if everything goes well then report that:
    if (MPIrank.eq.0) print*, "translation *.bin to *.h5 successful :)"
    deallocate(data) ! Deallocate data buffer.

    ! ------------ Finalize MPI -----------------
    CALL MPI_FINALIZE(MPIerror)

    end program bin2hdf5_parallel