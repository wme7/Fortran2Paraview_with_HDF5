program bin2hdf5_parallel

    ! Created by Manuel A. Diaz, ENSMA 2020

    USE HDF5 ! This module contains all necessary modules 

    implicit none

    ! USE MPI ! This module should contains all necessary modules, but ...
    include "mpif.h" ! This module contains all necessary modules.

    ! IMPORTANT NOTE:
    ! Number of processes and size of data in x-direction should be multiples of 2 (1,2,4,8)

    !-------- initialize variables -------------
    character(len=30) :: input_file0
    character(len=30) :: input_file1='xp.dat'
    character(len=30) :: input_file2='yp.dat'
    character(len=30) :: input_file3='zp.dat'
    character(len=30) :: output_file
    character(len=30) :: xdmf_file
    character(len=10) :: Nx_char
    character(len=10) :: Ny_char
    character(len=10) :: Nz_char
    character(len=3)  :: data_extension="h5"
    character(len=3)  :: xdmf_extension="xmf"

    integer,  parameter   :: fp=8             ! float data presicion
    integer               :: pointPosition    ! '.' location
    integer               :: Nx, Ny, Nz       ! Array dimensions
    real(fp), allocatable :: x(:), y(:), z(:) ! Axis points arrays
    real(fp), allocatable :: p(:,:,:)         ! Buffer for array
    integer               :: i, j, k          ! dummy indexes

    ! MPI definitions and calls.
    integer :: info = MPI_INFO_NULL
    integer :: comm = MPI_COMM_WORLD
    integer :: MPIsize, MPIrank, MPIerror
    integer :: input_file_id=1
    integer :: new_type, data_type = MPI_REAL8;
    integer (kind = MPI_OFFSET_KIND) :: disp=0
    integer, dimension(3) :: dims   ! dimensions full array
    integer, dimension(3) :: count  ! dimensions subarray
    integer, dimension(3) :: start  ! start coordinates

    ! hdf5 subroutines parameters
    integer(HID_T) :: output_file_id  ! File identifier 
    integer(HID_T) :: dataset_id      ! Dataset identifier 
    integer(HID_T) :: filespace_id    ! Dataspace identifier in file 
    integer(HID_T) :: memspace_id     ! Dataspace identifier in memory
    integer(HID_T) :: proplist_id     ! Property list identifier 
    integer        :: HDFerror        ! Error flags

    ! Hyperslab parameters
    integer(HSIZE_T), dimension(3) :: HDFdims   ! dimensions full array
    integer(HSIZE_T), dimension(3) :: HDFstart  ! dimensions subarray (hyperslab)
    integer(HSIZE_T), dimension(3) :: HDFcount  ! start coordinates of subarray 
    
    !--------  Parse arguments from command --------------------
    if ( command_argument_count() .NE. 4 ) then
        print*, "Mode of use: mpirun -n # bin2hdf5_parallel.run [*.bin] [nx] [ny] [nz]"; stop
    else 
        call get_command_argument(1,input_file0)
        call get_command_argument(2,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(3,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(4,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to read ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

    !-------- Set the name of the output files ----------
    pointPosition = scan(trim(input_file0),".", BACK=.true.)
    if (pointPosition>0) then
        output_file = input_file0(1:pointPosition)//data_extension
          xdmf_file = input_file0(1:pointPosition)//xdmf_extension
    end if

    ! --------------- Initialize MPI ---------------------------
    CALL MPI_INIT(MPIerror)
    CALL MPI_COMM_SIZE(comm, MPIsize, MPIerror)
    CALL MPI_COMM_RANK(comm, MPIrank, MPIerror) 
    
    !-----define file space and local rank memory space --------
    ! Set dimensions filespace_id
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
    if (MPIrank.eq.0) then
        allocate( x(Nx) )
        allocate( y(Ny) )
        allocate( z(Nz) )
    end if
    allocate ( p(count(1),count(2),count(3)) )

    !----------------- Initialize data_buffer ------------------
    ! fill buffer with data from the input_file 
    call MPI_FILE_OPEN(MPI_COMM_WORLD, input_file0, MPI_MODE_CREATE + MPI_MODE_RDWR, MPI_INFO_NULL, input_file_id, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_TYPE_CREATE_SUBARRAY(3, dims, count, start, MPI_ORDER_FORTRAN, data_type, new_type, MPIerror)
    call MPI_TYPE_COMMIT(new_type, MPIerror)
    call MPI_FILE_SET_VIEW(input_file_id, disp, data_type, new_type, 'native', MPI_INFO_NULL, MPIerror)
    call MPI_FILE_READ_ALL(input_file_id, p, count(1)*count(2)*count(3), data_type, MPI_STATUS_IGNORE, MPIerror)
    call MPI_TYPE_FREE(new_type, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_FILE_CLOSE(input_file_id, MPIerror)
    if (MPIrank.eq.0) then
        open (input_file_id,file=trim(input_file1), form='formatted', status='old', action='read')
        do i = 1, Nx
            read(input_file_id,*) x(i)
        end do
        close(input_file_id)
        open (input_file_id,file=trim(input_file2), form='formatted', status='old', action='read')
        do j = 1, Ny
            read(input_file_id,*) y(j)
        end do
        close(input_file_id)
        open (input_file_id,file=trim(input_file3), form='formatted', status='old', action='read')
        do k = 1, Nz
            read(input_file_id,*) z(k)
        end do
        close(input_file_id)
    end if

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
    CALL h5screate_simple_f(3, HDFdims, filespace_id, HDFerror)

    ! Create the dataset with default properties.
    CALL h5dcreate_f(output_file_id, "p", H5T_NATIVE_DOUBLE, filespace_id, dataset_id, HDFerror)
    CALL h5sclose_f(filespace_id, HDFerror)

    ! Each process defines dataset in memory and writes it to the hyperslab in the file. 
    CALL h5screate_simple_f(3, HDFcount, memspace_id, HDFerror) 

    ! Select hyperslab in the file.
    CALL h5dget_space_f(dataset_id, filespace_id, HDFerror)
    CALL h5sselect_hyperslab_f (filespace_id, H5S_SELECT_SET_F, HDFstart, HDFcount, HDFerror)

    ! Create property list for collective dataset write
    CALL h5pcreate_f(H5P_DATASET_XFER_F, proplist_id, HDFerror) 
    CALL h5pset_dxpl_mpio_f(proplist_id, H5FD_MPIO_COLLECTIVE_F, HDFerror)
    
    ! Write the dataset collectively. 
    CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, p, HDFdims, HDFerror, &
            file_space_id = filespace_id, mem_space_id = memspace_id, xfer_prp = proplist_id)

    ! Write the dataset independently. 
    ! CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, data, HDFdims, HDFerror, &
    !         file_space_id = filespace_id, mem_space_id = memspace_id)

    CALL h5sclose_f(filespace_id, HDFerror)     ! close file space
    CALL h5sclose_f(memspace_id, HDFerror)      ! close memory space
    CALL h5dclose_f(dataset_id, HDFerror)       ! Close dataset
    CALL h5pclose_f(proplist_id, HDFerror)      ! Close property list
    CALL h5fclose_f(output_file_id, HDFerror)   ! Close the file.
    CALL h5close_f(HDFerror)                    ! Close FORTRAN predefined datatypes.

    ! Write geometry file
    if (MPIrank.eq.0) CALL writeGeometry_h5_dp(x,y,z,Nx,Ny,Nz)

    ! if everything goes well then report that:
    if (MPIrank.eq.0) then
        print*, "translation *.bin to *.h5 successful :)"
        deallocate(x)
        deallocate(y)
        deallocate(z)
    end if
    deallocate(p) ! Deallocate data buffer.

    !-------- Write the associated XDMF file -------------
    if (MPIrank.eq.0) call writeFields3D_xmf_dp(xdmf_file,Nx,Ny,Nz,0.0)

    ! ------------ Finalize MPI -----------------
    CALL MPI_FINALIZE(MPIerror)

    end program bin2hdf5_parallel