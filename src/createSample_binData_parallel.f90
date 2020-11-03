program writeSampleBinData_parallel

    ! Created by Manuel A. Diaz, ENSMA 2020

    USE HDF5 ! This module contains all necessary modules 

    implicit none

    ! USE MPI ! This module should contains all necessary modules, but ...
    include "mpif.h" ! This module contains all necessary modules.

    ! IMPORTANT NOTE:
    ! Number of processes and size of data in x-direction should be multiples of 2 (1,2,4,8)

    !-------- initialize variables -------------
    character(len=256) :: output_file = "data.bin"
    character(len=10)  :: Nx_char
    character(len=10)  :: Ny_char
    character(len=10)  :: Nz_char

    integer              :: Nx, Ny, Nz       ! Array dimensions
    integer              :: output_file_id=3 ! File identifier
    real(8), allocatable :: buff_data(:,:,:) ! Buffer for array

    ! MPI definitions and calls.
    integer :: info = MPI_INFO_NULL
    integer :: comm = MPI_COMM_WORLD
    integer :: MPIsize, MPIrank, MPIerror
    integer :: new_type, data_type = MPI_REAL8;
    integer (kind = MPI_OFFSET_KIND) :: disp=0
    integer, dimension(3) :: dims  ! dimensions full array
    integer, dimension(3) :: count ! dimensions subarray
    integer, dimension(3) :: start ! start coordinates

    !-------- Parse arguments from command --------
    if ( command_argument_count() .NE. 3 ) then
        print*, "Mode of use: ./sample_binData.run [Nx] [Ny] [Nz]"; stop
    else 
        call get_command_argument(1,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(2,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(3,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to write ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

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

    !----------- Allocate the rank's memory space  -------------
    allocate ( buff_data(count(1),count(2),count(3)) )

    !----------------- Initialize data buffer ------------------
    ! use trivial data
    buff_data = 10 + MPIrank

    !--------------- write data buffer to file------------------
    call MPI_FILE_OPEN(MPI_COMM_WORLD, output_file, MPI_MODE_CREATE + MPI_MODE_RDWR, MPI_INFO_NULL, output_file_id, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_TYPE_CREATE_SUBARRAY(3, dims, count, start, MPI_ORDER_FORTRAN, data_type, new_type, MPIerror)
    call MPI_TYPE_COMMIT(new_type, MPIerror)
    call MPI_FILE_SET_VIEW(output_file_id, disp, data_type, new_type, 'native', MPI_INFO_NULL, MPIerror)
    call MPI_FILE_WRITE_ALL(output_file_id, buff_data, count(1)*count(2)*count(3), data_type, MPI_STATUS_IGNORE, MPIerror)
    call MPI_TYPE_FREE(new_type, MPIerror)
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)
    call MPI_FILE_CLOSE(output_file_id, MPIerror)

    ! if everything goes well then report that:
    if (MPIrank.eq.0) print*, "output file: ",trim(output_file)
    deallocate(buff_data) ! Deallocate data buffer.

    ! ------------ Finalize MPI -----------------
    CALL MPI_FINALIZE(MPIerror)

    end program writeSampleBinData_parallel