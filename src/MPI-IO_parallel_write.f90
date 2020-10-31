program  parallel_write

    ! Created by Manuel A. Diaz, ENSMA 2020

    USE MPI ! This module contains all necessary modules.

    ! Set output file and data parameters
    character (len = *), parameter :: output_file = "vector.bin"
    integer, parameter :: sp = 4    ! data presicion
    integer, parameter :: Nx = 40   ! number of elements to be read

    ! Set MPI variables
    integer ::  MPIrank, MPIerror, MPIsize
    integer ::  i, count, file_id=0
    integer (kind = MPI_OFFSET_KIND) :: start, empty=0
    integer, dimension (MPI_STATUS_SIZE) :: status

    !  Set output_file to output datafile
    real (kind = sp), dimension (:), allocatable :: buffer

    !  Initialize MPI
    call MPI_INIT(MPIerror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, MPIrank, MPIerror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, MPIsize, MPIerror)
    print*, "Hello from MPIrank", MPIrank !  Sanity check

    ! Set dimensions of the local buffer
    count = Nx / MPIsize    ! Set dimensions of the memory space in each rank
    start = count * MPIrank ! Define hyperslab starting point

    ! Allocate the buffer's memory
    allocate ( buffer(count) )

    !  There is no guarantee that an old file will be over written, so delete any previous output file
    if (MPIrank .eq. 0) then
            call MPI_File_delete(output_file, MPI_INFO_NULL, MPIerror)
    end if

    !  Initialize trivial data
    buffer = MPIrank + 10;

    !  We could check the values created in the local buffer
    do i = 1, count
        print *, " buffer ",i,"=",buffer(i)
    end do

    !  Open the output file
    call MPI_FILE_OPEN(MPI_COMM_WORLD, output_file, MPI_MODE_CREATE + MPI_MODE_RDWR, MPI_INFO_NULL, file_id, MPIerror)

    !  Wait on everyone to catch up.
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)

    !  Define the local view of the data 
    call MPI_FILE_SET_VIEW(file_id, empty, MPI_REAL4, MPI_REAL4, 'native', MPI_INFO_NULL, MPIerror)

    !  Send the data buffer to the output file in the proper place
    call MPI_FILE_WRITE_AT(file_id, start, buffer, count, MPI_REAL4, status, MPIerror)

    !  Wait on everyone to finish and close up shop
    call MPI_BARRIER(MPI_COMM_WORLD, MPIerror)

    ! Close file 
    call MPI_FILE_CLOSE(file_id, MPIerror)

    ! Finalize MPI
    call MPI_FINALIZE(MPIerror)

    end program parallel_write