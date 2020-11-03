program bin2hdf5

    ! Created by Manuel A. Diaz, ENSMA 2020

    use HDF5 ! This module contains all necessary modules
   
    IMPLICIT NONE
   
    !-------- initialize variables -------------
    character(len=256) :: input_file
    character(len=256) :: output_file
    character(len=10)  :: Nx_char
    character(len=10)  :: Ny_char
    character(len=10)  :: Nz_char
    character(len=3)   :: data_extension="h5"
    character(len=1)   :: dsetname_p="p"

    integer :: Nx, Ny, Nz       ! Array dimensions
    integer :: pointPosition    ! '.' location
   
    integer(HID_T) :: input_file_id=1 ! File identifier
    integer(HID_T) :: output_file_id  ! File identifier
    integer(HID_T) :: dset_id         ! Dataset identifier
    integer(HID_T) :: dspace_id       ! Dataspace identifier
    integer        :: error           ! Error flag

    integer(HSIZE_T), DIMENSION(3) :: dims   ! Dataset dimensions
    integer        ::   rank = 3      ! Dataset rank
   
    integer,   parameter  :: fp=8             ! float data presicion
    real(fp), allocatable :: buff_read(:,:,:) ! buffer for reading from input file
    integer               :: buff_lenght      ! buffer lenght

    !--------  Parse arguments from command -------------
    if ( command_argument_count() .NE. 4 ) then
        print*, "Mode of use: ./bin2hdf5_serial.run [*.bin] [Nx] [Ny] [Nz]"; stop
    else 
        call get_command_argument(1,input_file)
        call get_command_argument(2,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(3,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(4,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to read ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

    !-------- Set the name of the output files -------------
    pointPosition = scan(trim(input_file),".", BACK=.true.)
    if (pointPosition>0) output_file = input_file(1:pointPosition)//data_extension

    !-------- Allocate space for the array -------------
    allocate(buff_read(Nx,Ny,Nz))
   
    !-------- Read the input data -------------
    Inquire( iolength = buff_lenght ) buff_read
    open (input_file_id,FILE=trim(input_file),form='unformatted',access='direct',recl=buff_lenght)  
    read (input_file_id,rec=1) buff_read
    close(input_file_id)

    !-------- Write the data to HDF5 -------------
    ! Set dimensions of HDF5 dataspace 
    dims(1) = Nx
    dims(2) = Ny
    dims(3) = Nz
   
    ! Initialize FORTRAN interface of HDF5.
    CALL h5open_f(error)
   
    ! Create a new file.
    CALL h5fcreate_f (output_file, H5F_ACC_TRUNC_F, output_file_id, error)
   
    ! Create the dataspace.
    CALL h5screate_simple_f(rank, dims, dspace_id, error)
   
    ! Create the dataset with default properties.
    CALL h5dcreate_f(output_file_id, dsetname_p, H5T_NATIVE_DOUBLE, dspace_id, &
            dset_id, error)
   
    ! Write the dataset.
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, buff_read, dims, error)
   
    ! End access to the dataset and release resources used by it.
    CALL h5dclose_f(dset_id, error)
   
    ! Terminate access to the data space.
    CALL h5sclose_f(dspace_id, error)
   
    ! Close the file.
    CALL h5fclose_f(output_file_id, error)
   
    ! Close FORTRAN interface.
    CALL h5close_f(error)

    ! if everything goes well then report that:
    print*, "translation *.bin to *.h5 successful :)"
    deallocate(buff_read)
   
    end program bin2hdf5