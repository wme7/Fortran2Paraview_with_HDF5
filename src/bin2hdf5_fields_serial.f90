program bin2hdf5

    ! Created by Manuel A. Diaz, ENSMA 2020

    use HDF5 ! This module contains all necessary modules
   
    IMPLICIT NONE
   
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
    integer               :: buff_lenght      ! Buffer length
    integer               :: i, j, k          ! dummy indexes
   
    integer(HSIZE_T), dimension(3) :: dims    ! Dataset dimensions
    integer(HID_T)        :: input_file_id=1  ! File identifier
    integer(HID_T)        :: output_file_id   ! File identifier
    integer(HID_T)        :: dset_id          ! Dataset identifier
    integer(HID_T)        :: dspace_id        ! Dataspace identifier
    integer               :: error            ! Error flag

    !--------  Parse arguments from command -------------
    if ( command_argument_count() .NE. 4 ) then
        print*, "Mode of use: ./bin2hdf5_fields_serial.run [*.bin] [nx] [ny] [nz]"; stop
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

    !-------- Allocate space for the array -------------
    allocate( x(Nx) )
    allocate( y(Ny) )
    allocate( z(Nz) )
    allocate( p(Nx,Ny,Nz) )
   
    !-------- Read the input data -------------
    Inquire( iolength = buff_lenght ) p
    open (input_file_id,FILE=trim(input_file0), form='unformatted', status='old', access='direct',recl=buff_lenght)  
    read (input_file_id,rec=1) p
    close(input_file_id)
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

    !-------- Write the data to HDF5 -------------
    ! Write Fields
    dims(1) = Nx
    dims(2) = Ny
    dims(3) = Nz
    !
    CALL h5open_f(error) ! Initialize FORTRAN interface of HDF5. 
    CALL h5fcreate_f (output_file, H5F_ACC_TRUNC_F, output_file_id, error) ! Create a new file.
    ! p
    CALL h5screate_simple_f(3, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "p", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, p, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space. 
    
    CALL h5fclose_f(output_file_id, error) ! Close the file.
    CALL h5close_f(error) ! Close FORTRAN interface.

    ! Write geometry file
    CALL writeGeometry_h5_dp(x,y,z,Nx,Ny,Nz)
    ! CALL h5open_f(error) ! Initialize FORTRAN interface of HDF5. 
    ! CALL h5fcreate_f ("geometry_d0.h5", H5F_ACC_TRUNC_F, output_file_id, error) ! Create a new file.
    ! ! x
    ! dim = Nx
    ! CALL h5screate_simple_f(1, dim, dspace_id, error) ! Create the dataspace.
    ! CALL h5dcreate_f(output_file_id, "x_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    ! CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, x, dim, error) ! Write the dataset.
    ! CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    ! CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! ! y
    ! dim = Ny
    ! CALL h5screate_simple_f(1, dim, dspace_id, error) ! Create the dataspace.
    ! CALL h5dcreate_f(output_file_id, "y_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    ! CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, y, dim, error) ! Write the dataset.
    ! CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    ! CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! ! z
    ! dim = Nz
    ! CALL h5screate_simple_f(1, dim, dspace_id, error) ! Create the dataspace.
    ! CALL h5dcreate_f(output_file_id, "z_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    ! CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, z, dim, error) ! Write the dataset.
    ! CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    ! CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    
    ! CALL h5fclose_f(output_file_id, error) ! Close the file.
    ! CALL h5close_f(error) ! Close FORTRAN interface.

    ! if everything goes well then report that:
    print*, "translation *.bin to *.h5 successful :)"
    deallocate(x)
    deallocate(y)
    deallocate(z)
    deallocate(p)

    !-------- Write the associated XDMF file -------------
    call writeFields3D_xmf_dp(xdmf_file,Nx,Ny,Nz,0.0)
   
    end program bin2hdf5