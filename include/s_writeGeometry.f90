subroutine writeGeometry_h5_serial(x,y,z,Nx,Ny,Nz)

    ! writen by Manuel A. Diaz, ENSMA 2020

    use HDF5 ! This module contains all necessary modules

    implicit none

    ! Main parameters
    integer, intent(in) ::   Nx,   Ny,   Nz  ! Axis dimensions
    real(8), intent(in) :: x(Nx),y(Ny),z(Nz) ! Axis points arrays

    ! HDF5 parameters
    integer(HSIZE_T) :: dims(1)            ! Dataset dimensions
    integer(HID_T)   :: output_file_id     ! File identifier
    integer(HID_T)   :: dset_id            ! Dataset identifier
    integer(HID_T)   :: dspace_id          ! Dataspace identifier
    integer          :: error              ! Error flag

    ! Write geometry file
    !
    CALL h5open_f(error) ! Initialize FORTRAN interface of HDF5. 
    CALL h5fcreate_f ("geometry_d0.h5", H5F_ACC_TRUNC_F, output_file_id, error) ! Create a new file.
    ! x
    dims = Nx
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "x_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, x, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! y
    dims = Ny
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "y_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, y, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! z
    dims = Nz
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "z_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, z, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    
    CALL h5fclose_f(output_file_id, error) ! Close the file.
    CALL h5close_f(error) ! Close FORTRAN interface.
    
end subroutine writeGeometry_h5_serial

subroutine writeGeometry_h5_parallel(x,y,z,Nx,Ny,Nz)

    ! writen by Manuel A. Diaz, ENSMA 2020

    use HDF5 ! This module contains all necessary modules
    use MPI ! This module contains all necessary modules

    implicit none

    ! Main parameters
    integer, intent(in) ::   Nx,   Ny,   Nz  ! Axis dimensions
    real(8), intent(in) :: x(Nx),y(Ny),z(Nz) ! Axis points arrays

    ! HDF5 parameters
    integer(HSIZE_T) :: dims(1)            ! Dataset dimensions
    integer(HID_T)   :: output_file_id     ! File identifier
    integer(HID_T)   :: proplist_id        ! Property list identifier
    integer(HID_T)   :: dset_id            ! Dataset identifier
    integer(HID_T)   :: dspace_id          ! Dataspace identifier
    integer          :: error              ! Error flag

    ! Write geometry file
    !
    CALL h5open_f(error) ! Initialize FORTRAN interface of HDF5. 
    ! CALL h5fcreate_f ("geometry_d0.h5", H5F_ACC_TRUNC_F, output_file_id, error) ! Create a new file.
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, proplist_id, error)
    CALL h5pset_fapl_mpio_f(proplist_id, MPI_COMM_WORLD, MPI_INFO_NULL, error)
    CALL h5fcreate_f("geometry_d0.h5", H5F_ACC_TRUNC_F, output_file_id, error, access_prp = proplist_id)
    CALL h5pclose_f(proplist_id, error)
    ! x
    dims = Nx
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "x_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, x, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! y
    dims = Ny
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "y_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, y, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    ! z
    dims = Nz
    CALL h5screate_simple_f(1, dims, dspace_id, error) ! Create the dataspace.
    CALL h5dcreate_f(output_file_id, "z_nodes", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, z, dims, error) ! Write the dataset.
    CALL h5dclose_f(dset_id, error) ! End access to the dataset and release resources used by it.
    CALL h5sclose_f(dspace_id, error) ! Terminate access to the data space.
    
    CALL h5fclose_f(output_file_id, error) ! Close the file.
    CALL h5close_f(error) ! Close FORTRAN interface.
    
end subroutine writeGeometry_h5_parallel