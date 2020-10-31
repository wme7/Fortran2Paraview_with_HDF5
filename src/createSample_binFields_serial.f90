program writeSampleBinData

    ! Created by Manuel A. Diaz, ENSMA 2020
    
    implicit none

    !-------- initialize variables -------------------
    character(len=30) :: output_file0 = "fields_d0_it0.bin"
    character(len=30) :: output_file1 = "xp.dat"
    character(len=30) :: output_file2 = "yp.dat"
    character(len=30) :: output_file3 = "zp.dat"
    character(len=10) :: Nx_char
    character(len=10) :: Ny_char
    character(len=10) :: Nz_char

    integer              :: Nx, Ny, Nz       ! Array dimensions
    integer              :: output_file_id=3 ! File identifier
    real(8)              :: Lx, Ly, Lz       ! mesh Length
    real(8)              :: dx, dy, dz       ! mesh cell's sizes
    real(8), allocatable :: x(:), y(:), z(:) ! Axis points arrays
    real(8), allocatable :: p(:,:,:)         ! Buffer for array
    real(8)              :: IC_P0 = 1.0      ! Amplitude
    real(8)              :: IC_x0 = 0.5      ! x-center
    real(8)              :: IC_y0 = 0.5      ! y-center
    real(8)              :: IC_z0 = 0.5      ! z-center
    real(8)              :: IC_S0 = 0.31     ! Sigma
    integer              :: buff_lenght      ! Buffer length
    integer              :: i, j, k          ! dummy indexes

    !-------- Parse arguments from command -----------
    if ( command_argument_count() .NE. 3 ) then
        print*, "Mode of use: ./sample_binFields.run [nx] [ny] [nz]"; stop
    else 
        call get_command_argument(1,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(2,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(3,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to write ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

    !-------- Allocate space for the array -----------
    allocate( x(Nx) )
    allocate( y(Ny) )
    allocate( z(Nz) )
    allocate( p(nx,ny,nz) )

    !-------- Build mesh grid axis -------------------
    Lx = 1.0; dx = Lx / (Nx-1) 
    Ly = 1.0; dy = Ly / (Ny-1) 
    Lz = 1.0; dz = Lz / (Nz-1) 
    do i = 1,Nx
        x(i) = dx*real(i-1)
    end do
    do j = 1,Ny
        y(j) = dy*real(j-1)
    end do
    do k = 1,Nz
        z(k) = dz*real(k-1)
    end do

    !------ fill in the array with sample data -------
    do k = 1,Nz
        do j = 1,Ny
            do i = 1,Nx
                p(i,j,k) = IC_P0 * exp( -((x(i)-IC_x0)**2 + (y(j)-IC_y0)**2 + (z(k)-IC_z0)**2)/(IC_S0**2) )
            end do
        end do
    end do

    !------ write in data in a binary file -----------
    Inquire( iolength = buff_lenght ) p
    open (output_file_id,file=trim(output_file0), form='unformatted', access='direct', recl=buff_lenght)
    write(output_file_id,rec=1) p
    close(output_file_id)
    open (output_file_id,file=trim(output_file1), form='formatted', action='write')
    do i = 1, Nx
        write(output_file_id,'(F10.8)') x(i)
    end do
    close(output_file_id)
    open (output_file_id,file=trim(output_file2), form='formatted', action='write')
    do i = 1, Ny
        write(output_file_id,'(F10.8)') y(i)
    end do
    close(output_file_id)
    open (output_file_id,file=trim(output_file3), form='formatted', action='write')
    do i = 1, Nz
        write(output_file_id,'(F10.8)') z(i)
    end do
    close(output_file_id)

    ! If we reach this point then
    print*, "outputs: ", output_file0, output_file1, output_file2, output_file3
    deallocate(p)
    deallocate(x)
    deallocate(y)
    deallocate(z)

    end program writeSampleBinData