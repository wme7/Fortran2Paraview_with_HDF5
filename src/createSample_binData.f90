program writeSampleBinData

    ! Created by Manuel A. Diaz, ENSMA 2020

    implicit none

    !-------- initialize variables -------------
    character(len=256) :: output_file = "data.bin"
    character(len=10)  :: Nx_char
    character(len=10)  :: Ny_char
    character(len=10)  :: Nz_char

    integer              :: Nx, Ny, Nz       ! Array dimensions
    integer              :: output_file_id=3 ! File identifier
    real(8), allocatable :: buff_data(:,:,:) ! Buffer for array
    integer              :: buff_lenght      ! Buffer length
    integer              :: i,j,k            ! dummy indexes

    !-------- Parse arguments from command --------
    if ( command_argument_count() .NE. 3 ) then
        print*, "Mode of use: ./sample_binData.run [nx] [ny] [nz]"; stop
    else 
        call get_command_argument(1,Nx_char); read(Nx_char,*) Nx
        call get_command_argument(2,Ny_char); read(Ny_char,*) Ny
        call get_command_argument(3,Nz_char); read(Nz_char,*) Nz
        print*, "Attemping to write ",Nx,"Nx",Ny,"Ny",Nz,"Nz array."
    end if

    !-------- Allocate space for the array -------------
    allocate(buff_data(nx,ny,nz))

    !------ fill in the array with sample data ----
    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                buff_data(i,j,k) = 1.0
            end do
        end do
    end do

    !------ write in data in a binary file -----------
    Inquire( iolength = buff_lenght ) buff_data
    open (output_file_id,file=trim(output_file), form='unformatted', access='direct', recl=buff_lenght)
    write(output_file_id,rec=1) buff_data
    close(output_file_id)
    print*, "output file: ",trim(output_file)
    deallocate(buff_data)

    end program writeSampleBinData