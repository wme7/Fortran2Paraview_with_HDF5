subroutine writeFields3D_xmf_dp(filename,Nx,Ny,Nz,time)

    ! writen by Manuel A. Diaz, ENSMA 2020

    implicit none

    integer, parameter :: fp = 8
    integer, parameter :: xdmf_file_id = 11
    real(8), intent(in) :: time
    integer, intent(in) :: Nx, Ny, Nz
    character(len=10) :: Nx_char, Ny_char, Nz_char, fp_char, time_char
    character(LEN=20) :: filename

    write(fp_char,'(I1)') fp ! transform integer to char(1)
    write(Nx_char,'(I6.2)') Nx; Nx_char=adjustl(Nx_char) ! transform integer to char(8)
    write(Ny_char,'(I6.2)') Ny; Ny_char=adjustl(Ny_char) ! transform integer to char(8)
    write(Nz_char,'(I6.2)') Nz; Nz_char=adjustl(Nz_char) ! transform integer to char(8)
    write(time_char,'(F8.6)') time ! transform real(8) to char(8)
    ! write the file
    open (xdmf_file_id,file=trim(filename),status='unknown')
    write(xdmf_file_id,'(g0)') '<?xml version="1.0" ?>'
    write(xdmf_file_id,'(g0)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
    write(xdmf_file_id,'(g0)') '<Xdmf Version="2.0">'
    write(xdmf_file_id,'(g0)') ' <Domain>'
    write(xdmf_file_id,'(g0)') '  <Grid Name="mesh" GridType="Uniform">'
    write(xdmf_file_id,'(g0)') '   <Topology TopologyType="3DRectMesh" NumberOfElements="' & 
                            //trim(Nz_char)//' '//trim(Ny_char)//' '//trim(Nx_char)//'"/>'
    write(xdmf_file_id,'(g0)') '   <Geometry GeometryType="VXVYVZ">'
    write(xdmf_file_id,'(g0)') '    <DataItem Name="coordx" Dimensions="'//trim(Nx_char)// &
                            '" NumberType="Float" Precision="'//trim(fp_char)//'" Format="HDF">'
    write(xdmf_file_id,'(g0)') '     geometry_d0.h5:/x_nodes'
    write(xdmf_file_id,'(g0)') '    </DataItem>'
    write(xdmf_file_id,'(g0)') '    <DataItem Name="coordy" Dimensions="'//trim(Ny_char)// &
                            '" NumberType="Float" Precision="'//trim(fp_char)//'" Format="HDF">'
    write(xdmf_file_id,'(g0)') '     geometry_d0.h5:/y_nodes'
    write(xdmf_file_id,'(g0)') '    </DataItem>'
    write(xdmf_file_id,'(g0)') '    <DataItem Name="coordz" Dimensions="'//trim(Nz_char)// &
                            '" NumberType="Float" Precision="'//trim(fp_char)//'" Format="HDF">'
    write(xdmf_file_id,'(g0)') '     geometry_d0.h5:/z_nodes'
    write(xdmf_file_id,'(g0)') '    </DataItem>'
    write(xdmf_file_id,'(g0)') '   </Geometry>'
    write(xdmf_file_id,'(g0)') '   <Time TimeType="Single" Value="'//trim(time_char)//'"/>'
    write(xdmf_file_id,'(g0)') '   <Attribute Name="p" AttributeType="Scalar" Center="Node">'
    write(xdmf_file_id,'(g0)') '    <DataItem Dimensions="'//trim(Nz_char)//' '//trim(Ny_char)// &
        ' '//trim(Nx_char)//'" NumberType="Float" Precision="'//trim(fp_char)//'" Format="HDF">'
    write(xdmf_file_id,'(g0)') '     fields_d0_it0.h5:/p'
    write(xdmf_file_id,'(g0)') '    </DataItem>'
    write(xdmf_file_id,'(g0)') '   </Attribute>'
    write(xdmf_file_id,'(g0)') '  </Grid>'
    write(xdmf_file_id,'(g0)') ' </Domain>'
    write(xdmf_file_id,'(g0)') '</Xdmf>'
    close(xdmf_file_id)

end subroutine writeFields3D_xmf_dp