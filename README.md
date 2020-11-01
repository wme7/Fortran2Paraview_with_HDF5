# Fortran2Paraview_with_HDF5
This example is mean to show how to export data from fortran using XDMF/HDF5 solution. This snippet can also be used as translator of binary (unformatted) data into HDF5 format.

![Visualization of pressure field in field_d0_it0.h5](/figures/ParaView_ScreenShot.png)

## HDF5 installation
Download the `hdf5` library [here](https://www.hdfgroup.org/downloads/hdf5/) and install it in serial-mode (single CPU mode) as:
```
$ FC=/path/to/gfortran ./configure --enable-fortran --enable-shared --prefix=/install/path/hdf5/ 
```

Or, if that you have an `MPI` compiler already installed, in parallel-mode as:
```
$ FC=/path/to/mpif90 ./configure --enable-parallel --enable-fortran --enable-shared --prefix=/install/path/hdf5/
```

## Serial compilation of examples
This can be simply done as 
```
$ gfrotran -o build/example.run src/bin2hdf5_fields_serial.f90 -I/path/to/phdf5/include -L/path/to/phdf5/lib -lhdf5_fortran 
```

Or using the `Makefile` provided in this repository. Make sure to set the correct path your hdf5 folder installation so you can simply type in terminal:
```
$ make
```

All executables created are saved inside the `/build` directory.

To clean the repository folder and delete existing executables:
```
$ make clean
```

## Mode of use
First call the `createSample_binData_serial.run` to build sample binary file containing a 3D array of 100x51x32 points. Simply type on terminal:
```
$ ./createSample_binData_serial.run 100 51 32
```

It will create a `data.bin`. Now we can call the translator to HDF5 as:
```
$ ./bin2hdf5_data_serial.run data.bin 100 51 32
```

Proceed is similarl manner with `createSample_binFields_serial.run` and `bin2hdf5_fields_serial.run`. The program will always print all files it has written. 
In the `fields translators`, I have included a subroutine that prints the XDMF file. So that, the information of a 3D scalar field can be inmediatly visualized in Paraview.

Happy coding! ;D

M.A.Diaz
