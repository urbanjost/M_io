# M_io.f90 and associated files

## NAME

   M_io - Fortran module for common I/O tasks

## DESCRIPTION

This package is a self-contained version of the M_io library from
the GPF (General Purpose Fortran) package that has been extracted for
those just interested in a library of string-related functions. In the
GPF package this library is intertwined with several other large modules.

    git clone https://github.com/urbanjost/M_io.git
    cd M_io/src
    # change Makefile if not using one of the listed compilers
     
    # for gfortran
    make clean
    make F90=gfortran gfortran
     
    # for ifort
    make clean
    make F90=ifort ifort

    # for nvfortran
    make clean
    make F90=nvfortran nvfortran

This will compile the M_io module and build all the example programs from
the document pages in the `test/` sub-directory.

- [fileclose](md/fileclose.3m_io.md)  A simple close of a sequential file
- [filedelete](md/filedelete.3m_io.md) A simple close of an open file with STATUS='DELETE'
- [fileopen](md/fileopen.3m_io.md) A simple open of a sequential file

- [getline](md/getline.3m_io.md) read a line from specified LUN into allocatable string up to line length limit
- [read_line](md/read_line.3m_io.md) read a line from specified LUN into allocatable string up to line length limit cleaning up input line
- [read_table](md/read_table.3m_io.md) read file containing a table of numeric values
- [slurp](md/slurp.3m_io.md) read a file into a character array
- [swallow](md/swallow.3m_io.md) read a file into a character array line by line

- [get_tmp](md/get_tmp.3m_io.md) Return the name of the scratch directory
- [uniq](md/uniq.3m_io.md) append a number to the end of filename to make a unique name if name exists

- [dirname](md/dirname.3m_io.md) dirname  strip last component from filename
- [splitpath](md/splitpath.3m_io.md) split a Unix pathname into components

- [notopen](md/notopen.3m_io.md) Find a FUN/LUN (Fortran-unit-number) that is not in use

- [print_inquire](md/print_inquire.3m_io.md) Do INQUIRE on file by name/number and print results

