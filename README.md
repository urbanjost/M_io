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
    # change Makefile if not using gfortran(1)
    make

This will compile the M_io module and build all the example programs from
the document pages in the PROGRAMS/ sub-directory.

- [fileclose](fileclose.3m_io.md)  A simple close of a sequential file
- [filedelete](filedelete.3m_io.md)
- [fileopen](fileopen.3m_io.md) A simple open of a sequential file

- [read_all](read_all.3m_io.md) read a line from specified LUN into allocatable string up to line length limit
- [read_line](read_line.3m_io.md) read a line from specified LUN into allocatable string up to line length limit cleaning up input line
- [read_table](read_table.3m_io.md) read file containing a table of numeric values
- [slurp](slurp.3m_io.md) read a file into a character array
- [swallow](swallow.3m_io.md) read a file into a character array line by line

- [get_tmp](get_tmp.3m_io.md) Return the name of the scratch directory
- [uniq](uniq.3m_io.md) append a number to the end of filename to make a unique name if name exists

- [dirname](dirname.3m_io.md) dirname  strip last component from filename
- [splitpath](splitpath.3m_io.md) split a Unix pathname into components

- [notopen](notopen.3m_io.md) Find a FUN/LUN (Fortran-unit-number) that is not in use

- [print_inquire](print_inquire.3m_io.md) Do INQUIRE on file by name/number and print results

