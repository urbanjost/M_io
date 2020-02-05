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

- [fileclose](fileclose.3m_io.md)
- [filedelete](filedelete.3m_io.md)
- [fileopen](fileopen.3m_io.md)

- [read_all](read_all.3m_io.md)
- [read_line](read_line.3m_io.md)
- [read_table](read_table.3m_io.md)
- [slurp](slurp.3m_io.md)
- [swallow](swallow.3m_io.md)

- [get_tmp](get_tmp.3m_io.md)
- [uniq](uniq.3m_io.md)

- [dirname](dirname.3m_io.md)
- [splitpath](splitpath.3m_io.md)

- [notopen](notopen.3m_io.md)

- [print_inquire](print_inquire.3m_io.md)
