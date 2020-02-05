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
