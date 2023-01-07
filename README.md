# M_io.f90 and associated files

## NAME

   M_io - Fortran module for common I/O tasks

## DESCRIPTION

This package is a self-contained version of the M_io library from the GPF
(General Purpose Fortran) package that has been extracted for those just
interested in a library of IO-related functions. In the GPF package
this library is intertwined with several other large modules.

There are a lot of related functions in 
 [M_system](https://urbanjost.github.io/M_system/man3.html) 
but note that it requires a POSIX system at the time of this writing, and
 [M_strings](https://urbanjost.github.io/M_strings/man3.html) has a lot
 of functions for pure string manipulation.
 The procedures collected into M_io are hopefully very portable, unlike
 in M_system(3f).

---
![docs](docs/images/docs.gif)
---

## DOCUMENTATION
### USER 

In the docs/ directory there is

 - An [index](https://urbanjost.github.io/M_io/man3.html) to HTML versions
   of the man-pages 

 - A single page that uses javascript to combine all the HTML descriptions
   of the man-pages is at
   [BOOK_M_io](https://urbanjost.github.io/M_io/BOOK_M_io.html).

---
![manpages](docs/images/manpages.gif)
### real man-pages
 - man-pages are in archive files that can be installed on ULS (Unix-like systems)
    + [manpages.zip](https://urbanjost.github.io/M_io/manpages.zip) 
    + [manpages.tgz](https://urbanjost.github.io/M_io/manpages.tgz) 

 - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### DEVELOPER 

 - The code was run through [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html)
   to produce a [developers' document](https://urbanjost.github.io/M_strings/fpm-ford/index.html).

 - [github action status](docs/STATUS.md)

---
![demos](docs/images/demo.gif)
---

## DEMO PROGRAMS

There are demo programs extracted from the man pages in the example/
directory.

---
![gmake](docs/images/gnu.gif)
---

## BUILD WITH MAKE

    git clone https://github.com/urbanjost/M_io.git
    cd M_io/src
    # change Makefile if not using one of the listed compilers
     
    # for gfortran
    make clean
    make F90=gfortran gfortran
     
    # for ifort
    make clean
    make F90=ifort ifort

This will compile the M_io module and build all the example programs from
the document pages in the `example/` sub-directory.

---
![fpm](docs/images/fpm_logo.gif)
---

## BUILD WITH FPM

Alternatively, download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_io.git
     cd M_io
     fpm test  # build and run unit tests
```

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     M_io        = { git = "https://github.com/urbanjost/M_io.git" }
```
