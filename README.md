## M_io.f90 and associated files

### Name

   M_io - Fortran module for common I/O tasks

### Description

This package is a self-contained version of the M_io library from the GPF
(General Purpose Fortran) package that has been extracted for those just
interested in a library of IO-related functions. In the GPF package this
library is intertwined with several other large modules.

The procedures collected into M_io do not depend on interfacing to
system-dependent functionality such as POSIX functions. They are
self-contained standard Fortran.

There are related functions in 
 [M_system](https://urbanjost.github.io/M_system/man3.html) 
that require a POSIX system.

 [M_display](https://urbanjost.github.io/M_display/man3.html) is
 a collection of routines for writing tables.

 [M_strings](https://urbanjost.github.io/M_strings/man3.html) has 
 functions for pure string manipulation.

#### File OPs
 + **fileopen**     - A simple open of a sequential file 
 + **filewrite**    - A simple write of a CHARACTER array to a file 
 + **fileread**     - read a file into a string array 
 + **read_table**   - read file containing a table of numeric values 
 + **filebyte**     - read a file into a character array 
 + **fileclose**    - A simple close of a sequential file 
 + **filedelete**   - A simple close of an open file with STATUS='DELETE' 
#### Sequential Reads
 + **rd**            - ask for string from standard input with user-definable prompt 
 + **get_next_char** - read from a file one character at a time 
 + **getline**       - read a line from specified LUN into allocatable string up to line length limit 
 + **read_line**     - read a line from specified LUN into allocatable string up to line length limit cleaning up input line 
#### Manipulate Pathnames
 + **joinpath**      - join parts of a pathname together 
 + **basename**      - return last component from filename 
 + **splitpath**     - split a Unix pathname into components 
 + **dirname**       - strip last component from filename 
#### Find Pathnames
 + **lookfor**       - look for a filename in a number of directories specified by an environment variable 
 + **which**         - given a command name find the pathname by searching the directories in the environment variable $PATH 
 + **getname**       - get name of the current executable 
 + **get_tmp**       - Return the name of the scratch directory 
 + **scratch**       - Return the name of a scratch file 
 + **uniq**          - append a number to the end of filename to make a unique name if name exists 
#### Other
 + **get_env**           - a function returning the value of an environment variable 
 + **print_inquire**     - Do INQUIRE on file by name/number and print results 
 + **notopen**           - Find a FUN/LUN (Fortran-unit-number) that is not in use 
 + **number_of_lines**   - read an open sequential file to get number of lines 
 + **separator**         - try to determine pathname directory separator character 
 + **generate_filename** - generate a filename containing the specified whole number

![docs](docs/images/docs.gif)

### Documentation
#### User 

In the docs/ directory there is

 - An [index](https://urbanjost.github.io/M_io/man3.html) to HTML versions
   of the man-pages 

 - A single page that uses javascript to combine all the HTML descriptions
   of the man-pages is at
   [BOOK_M_io](https://urbanjost.github.io/M_io/BOOK_M_io.html).

 - ![manpages](docs/images/manpages.gif)
   man-pages are in archive files that can be installed on ULS (Unix-like systems)
    + [manpages.zip](https://urbanjost.github.io/M_io/manpages.zip) 
    + [manpages.tgz](https://urbanjost.github.io/M_io/manpages.tgz) 

 - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

#### Developer 

 - The code was run through [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html)
   to produce a [developers' document](https://urbanjost.github.io/M_strings/fpm-ford/index.html).

 - [github action status](docs/STATUS.md)

![demos](docs/images/demo.gif)

### Demo Programs

There are demo programs extracted from the man pages in the [example/](example/)
directory.

![gmake](docs/images/gnu.gif)

### Build with make

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

![fpm](docs/images/fpm_logo.gif)

### Build with fpm

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
