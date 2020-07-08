!===================================================================================================================================
MODULE M_io
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
implicit none
private
public uniq
public print_inquire
public notopen
public slurp
public swallow
public dirname
public splitpath
public fileopen
public fileclose
public filedelete
public get_tmp
public read_line
public read_all
public read_table

character(len=*),parameter::ident_1="@(#)M_io::read_table(3f): read file containing a table of numeric values"

interface read_table
   module procedure read_table_real, read_table_doubleprecision
end interface

interface string_to_value
   module procedure a2d, a2r, a2i
end interface

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface

CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io] append a number to the end of filename to make a unique name if name exists
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=:),allocatable function uniq(name,istart,verbose,create)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!       logical,intent(in),optional :: verbose
!!       logical,intent(in),optional :: create
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a number to the end of the name and
!!    test if the new name exists. If necessary, increment the number and
!!    try again up to the value 9999999. By default an empty file is created
!!    if an unused name is found.
!!
!!    o relatively non-generic;
!!    o does not try to detect io errors
!!
!!##OPTIONS
!!    name     base input name used to create output filename
!!             If name ends in "." a numeric suffix is always added.
!!    istart   number to start with as a suffix. Default is 1. Must be a
!!             positive integer less than 9999999.
!!    verbose  writes extra messages to stdout. Defaults to .false.
!!    create   create file if new name is successfully found. Defaults
!!             to .true. .
!!
!!##RETURNS
!!    uniq     A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed. If could
!!             not find a unique name a blank is returned.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=4096) :: myname
!!       integer             :: i
!!          myname=uniq('does_not_exist')
!!          open(unit=10,file='does_exist')
!!          write(*,*)'name stays the same ',trim(myname)
!!          myname=uniq('does_exist')
!!          write(*,*)'name has suffix added ',trim(myname)
!!          do i=1,10
!!             myname=uniq('does_exist')
!!             write(*,*) 'FILENAME:',trim(myname)
!!             open(unit=20+i,file=myname)
!!          enddo
!!       end program demo_uniq
!!
!!    Expected output
!!
!!     name stays the same does_not_exist
!!     name has suffix added does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!     FILENAME:does_exist0011
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!! Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
function uniq(name,istart,verbose,create)
implicit none

character(len=*),parameter::ident_3="&
&@(#)M_io::uniq(3f): append a number to the end of filename to make a unique name if name exists"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: name
character(len=:),allocatable :: uniq
integer,intent(in),optional  :: istart
logical,intent(in),optional  :: verbose
logical,intent(in),optional  :: create
!-----------------------------------------------------------------------------------------------------------------------------------
logical                     :: around
integer,save                :: icount=1           ! counter to generate suffix from
character(len=4096),save    :: lastname=' '       ! name called with last time the routine was called
integer                     :: ilen
integer                     :: itimes
integer                     :: iscr
integer                     :: ios
logical                     :: verbose_local
logical                     :: create_local
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=trim(name)                                   ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=1                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(create))then
      create_local=create
   else
      create_local=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      icount=istart                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.ne.0)then                                 ! a blank input name so name will just be a suffix
      if(name(ilen:ilen).ne.'.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=trim(name)
            if(create_local)then
               open(newunit=iscr,file=uniq,iostat=ios,status='new')
               close(unit=iscr,iostat=ios)
            endif
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   deallocate(uniq)
   allocate(character(len=ilen+8) :: uniq)            ! make it useable with an internal WRITE(3f) with room for a numeric suffix
   uniq(:)=name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes.ge.9999999)then                       ! if too many tries to be reasonable give up
         write(*,*) '*uniq* unable to find a unique filename. Too many tries'
         uniq=''
         return
      endif
      if(icount.gt.9999999) icount=1                  ! reset ICOUNT when it hits arbitrary maximum value
      if(icount.le.9999)then
         write(uniq(ilen+1:),'(i4.4)')icount          ! create name by adding a numeric string to end
      else
         write(uniq(ilen+1:),'(i7.7)')icount          ! create name by adding a numeric string to end
      endif
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq,exist=around)                 ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         if(verbose_local)then
            write(*,*)trim('*uniq* name='//trim(uniq)) ! write out message reporting name used
         endif
         if(create_local)then
            open(newunit=iscr,file=uniq,iostat=ios,status='new')
            close(unit=iscr,iostat=ios)
         endif
         uniq=trim(uniq)
         return                                       ! return successfully
      endif
      itimes=itimes+1                                 ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_inquire(3f) - [M_io] Do INQUIRE on file by name/number and print results
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(lun)
!!      or
!!    subroutine print_inquire(name)
!!    integer,intent(in),optional          :: lun
!!    character(len=*),intent(in),optional :: name
!!
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the INQUIRE(3f)
!!    intrinsic and print typical status information.
!!
!!##OPTIONS
!!    lun    if lun is not equal to -1 then query by number and ignore
!!           filename even if present
!!    name   if lun = -1  or is not present then query by this filename
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print_inquire
!!    use M_io, only : print_inquire, fileopen
!!    implicit none
!!    character(len=4096)  :: filename
!!    character(len=20)    :: mode
!!    integer              :: ios
!!    character(len=256)   :: message
!!    integer              :: lun
!!       do
!!          write(*,'(a)',advance='no')'enter filename>'
!!          read(*,'(a)',iostat=ios)filename
!!          if(ios.ne.0)exit
!!          write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
!!          read(*,'(a)',iostat=ios)mode
!!          if(ios.ne.0)exit
!!          lun=fileopen(filename,mode,ios)
!!          if(ios.eq.0)then
!!             write(*,*)'OPENED'
!!          else
!!             write(*,*)'ERROR: IOS=',ios
!!          endif
!!          if(lun.ne.-1)then
!!             call print_inquire(lun,'')
!!             close(lun,iostat=ios,iomsg=message)
!!             if(ios.ne.0)then
!!                write(*,'(a)')trim(message)
!!             endif
!!          endif
!!       enddo
!!    end program demo_print_inquire
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine print_inquire(lun_in,namein_in) ! Version: JSU-1997-12-31, 2020-01-11

character(len=*),parameter::ident_4="@(#)M_io::print_inquire(3f): Do INQUIRE on file by name/number and print results"

integer,intent(in),optional             :: lun_in        ! if unit >= 0 then query by unit number, else by name
character(len=*),intent(in),optional    :: namein_in
integer                        :: ios
character(len=256)             :: message
character(len=:),allocatable   :: namein
integer                        :: lun
!==============================================================================================
!  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
!  ACTION    =  READ        | WRITE         |  READWRITE
!  FORM      =  FORMATTED   |  UNFORMATTED
!  POSITION  =  ASIS        |  REWIND       |  APPEND
!  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
character(len=20)              :: access         ; namelist/inquire/access
character(len=20)              :: action         ; namelist/inquire/action
character(len=20)              :: asynchronous   ; namelist/inquire/asynchronous
character(len=20)              :: blank          ; namelist/inquire/blank
character(len=20)              :: decimal        ; namelist/inquire/decimal
character(len=20)              :: delim          ; namelist/inquire/delim
character(len=20)              :: direct         ; namelist/inquire/direct
character(len=20)              :: encoding       ; namelist/inquire/encoding
logical                        :: exist          ; namelist/inquire/exist
character(len=20)              :: form           ; namelist/inquire/form
character(len=20)              :: formatted      ; namelist/inquire/formatted
integer                        :: id             ; namelist/inquire/id
character(len=20)              :: name           ; namelist/inquire/name
logical                        :: named          ; namelist/inquire/named
integer                        :: nextrec        ; namelist/inquire/nextrec
integer                        :: number         ; namelist/inquire/number
logical                        :: opened         ; namelist/inquire/opened
character(len=20)              :: pad            ; namelist/inquire/pad
logical                        :: pending        ; namelist/inquire/pending
integer                        :: pos            ; namelist/inquire/pos
character(len=20)              :: position       ; namelist/inquire/position
character(len=20)              :: read           ; namelist/inquire/read
character(len=20)              :: readwrite      ; namelist/inquire/readwrite
integer                        :: recl           ; namelist/inquire/recl
character(len=20)              :: round          ; !BUG!namelist/inquire/round
character(len=20)              :: sequential     ; namelist/inquire/sequential
character(len=20)              :: sign           ; !BUG!namelist/inquire/sign
integer                        :: size           ; namelist/inquire/size
character(len=20)              :: stream         ; namelist/inquire/stream
character(len=20)              :: unformatted    ; namelist/inquire/unformatted
character(len=20)              :: write          ; namelist/inquire/write
!==============================================================================================
   namein=merge_str(namein_in,'',present(namein_in))
   lun=merge(lun_in,-1,present(lun_in))
   ! exist, opened, and named always become defined unless an error condition occurs.
   !!write(*,*)'LUN=',lun,' FILENAME=',namein
   !-----------------------------------------------------------------------------------------------------------------------------------
   name=''
   if(namein.eq.''.and.lun.ne.-1)then
         write(*,*)'*print_inquire* checking unit',lun
         inquire(unit=lun,                                                                               &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
    elseif(namein.ne.'')then
         write(*,*)'*print_inquire* checking file:'//namein
         inquire(file=namein,                                                                            &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
     if(name.eq.'')name=namein
    else
       write(*,*)'*print_inquire* must specify either filename or unit number'
    endif
!-----------------------------------------------------------------------------------------------------------------------------------
   write(*,nml=inquire,delim='none')
   return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
   write(*,*)'*print_inquire* bad inquire'
!  If an error condition occurs during execution of an INQUIRE  statement,
!  all of the inquiry identifiers except ios become undefined.
   write(*,*)'*print_inquire* inquire call failed,iostat=',ios,'message=',message
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    read_table(3f) - [M_io] read file containing a table of numeric values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine read_table(filename,array,ierr)
!!
!!    character(len=*),intent(in)             :: filename
!!
!!    doubleprecision,allocatable,intent(out) :: array(:,:)
!!    ! or
!!    real           ,allocatable,intent(out) :: array(:,:)
!!
!!    integer,intent(out)                     :: ierr
!!
!!##DESCRIPTION
!!    Read a table from a file that is assumed to be columns of
!!    space-delimited numbers, with each row containing the same
!!    number of values
!!
!!##OPTIONS
!!    filename   filename to read
!!    array      array to create
!!    ierr       zero if no error occurred
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
!!
!!     program demo_read_table
!!     use M_io, only : read_table
!!     doubleprecision,allocatable :: array(:,:)
!!
!!     ! create test file
!!     open(file='inputfile',unit=10)
!!     write(10,'(a)') '1 10  45'
!!     write(10,'(a)') '10 10  45'
!!     write(10,'(a)') '  2 20  15'
!!     write(10,'(a)') ' 20.345 20  15'
!!     write(10,'(a)') '  3 30.111   0'
!!     write(10,'(a)') '30 30e3   0'
!!     write(10,'(a)') '  4 300.444e-1 -10'
!!     write(10,'(a)') '40 30.5555d0 -10'
!!     write(10,'(a)') '  4 300.444E-1 -10'
!!     write(10,'(a)') '40 30.5555D0 -10'
!!     close(unit=10)
!!
!!     ! read file as a table
!!     call read_table('inputfile',array,ierr)
!!
!!     ! print values
!!     write(*,*)'size=       ',size(array)
!!     write(*,*)'size(dim=1)=',size(array,dim=1)
!!     write(*,*)'size=(dim=2)',size(array,dim=2)
!!     do i=1,size(array,dim=1)
!!        write(*,*)array(i,:)
!!     enddo
!!
!!     ! remove sample file
!!     open(file='inputfile',unit=10)
!!     close(unit=10,status='delete')
!!
!!     end program demo_read_table
!!
!!   Results:
!!
!!     size=          30
!!     size(dim=1)=   10
!!     size(dim=2)=    3
!!       1.0000000000000000        10.000000000000000        45.000000000000000
!!       10.000000000000000        10.000000000000000        45.000000000000000
!!       2.0000000000000000        20.000000000000000        15.000000000000000
!!       20.344999999999999        20.000000000000000        15.000000000000000
!!       3.0000000000000000        30.111000000000001        0.0000000000000000
!!       30.000000000000000        30000.000000000000        0.0000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine read_table_doubleprecision(filename,array,ierr)
implicit none

character(len=*),intent(in)             :: FILENAME
doubleprecision,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_doubleprecision* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          if(text(i).eq.char(9))then
             line(i:i)=' '
          else
             line(i:i)=text(i)
          endif
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0d0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             if(text(i).eq.char(9))then
                line(k:k)=' '
             else
                line(k:k)=text(i)
             endif
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_doubleprecision
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_table_real(filename,array,ierr)
implicit none

character(len=*),intent(in)             :: FILENAME
real,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_real* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          if(text(i).eq.char(9))then
             line(i:i)=' '
          else
             line(i:i)=text(i)
          endif
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             if(text(i).eq.char(9))then
                line(k:k)=' '
             else
                line(k:k)=text(i)
             endif
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    swallow(3f) - [M_io] read a file into a character array line by line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine swallow(filename,pageout)
!!
!!    character(len=*),intent(in) :: filename
!!      or
!!    integer,intent(in)          :: io
!!
!!    character(len=1),allocatable,intent(out) :: pageout(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a character array, one character
!!    variable per line.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory, or LUN (Fortran Logical Unit Number).
!!                  If filename is a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       pageout    array of characters to hold file
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_swallow
!!    use M_io,      only : swallow
!!    implicit none
!!    character(len=4096)          :: FILENAME   ! file to read
!!    character(len=:),allocatable :: pageout(:) ! array to hold file in memory
!!    integer                      :: longest, lines, i, ilen
!!    character(len=:),allocatable :: line
!!       ! get a filename
!!       call get_command_argument(1, FILENAME)
!!       ! allocate character array and copy file into it
!!       call swallow(FILENAME,pageout)
!!       if(.not.allocated(pageout))then
!!          write(*,*)'*demo_swallow* failed to load file '//FILENAME
!!       else
!!          ! write file from last line to first line
!!          longest=len(pageout)
!!          lines=size(pageout)
!!          allocate(character(len=longest)::line)
!!          write(*,*)'number of lines is ',lines
!!          write(*,*)'and length of lines is ',longest
!!          write(*,'(a)')repeat('%',longest+2)
!!          do i=lines,1,-1
!!             write(*,'("%",a,"%")')pageout(i)
!!          enddo
!!          write(*,'(a)')repeat('%',longest+2)
!!          deallocate(pageout)  ! release memory
!!       endif
!!    end program demo_swallow
!!
!!   Given
!!
!!    first line
!!    second line
!!    third line
!!
!!   Expected output
!!
!!     number of lines is 3
!!     and length of lines is 11
!!    %%%%%%%%%%%%%
!!    %third line %
!!    %second line%
!!    %first line %
!!    %%%%%%%%%%%%%
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine swallow(FILENAME,pageout)
implicit none
class(*),intent(in)                      :: FILENAME   ! file to read
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call slurp(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
      select type(FILENAME)
       type is (character(len=*)); write(*,*)'*swallow* failed to load file '//FILENAME
       type is (integer);          write(*,'(a,i0)')'*swallow* failed to load file unit ',FILENAME
      end select
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif

contains
function page(array)  result (table)

character(len=*),parameter::ident_5="@(#)M_strings::page(3fp): function to copy char array to page of text"

character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl=char(10)
   lines=0
   linelength=0
   length=0
   sz=size(array)
   do i=1,sz
      if(array(i).eq.nl)then
         linelength=max(linelength,length)
         lines=lines+1
         length=0
      else
         length=length+1
      endif
   enddo
   if(sz.gt.0)then
      if(array(sz).ne.nl)then
         lines=lines+1
      endif
   endif

   allocate(character(len=linelength) :: table(lines))
   table=' '

   linecount=1
   position=1
   do i=1,sz
      if(array(i).eq.nl)then
         linecount=linecount+1
         position=1
      elseif(linelength.ne.0)then
         table(linecount)(position:position)=array(i)
         position=position+1
      endif
   enddo
end function page
end subroutine swallow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SLURP(3f) - [M_io] read a file into a character array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine slurp(filename,text)
!!
!!    character(len=*),intent(in) :: filename
!!     or
!!    integer,intent(in)          :: filenumber
!!
!!    character(len=1),allocatable,intent(out) :: text(:)
!!    integer,intent(out),optional :: length
!!    integer,intent(out),optional :: lines
!!##DESCRIPTION
!!    Read an entire file as a stream into memory as an array of single
!!    characters, retaining line end terminators.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory or LUN (Fortran Logical
!!                  Unit Number) If a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       text       array of characters to hold file
!!       length     length of longest line read(Optional).
!!       lines      number of lines read(Optional).
!!
!!##EXAMPLES
!!
!!    Sample program, which  creates test input file "inputfile":
!!
!!     program demo_slurp
!!     use M_io, only      : slurp
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=*),parameter :: FILENAME='inputfile' ! file to read
!!
!!     ! create test file
!!     open(file=FILENAME,unit=10)
!!     write(10,'(a)') new_line('A')//'esrever lliw'
!!     write(10,'(a)') 'margorp elpmas eht taht'
!!     write(10,'(a)') 'elif elpmas a si sihT'
!!     close(unit=10)
!!
!!     call slurp(FILENAME,text) ! allocate character array and copy file into it
!!
!!     if(.not.allocated(text))then
!!        write(*,*)'*rever* failed to load file '//FILENAME
!!     else
!!        ! write file reversed to stdout
!!        write(*,'(*(a:))',advance='no')text(size(text):1:-1)
!!        deallocate(text)  ! release memory
!!     endif
!!
!!     end program demo_slurp
!!
!!    Expected output:
!!
!!     >This is a sample file
!!     >that the sample program
!!     >will reverse
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine slurp(filename,text,length,lines)
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none

character(len=*),parameter::ident_6="@(#)M_io::slurp(3f): allocate text array and read file filename into it"

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines
!-----------------------------------------------------------------------------------------------------------------------------------
integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: local_filename
!-----------------------------------------------------------------------------------------------------------------------------------
   length_local=0
   lines_local=0
   igetunit=notopen(10,99)         ! find unused file unit number (hopefully)
   if(igetunit.lt.0)then
      call stderr_local('*slurp* could not find unused file unit number')
      return
   endif
!-------------------------------------------
   message=''
      select type(FILENAME)
       type is (character(len=*))
          open(unit=igetunit, file=trim(filename), action="read", iomsg=message,&
           &form="unformatted", access="stream",status='old',iostat=ios)
          local_filename=filename
       type is (integer)
          rewind(unit=filename,iostat=ios,iomsg=message)
          write(local_filename,'("unit ",i0)')filename
          igetunit=filename
      end select
!-------------------------------------------
   if(ios.eq.0)then  ! if file was successfully opened
!-------------------------------------------
      inquire(unit=igetunit, size=nchars)
!-------------------------------------------
      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
      !
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios,iomsg=message) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr_local( '*slurp* bad read of '//trim(local_filename)//':'//trim(message) )
      endif
   else
      call stderr_local('*slurp* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i).eq.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars.ne.0)then
         if(text(nchars).ne.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
use, intrinsic :: iso_fortran_env, only : error_unit
character(len=*) :: message
   write(error_unit,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine slurp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    Usage
!!
!!       integer function notopen(start,end,err)
!!       integer,optional,intent(in)  :: start
!!       integer,optional,intent(in)  :: end
!!       integer,optional,intent(out) :: err
!!##DESCRIPTION
!!    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns
!!    a FORTRAN unit number from START to END not currently associated with
!!    an I/O unit. START and END are expected to be positive integers where
!!    END .ge. START.
!!
!!    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in
!!    the specified range.
!!
!!    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN
!!    logical unit number. Note that NOTOPEN() assumes the following unit
!!    numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module
!!
!!       ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
!!
!!    are special, and will never return those values.
!!
!!##OPTIONS
!!       start  optional logical unit number to start scan at, defaults to 10.
!!       end    optional logical unit number to stop scan at, defaults to 99.
!!       err    optional error flag returned. ERR will be non-zero if no errors.
!!              If not present and an error occurs the program will stop instead
!!              of returning.
!!
!!##NOTES
!!
!!    Why are the default START and END limits from 10 to 99? the Fortran 77
!!    standard did not specify a specific limit on the upper range limit, but
!!    the LUN range of 1 to 99 was almost always supported in conventional
!!    programming environments. Additionally, units in the range 0-10 have
!!    often been the units used for pre-assigned files. Occasionally 100,
!!    101 and 102 are reserved (for files such as standard input, standard
!!    output, standard error, ...). Therefore, the defaults for START and
!!    END were selected to be 10 and 99. And most programs do not need
!!    more than 90 files simultaneously open, so the defaults work well in
!!    practice with many versions/vintages of Fortran.
!!
!!    Note that an environment may impose a limit on the number of
!!    simultaneously open files (which some compilers work around).
!!
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead
!!    of an open unit locator.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_notopen ! test the NOTOPEN(3f) function
!!     use m_io, only: notopen
!!     implicit none
!!     integer :: ii, ierr, igot
!!
!!     write(*,*)'check for preassigned files from unit 0 to unit 1000'
!!     write(*,*)'(5 and 6 always return -1)'
!!
!!     do ii=0,1000
!!        if(notopen(ii,ii,ierr) .ne. ii)then
!!           write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
!!        endif
!!     enddo
!!
!!     ! open all files from UNIT=10 to UNIT=30 so have used units
!!     do ii=10,30,1
!!       open(unit=ii,status="scratch")
!!     enddo
!!     ! close UNIT=25
!!     close(25)
!!
!!     ! find open file in range 10 to 30
!!     write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
!!
!!     close(18)
!!     do ii=10,32
!!       igot=notopen(ii,ii,ierr)
!!       write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
!!     enddo
!!
!!     end program demo_notopen
!!
!!    Expected output(can vary with each programming environment):
!!
!!       check for preassigned files from unit 0 to unit 1000
!!       (5 and 6 always return -1)
!!       INUSE:    0    -1
!!       INUSE:    5    -1
!!       INUSE:    6    -1
!!       Should get 25 for this .. 25
!!       For  unit  10  I  got  -1  with  ERR=  -1
!!       For  unit  11  I  got  -1  with  ERR=  -1
!!       For  unit  12  I  got  -1  with  ERR=  -1
!!       For  unit  13  I  got  -1  with  ERR=  -1
!!       For  unit  14  I  got  -1  with  ERR=  -1
!!       For  unit  15  I  got  -1  with  ERR=  -1
!!       For  unit  16  I  got  -1  with  ERR=  -1
!!       For  unit  17  I  got  -1  with  ERR=  -1
!!       For  unit  18  I  got  18  with  ERR=   0
!!       For  unit  19  I  got  -1  with  ERR=  -1
!!       For  unit  20  I  got  -1  with  ERR=  -1
!!       For  unit  21  I  got  -1  with  ERR=  -1
!!       For  unit  22  I  got  -1  with  ERR=  -1
!!       For  unit  23  I  got  -1  with  ERR=  -1
!!       For  unit  24  I  got  -1  with  ERR=  -1
!!       For  unit  25  I  got  25  with  ERR=   0
!!       For  unit  26  I  got  -1  with  ERR=  -1
!!       For  unit  27  I  got  -1  with  ERR=  -1
!!       For  unit  28  I  got  -1  with  ERR=  -1
!!       For  unit  29  I  got  -1  with  ERR=  -1
!!       For  unit  30  I  got  -1  with  ERR=  -1
!!       For  unit  31  I  got  31  with  ERR=   0
!!       For  unit  32  I  got  32  with  ERR=   0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
integer function notopen(start,end,err)
use, intrinsic :: iso_fortran_env, only : error_unit,input_unit,output_unit     ! access computing environment
implicit none

character(len=*),parameter::ident_7="@(#)M_io::notopen(3f): find free FORTRAN unit number to OPEN() a file"

integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
integer                        :: istart
integer                        :: iend
integer                        :: ierr

integer         :: i10                                            ! counter from start to end
integer         :: ios                                            ! iostatus from INQUIRE
logical         :: lopen                                          ! returned from INQUIRE
logical         :: lexist                                         ! returned from INQUIRE
!-----------------------------------------------------------------------------------------------------------------------------------
   !! IEND=MERGE( END, 99, PRESENT(END)) do not use merge, as TSOURCE must be evaluated before the call
   if(present(start))then; istart=start; else; istart=10; endif
   if(present(end  ))then; iend  =end  ; else; iend  =99; endif
   ierr=0
   notopen=(-1)                                                   ! result if no units are available
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=istart,iend                                             ! check units over selected range
      select case (i10)                                           ! always skip these predefined units
      case(error_unit,input_unit,output_unit)
          cycle
      end select
      inquire( unit=i10, opened=lopen, exist=lexist, iostat=ios )
      if( ios == 0 )then                                          ! no error on inquire
         if(.not. lopen .and. lexist)then                         ! if unit number not in use, return it
            notopen = i10
            exit                                                  ! only need to find one, so return
         endif
      else
         write(error_unit,*)'*notopen*:error on unit ',i10,'=',ios
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if (notopen .lt. 0 )then                                       ! no valid unit was found in given range
      ierr=-1
   else                                                           ! valid value being returned
      ierr=0
   endif
   if(present(err))then                                           ! if error flag is present set it
      err=ierr
   elseif(ierr.ne.0)then                                          ! if error occurred and error flag not present stop program
      stop 1
   endif
end function notopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dirname(3f) - [M_io] strip last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing
!!    slashes removed. If FILENAME contains no '/' character, output
!!    '.' (meaning the current directory).
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to remove the last leaf from
!!
!!##RETURNS
!!      DIRECTORY  directory name for pathname
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dirname
!!    use M_io, only : dirname
!!    implicit none
!!    character(len=:),allocatable :: filename
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1 , command_argument_count()
!!       call get_command_argument (i , length=filename_length)
!!       allocate(character(len=filename_length) :: filename)
!!       call get_command_argument (i , value=filename)
!!       write(*,'(a)')dirname(filename)
!!       deallocate(filename)
!!    enddo
!!    end program demo_dirname
!!
!!   Sample program executions:
!!
!!      demo_dirname /usr/bin/          -> "/usr"
!!      demo_dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      demo_dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(3c), basename(3c), readlink(3c), realpath(3c)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function dirname(filename) result (directory)
implicit none

character(len=*),parameter::ident_8="@(#)M_io::dirname(3f): strip last component from filename"

character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,'/',back=.true.)     ! find last slash if any
   if(iend.eq.0)then                         ! filename is a leaf
      directory='.'                          ! special case
   else
      directory=directory(:iend-1)           ! remove leaf
      call removetail()                      ! trim off trailing slashes in case duplicates
   endif
   directory=trim(directory)                 ! clean up return value
contains
   subroutine removetail()              ! replace trailing slashes with spaces even if duplicates
   integer :: right
   do right=len(directory),1,-1
      if(directory(right:right).eq.'/'.or.directory(right:right).eq.' ')then
         directory(right:right)=' '
      else
         exit
      endif
   enddo
   end subroutine removetail

end function dirname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileopen(3f) - [M_io] A simple open of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fileopen(filename,mode,ios) result(lun)
!!
!!    character(len=*),intent(in)           :: filename
!!    character(len=*),intent(in),optional  :: mode
!!    integer,intent(out),optional          :: ios
!!    integer                               :: lun
!!
!!##DESCRIPTION
!!    fileopen(3f) is a convenience routine that allows you to open a file
!!    for sequential reading and writing as a text file in a form commonly
!!    found in C and interpreted languages such as shells. See the OPEN(3f)
!!    statement for more demanding I/O specifications (asynchronous, direct,
!!    unformatted, ... ). The documentation for the flexible and powerful
!!    OPEN(3f) statement can be a bit overwhelming; this routine cuts it
!!    down to the just the simple basic functions typically available in
!!    a scripting language such as bash, tcsh, sh, ...
!!
!!    Specify the file's name as the string FILENAME with a shell-like prefix
!!    specifying the access mode, or alternatively specify a plain FILENAME
!!    and the kind of access you need to the file with the string MODE.
!!
!!    Three fundamental kinds of access are available: read, write,
!!    and append.
!!
!!##OPTION
!!   FILENAME  The filename to open. If the beginning of the filename is
!!
!!             <   open for read. File must exist
!!             >   open for write. Will overwrite current file
!!             >>  open for append. Will append to current file
!!
!!             If no prefix exists to specify a file access mode, it
!!             will depend on the values of the MODE argument (meaning
!!             the default will be "readwrite").
!!
!!             A blank filename causes a unit number for a scratch file
!!             to be returned.
!!
!!   MODE     [rwa][tb][+]
!!            An alternate way to specify the file access mode is to specify
!!            a MODE value. It should begin with one of the three characters
!!            "r", "w", or "a". It defaults to 'rw'. It is case-insensitive.
!!
!!
!!        READING PREFIX
!!        r,<   Open the file for reading; the operation will fail if the
!!              file does not exist, or if the host system does not permit
!!              you to read it.
!!
!!        WRITING PREFIXES
!!        w,>   Open a file for writing from the beginning of the file.
!!              If the file whose name you specified already existed,
!!              the call fails.
!!
!!        o     Open the file for writing from the beginning of the file:
!!              effectively, this always creates a new file. If the file
!!              whose name you specified already existed, its old contents
!!              are discarded.
!!
!!        a,<<  Initially open the file for appending data (ie. writing
!!              at the end of file).
!!
!!        SUFFIX
!!
!!        b   Append a "b" to any of the three modes above to specify that
!!            you are opening the file as a "binary file" (the default is
!!            to open the file as a sequential formatted text file. This
!!            switch changes to to an unformatted stream).
!!
!!                   open( ... access='stream';form='unformatted')
!!
!!        t   Append a "t" to any of the three modes (rwa) to specify a
!!            formatted stream
!!
!!                   open( ... access='stream';form='formatted')
!!
!!        +   Finally, you might need to both read and write from the same
!!            file. You can specify "rw" or you can append a `+' to any of
!!            the three primary modes ("rwa") to permit "readwrite" access
!!
!!        v   Additionally, "v" selects verbose mode, which prints the
!!            OPEN(3f) options explicitly selected
!!
!!        NOTES
!!
!!            If you want to append both `b' and `+', you can do it in
!!            either order: for example, "rb+" means the same thing as
!!            "r+b" when used as a mode string.)
!!
!!    IOS    The error code returned by the OPEN(3f) statement ultimately
!!           executed by this function. If not present the program stops on
!!           an error.
!!##RETURNS
!!        FILEOPEN(3f) returns a Fortran unit number which you can use for other file
!!        operations, unless the file you requested could not be opened;
!!        in that situation, the result is -1 (a reserved value that cannot be returned
!!        as a NEWUNIT value on an OPEN(3f)) and IOS will be non-zero.
!!
!!##EXAMPLE
!!
!!  Common usage
!!
!!   READ
!!     R=fileopen('<in.txt')
!!     or
!!     R=fileopen('in.txt','r')
!!
!!   WRITE
!!     W=fileopen('>out.txt')
!!     or
!!     W=fileopen('out.txt','W')
!!
!!   READWRITE
!!     RW=fileopen('inout.txt')
!!
!!   APPEND
!!     A=fileopen('>>inout.txt')
!!     or
!!     A=fileopen('inout.txt','a')
!!
!!   Sample program
!!
!!       program demo_fileopen
!!       use M_io, only : fileopen, fileclose, print_inquire
!!       integer :: lun
!!       lun=fileopen('fred.txt')
!!       call print_inquire(lun)
!!       end program demo_fileopen
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileopen(filename,mode,ios) result(lun)
character(len=*),intent(in)           :: filename
character(len=*),intent(in),optional  :: mode
integer,intent(out),optional          :: ios
integer                               :: lun, i, ios_local,ifound,gts
character(len=:),allocatable          :: local_mode
character(len=256)                    :: message
character(len=:),allocatable          :: action, position, access, form, status, file
logical                               :: verbose
   local_mode=lower(merge_str(mode,'',present(mode)))
   file=trim(adjustl(filename))//'   '
   ifound=index(file,'>>')
   if(ifound.ne.0)then
      file(ifound:ifound+1)='  '
      local_mode=local_mode//'a'
   endif
   ifound=index(file,'>')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'w'
   endif
   ifound=index(file,'<')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'r'
   endif
   file=adjustl(file)
   local_mode=merge_str('rw',local_mode,local_mode.eq.'')
   file=trim(file)

   gts=0
   action=''
   position='asis'
   form='formatted'
   access='sequential'
   status='unknown'
   verbose=.false.
   do i=1,len(local_mode) ! create order independence
      select case(local_mode(i:i))
       case('r','<'); if(action.ne.'readwrite'.and.action.ne.'read')action='read'//action
                      if(status.eq.'unknown')status='old'
       case('w','>'); if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')status='new'
                      if(gts.gt.0)then
                         position='append'
                      endif
                      gts=gts+1
       case('o');     if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')then
                         status='replace'
                      endif
       case('a');     position='append'
                      if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status.eq.'old')status='unknown'
       case('b');     access='stream';form='unformatted'
       case('t');     access='stream';form='formatted'
       case('+');     action='readwrite'
                      status='unknown'
       case('v');     verbose=.true.
       case default
         write(*,'(*(g0))',advance='no')'*fileopen* unknown mode key ',local_mode(i:i)
         write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & ' INPUTNAME=',trim(file), &
         & ' MODE=',trim(local_mode)
      end select
   enddo
   if(action.eq.'')action='readwrite'

   if(verbose)then
      write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & 'INPUTNAME=',trim(file), &
         & 'MODE=',trim(local_mode)
      write(*,'(a)',advance='no')'==>'
      write(*,'(*(:,"[",g0,"=",g0,"]"))') &
         & 'FILE=',trim(file), &
         & 'FORM=',trim(form), &
         & 'ACCESS=',trim(access), &
         & 'ACTION=',trim(action), &
         & 'POSITION=',trim(position), &
         & 'STATUS=',trim(status)
   endif
   if(file.ne.' ')then
    open(file=file,newunit=lun,form=form,access=access,action=action,position=position,status=status,iostat=ios_local,iomsg=message)
   else
    open(newunit=lun,form=form,access=access,action=action,status='scratch',iostat=ios_local,iomsg=message)
   endif
   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
   !  ACTION    =  READ|WRITE  |  READWRITE
   !  FORM      =  FORMATTED   |  UNFORMATTED
   !  POSITION  =  ASIS        |  REWIND       |  APPEND
   !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
   if(ios_local.ne.0)then
      write(*,*)'*fileopen* ',message
      lun=-1
   endif
   if(present(ios))then        ! caller has asked for status so let caller process any error
      ios=ios_local
   elseif(ios_local.ne.0)then  ! caller did not ask for status so stop program on error
      stop 1
   endif
end function fileopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileclose(3f) - [M_io] A simple close of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function fileclose(lun) result(ios)
!!
!!      integer,intent(in)       :: lun
!!      integer                  :: ios
!!##DESCRIPTION
!!   A convenience command for closing a file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN unit number to close
!!##RETURNS
!!   IOS status value from CLOSE
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_fileclose
!!     use M_io, only : fileclose, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=fileclose(lun)
!!     end program demo_fileclose
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileclose(lun) result(ios)
integer,intent(in)       :: lun
integer                  :: ios
character(len=256)       :: message
   close(unit=lun,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)'*fileclose* ',trim(message)
      stop
   endif
end function fileclose
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filedelete(3f) - [M_io] A simple close of an open file with STATUS='DELETE'
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function filedelete(lun) result(ios)
!!
!!     integer,intent(in)    :: lun
!!     integer               :: ios
!!
!!##DESCRIPTION
!!   A convenience command for deleting an OPEN(3f) file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN  unit number of open file to delete
!!##RETURNS
!!   IOS  status returned by CLOSE().
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filedelete
!!     use M_io, only : filedelete, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=filedelete(lun)
!!     end program demo_filedelete
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filedelete(lun) result(ios)
integer,intent(in)    :: lun
integer               :: ios
character(len=256)    :: message
   close(unit=lun,iostat=ios,status='delete',iomsg=message)
   if(ios.ne.0)then
      write(*,*)'*filedelete* ',trim(message)
   endif
end function filedelete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of
!!    the filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine splitpath(path,dir,name,basename,ext)
implicit none

character(len=*),parameter::ident_9="@(#)M_io::splitpath(3f): split Unix pathname into components (dir,name,basename,extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=*),intent(out),optional :: dir
character(len=*),intent(out),optional :: name
character(len=*),intent(out),optional :: basename
character(len=*),intent(out),optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.'/')then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.'/')then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir_local='/'
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,'/',BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local.eq.'')then
         if(path_local(1:1).eq.'/')then
            dir_local='/'
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local.eq.'.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=dir_local
   if(present(name))name=name_local
   if(present(basename))basename=basename_local
   if(present(ext))ext=ext_local
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_all(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function read_all(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,intent(out)                      :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##OPTIONS
!!
!!    LINE   line read
!!    LUN    optional LUN (Fortran logical I/O unit) number. Defaults
!!           to stdin.
!!##RETURNS
!!
!!    IER    zero unless an error occurred. If not zero, LINE returns the
!!           I/O error message.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_read_all
!!    use M_io, only : read_all
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (read_all(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_read_all
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function read_all(line,lun) result(ier)
use, intrinsic :: iso_fortran_env, only : INPUT_UNIT
implicit none

character(len=*),parameter::ident_10="&
&@(#)M_io::read_all(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier
character(len=4096)                      :: message

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif

   INFINITE: do                                                      ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for files
                                                                     ! other than stdin so system line limit is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)            ! append what was read to result
      if(is_iostat_eor(ier))then                                     ! if hit EOR reading is complete unless backslash ends the line
         ier=0                                                       ! hitting end of record is not an error for this routine
         exit INFINITE                                               ! end of reading line
     elseif(ier.ne.0)then                                            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                                                   ! trim line
end function read_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_line(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit cleaning up input line
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function read_line(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer                                  :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program simple_read_line
!!    use M_io, only : read_line
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (read_line(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program simple_read_line
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_read_line
!!     use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
!!     use,intrinsic :: iso_fortran_env, only : stderr => error_unit
!!     use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
!!     use M_io, only : read_line
!!     implicit none
!!     character (len =: ), allocatable :: line
!!     integer  ::  ios, icount=0
!!        INFINITE: do while (isave (read_line (line), ios) == 0)
!!           write (*, '(*(g0))') icount,' [',line,']'
!!        enddo INFINITE
!!        if ( .not.is_iostat_end(ios) ) then
!!           write (stderr, '(*(g0))') 'error: line ',icount,'==>',trim (line)
!!        endif
!!     contains
!!        integer function isave (iin, iout)
!!        integer, intent (in) :: iin
!!        integer, intent (out) :: iout
!!           iout = iin
!!           isave = iin
!!           icount=icount+1
!!        end function isave
!!     end program demo_read_line
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function read_line(line,lun) result(ier)
use, intrinsic :: iso_fortran_env, only : INPUT_UNIT
implicit none

character(len=*),parameter::ident_11="&
&@(#)M_io::read_line(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=256)                       :: message
integer                                  :: biggest
character(len=buflen)                    :: buffer
integer                                  :: last
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   lun_local=merge(lun,INPUT_UNIT,present(lun))

   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for
                                                                          ! files other than stdin so system line limit
                                                                          ! is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)   ! append what was read to result
      if(is_iostat_eor(ier))then                            ! if hit EOR reading is complete unless backslash ends the line
         last=len(line_local)
         if(last.ne.0)then
            if(line_local(last:last).eq.'\')then            ! if line ends in backslash it is assumed a continued line
               line_local=line_local(:last-1)               ! remove backslash
               cycle INFINITE                               ! continue on and read next line and append to result
            endif
         endif
         ier=0                                              ! hitting end of record is not an error for this routine
         exit INFINITE                                      ! end of reading line
     elseif(ier.ne.0)then                                   ! end of file or error
        line_local=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=trim(line(:last))                                   ! trim line
end function read_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get_tmp(3f) - [M_io] Return the name of the scratch directory
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function get_tmp() result(tname)
!!
!!      character(len=:),allocatable :: tname
!!##DESCRIPTION
!!
!!    Return the name of the scratch directory set by the most common
!!    environment variables used to designate a scratch directory.
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    to use to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is returned. The returned value always ends in
!!    "/". No test is made that the directory exists or is writable.
!!
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_get_tmp
!!     use M_io, only : get_tmp, uniq
!!     implicit none
!!     character(len=:),allocatable :: answer
!!        answer=get_tmp()
!!        write(*,*)'result is ',answer
!!        answer=get_tmp()//uniq('_scratch',create=.false.)
!!        write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
!!     end program demo_get_tmp
!!
!!   Sample Results:
!!
!!     result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function get_tmp() result(tname)

character(len=*),parameter::ident_12="@(#)M_io::get_tmp(3f): Return the name of the scratch directory"

character(len=:),allocatable :: tname
integer                      :: lngth
character(len=10),parameter  :: names(4)=["TMPDIR    ","TEMP      ","TEMPDIR   ","TMP       "]
integer                      :: i
   tname=''
   do i=1,size(names)
      call get_environment_variable(name=names(i), length=lngth)
      if(lngth.ne.0)then
         deallocate(tname)
         allocate(character(len=lngth) :: tname)
         call get_environment_variable(name=names(i), value=tname)
         exit
      endif
   enddo
   if(lngth.eq.0)then
      tname='/tmp'
      lngth=len_trim(tname)
   endif
   if(scan(tname(lngth:lngth),'/\').eq.0)then
      tname=tname//'/'
   endif
end function get_tmp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! COPIES FROM OTHER MODULES
!===================================================================================================================================
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

character(len=*),parameter::ident_37="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in)     :: str1
character(len=*),intent(in)     :: str2
logical,intent(in)              :: expr
character(len=:),allocatable    :: strout
integer                         :: big
   big=max(len(str1),len(str2))
   strout=trim(merge(lenset(str1,big),lenset(str2,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_7="&
&@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function upper(str,begin,end) result (string)

character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)

character(len=*),parameter::ident_23="@(#)M_strings::a2s(3fp): function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

character(len=*),parameter::ident_24="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

character(len=*),parameter::ident_25="@(#)M_strings::s2c(3f): copy string(1:Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function expand(line,escape) result(lineout)
USE ISO_C_BINDING ,ONLY: c_horizontal_tab
implicit none

character(len=*),parameter::ident_29="@(#)M_strings::expand(3f): return string with escape sequences expanded"

character(len=*)                      :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%     escape character           %a     alert (BEL) -- gi is an alias for a
!    %b     backspace                  %c     suppress further output
!    %e     escape                     %E     escape
!    %f     form feed                  %n     new line
!    %r     carriage return            %t     horizontal tab
!    %v     vertical tab
!    %oNNN  byte with octal value NNN (3 digits)
!    %dNNN  byte with decimal value NNN (3 digits)
!    %xHH   byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: ilen
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   ilen=len_trim(line)
   lineout=''

   if(ilen.eq.0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i.gt.ilen)exit
      if(line(i:i).eq.esc)then
         i=i+1
         if(i.gt.ilen)exit
         if(line(i:i).ne.esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//char(  9)         ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         lineout=lineout//line(i:i)
      endif
      if(i.ge.ilen)exit EXP
   enddo EXP

end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine notabs(INSTR,OUTSTR,ILEN)

character(len=*),parameter::ident_31="&
&@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               write(*,*)"*notabs* output string overflow"
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lenset(line,length) result(strout)

character(len=*),parameter::ident_36="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2r(chars,valu,ierr)

character(len=*),parameter::ident_40="@(#)M_strings::a2r(3fp): subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr.eq.0)then
      if(valu8.le.huge(valu))then
         valu=real(valu8)
      else
         write(*,*)'*a2r*','- value too large',valu8,'>',huge(valu)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

character(len=*),parameter::ident_41="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         write(*,*)'sc','*a2i*','- value too large',valu8,'>',huge(valu)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

character(len=*),parameter::ident_42="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero.  if no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         write(*,*)'sc','*a2d* - cannot produce number from string ['//trim(chars)//']'
         if(msg.ne.'')then
            write(*,*)'*a2d* - ['//trim(msg)//']'
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

character(len=*),parameter::ident_43="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
subroutine string_to_values(line,iread,values,inums,delims,ierr)
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_54="@(#)M_strings::string_to_values(3f): reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,ilen,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local.eq.'')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      ilen=0                                        ! ilen will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)).eq.0)then   ! found a non-delimiter
            ilen=i20
            exit
         endif
      enddo
      if(ilen.eq.0)then                             ! command was totally composed of delimiters
         write(*,*)'*string_to_values* blank line passed as a list of numbers'
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     ilen is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol.gt.ilen) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)).eq.0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,ilen                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)).ne.0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend.eq.0)iend=ilen+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier.eq.0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function s2vs(string,delim) result(darray)

character(len=*),parameter::ident_55="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

character(len=*),parameter::ident_45="@(#)M_strings::d2s(3fp): private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

character(len=*),parameter::ident_46="@(#)M_strings::r2s(3fp): private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

character(len=*),parameter::ident_47="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

character(len=*),parameter::ident_48="@(#)M_strings::l2s(3fp): private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         write(*,*)'*value_to_string* UNKNOWN TYPE'
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function decodebase(string,basein,out_baseten)
implicit none

character(len=*),parameter::ident_72="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine trimzeros(string)

character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine substitute(targetline,old,new,ierr,start,end)

character(len=*),parameter::ident_11="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      write(*,*)'*substitute* new line will be too long'
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
