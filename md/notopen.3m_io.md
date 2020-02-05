<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c18">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>notopen(3f)</b> - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use 
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
<b>Usage</b>
<br />      integer function notopen(start,end,err)
      integer,optional,intent(in)  :: start
      integer,optional,intent(in)  :: end
      integer,optional,intent(out) :: err
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        A free FORTRAN unit number is needed to OPEN a file. <b>NOTOPEN</b>() returns a FORTRAN unit number from START to END not currently associated with
        an I/O unit. START and END are expected to be positive integers where END .ge. START.
        <p>If <b>NOTOPEN</b>() returns <b>-1</b>, then no free FORTRAN unit could be found in the specified range.</p>
        <p>Otherwise, <b>NOTOPEN</b>() returns an integer representing a free FORTRAN logical unit number. Note that <b>NOTOPEN</b>() assumes the following
        unit numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module</p>
        <pre>
      ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
<br />
</pre>are special, and will never return those values.
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c19" width="6%" nowrap="nowrap">start</td>
            <td valign="bottom">optional logical unit number to start scan at, defaults to 10.</td>
          </tr>
          <tr valign="top">
            <td class="c19" width="6%" nowrap="nowrap">end</td>
            <td valign="bottom">optional logical unit number to stop scan at, defaults to 99.</td>
          </tr>
          <tr valign="top">
            <td class="c19" width="6%" nowrap="nowrap">err</td>
            <td valign="bottom">optional error flag returned. ERR will be non-zero if no errors. If not present and an error occurs the program will stop
            instead of returning.</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">NOTES</a></h3>
      <blockquote>
        <p>Why are the default START and END limits from 10 to 99? the Fortran 77 standard did not specify a specific limit on the upper range limit, but
        the LUN range of 1 to 99 was almost always supported in conventional programming environments. Additionally, units in the range 0-10 have often been
        the units used for pre-assigned files. Occasionally 100, 101 and 102 are reserved (for files such as standard input, standard output, standard
        error, ...). Therefore, the defaults for START and END were selected to be 10 and 99. And most programs do not need more than 90 files
        simultaneously open, so the defaults work well in practice with many versions/vintages of Fortran.</p>
        <p>Note that an environment may impose a limit on the number of simultaneously open files (which some compilers work around).</p>
        <p>Beginning with f2008, you can probably use <b>OPEN</b>(NEWUNIT=...) instead of an open unit locator.</p>
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        <p>Sample program:</p>
        <pre>
    program demo_notopen ! test the NOTOPEN(3f) function
    use m_io, only: notopen
    implicit none
    integer :: ii, ierr, igot
<br />    write(*,*)'check for preassigned files from unit 0 to unit 1000'
    write(*,*)'(5 and 6 always return -1)'
<br />    do ii=0,1000
       if(notopen(ii,ii,ierr) .ne. ii)then
          write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
       endif
    enddo
<br />    ! open all files from UNIT=10 to UNIT=30 so have used units
    do ii=10,30,1
      open(unit=ii,status="scratch")
    enddo
    ! close UNIT=25
    close(25)
<br />    ! find open file in range 10 to 30
    write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
<br />    close(18)
    do ii=10,32
      igot=notopen(ii,ii,ierr)
      write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
    enddo
<br />    end program demo_notopen
<br />
</pre>Expected <b>output</b>(can vary with each programming environment):
        <pre>
      check for preassigned files from unit 0 to unit 1000
      (5 and 6 always return -1)
      INUSE:    0    -1
      INUSE:    5    -1
      INUSE:    6    -1
      Should get 25 for this .. 25
      For  unit  10  I  got  -1  with  ERR=  -1
      For  unit  11  I  got  -1  with  ERR=  -1
      For  unit  12  I  got  -1  with  ERR=  -1
      For  unit  13  I  got  -1  with  ERR=  -1
      For  unit  14  I  got  -1  with  ERR=  -1
      For  unit  15  I  got  -1  with  ERR=  -1
      For  unit  16  I  got  -1  with  ERR=  -1
      For  unit  17  I  got  -1  with  ERR=  -1
      For  unit  18  I  got  18  with  ERR=   0
      For  unit  19  I  got  -1  with  ERR=  -1
      For  unit  20  I  got  -1  with  ERR=  -1
      For  unit  21  I  got  -1  with  ERR=  -1
      For  unit  22  I  got  -1  with  ERR=  -1
      For  unit  23  I  got  -1  with  ERR=  -1
      For  unit  24  I  got  -1  with  ERR=  -1
      For  unit  25  I  got  25  with  ERR=   0
      For  unit  26  I  got  -1  with  ERR=  -1
      For  unit  27  I  got  -1  with  ERR=  -1
      For  unit  28  I  got  -1  with  ERR=  -1
      For  unit  29  I  got  -1  with  ERR=  -1
      For  unit  30  I  got  -1  with  ERR=  -1
      For  unit  31  I  got  31  with  ERR=   0
      For  unit  32  I  got  32  with  ERR=   0
<br />
</pre>
      <br />
      <div class="c18"><img src="images/notopen.3m_io.gif" /></div>
    </div>
  </div>
</body>
