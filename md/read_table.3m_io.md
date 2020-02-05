<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c33">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>read_table(3f)</b> - [M_io] read file containing a table of numeric values <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="7">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
subroutine <b>read_table</b>(filename,array,ierr)
<br />   character(len=*),intent(in)             :: filename
<br />   doubleprecision,allocatable,intent(out) :: array(:,:)
   ! or
   real           ,allocatable,intent(out) :: array(:,:)
<br />   integer,intent(out)                     :: ierr
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Read a table from a file that is assumed to be columns of space-delimited numbers, with each row containing the same number of values
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c34" colspan="2">filename</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>filename to read</td>
          </tr>
          <tr valign="top">
            <td class="c34" width="6%" nowrap="nowrap">array</td>
            <td valign="bottom">array to create</td>
          </tr>
          <tr valign="top">
            <td class="c34" width="6%" nowrap="nowrap">ierr</td>
            <td valign="bottom">zero if no error occurred</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">EXAMPLES</a></h3>
      <blockquote>
        Sample program, assuming the input file "inputfile" exists:
        <pre>
    program demo_read_table
    use M_io, only : read_table
    doubleprecision,allocatable :: array(:,:)
<br />    ! create test file
    open(file='inputfile',unit=10)
    write(10,'(a)') '1 10  45'
    write(10,'(a)') '10 10  45'
    write(10,'(a)') '  2 20  15'
    write(10,'(a)') ' 20.345 20  15'
    write(10,'(a)') '  3 30.111   0'
    write(10,'(a)') '30 30e3   0'
    write(10,'(a)') '  4 300.444e-1 -10'
    write(10,'(a)') '40 30.5555d0 -10'
    write(10,'(a)') '  4 300.444E-1 -10'
    write(10,'(a)') '40 30.5555D0 -10'
    close(unit=10)
<br />    ! read file as a table
    call read_table('inputfile',array,ierr)
<br />    ! print values
    write(*,*)'size=       ',size(array)
    write(*,*)'size(dim=1)=',size(array,dim=1)
    write(*,*)'size=(dim=2)',size(array,dim=2)
    do i=1,size(array,dim=1)
       write(*,*)array(i,:)
    enddo
<br />    ! remove sample file
    open(file='inputfile',unit=10)
    close(unit=10,status='delete')
<br />    end program demo_read_table
<br />
</pre>
      </blockquote>Results:
      <pre>
    size=          30
    size(dim=1)=   10
    size(dim=2)=    3
      1.0000000000000000        10.000000000000000        45.000000000000000
      10.000000000000000        10.000000000000000        45.000000000000000
      2.0000000000000000        20.000000000000000        15.000000000000000
      20.344999999999999        20.000000000000000        15.000000000000000
      3.0000000000000000        30.111000000000001        0.0000000000000000
      30.000000000000000        30000.000000000000        0.0000000000000000
      4.0000000000000000        30.044400000000000       -10.000000000000000
      40.000000000000000        30.555499999999999       -10.000000000000000
      4.0000000000000000        30.044400000000000       -10.000000000000000
      40.000000000000000        30.555499999999999       -10.000000000000000
<br />
</pre><a name="5"></a>
      <br />
      <div class="c33"><img src="images/read_table.3m_io.gif" /></div>
    </div>
  </div>
</body>
