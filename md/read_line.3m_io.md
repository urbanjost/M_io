<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c30">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>read_line(3f)</b> - [M_io] read a line from specified LUN into allocatable string up to line length limit cleaning up input line
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="6">SYNTAX</a></h3>
      <blockquote>
        function <b>read_line</b>(line,lun) <b>result</b>(ier)
        <pre>
   character(len=:),allocatable,intent(out) :: line
   integer,intent(in),optional              :: lun
   integer                                  :: ier
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        <p>Read a line of any length up to programming environment's maximum line length. Requires Fortran 2003+.</p>
        <p>It is primarily expected to be used when reading input which will then be parsed.</p>
        <table cellpadding="3">
          <!-- tsb: It is primarily expected to be used when reading input which will
 -->
          <tr valign="top">
            <td width="3%">o</td>
            <td>Append lines that end in a backslash with next line</td>
          </tr>
          <tr valign="top">
            <td width="3%">o</td>
            <td>Expand tabs</td>
          </tr>
          <tr valign="top">
            <td width="3%">o</td>
            <td>Replace unprintable characters with spaces</td>
          </tr>
          <tr valign="top">
            <td width="3%">o</td>
            <td>Remove trailing carriage return characters and white space</td>
          </tr>
        </table>The simple use of a loop that repeatedly re-allocates a character variable in addition to reading the input file one buffer at a time could
        (depending on the programming environment used) be inefficient, as it could reallocate and allocate memory used for the output string with each
        buffer read.
      </blockquote><a name="3"></a>
      <h3><a name="3">EXAMPLE</a></h3>
      <blockquote>
        Sample program:
        <pre>
   program simple_read_line
   use M_io, only : read_line
   implicit none
   character(len=:),allocatable :: line
      INFINITE: do while (read_line(line)==0)
         write(*,'(a)')'['//line//']'
      enddo INFINITE
   end program simple_read_line
<br />
</pre>Checking the error message and counting lines:
        <pre>
    program demo_read_line
    use,intrinsic :: iso_fortran_env, only : stdin  =&gt; input_unit
    use,intrinsic :: iso_fortran_env, only : stderr =&gt; error_unit
    use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
    use M_io, only : read_line
    implicit none
    character (len =: ), allocatable :: line
    integer  ::  ios, icount=0
       INFINITE: do while (isave (read_line (line), ios) == 0)
          write (*, '(*(g0))') icount,' [',line,']'
       enddo INFINITE
       if ( .not.is_iostat_end(ios) ) then
          write (stderr, '(*(g0))') 'error: line ',icount,'==&gt;',trim (line)
       endif
    contains
       integer function isave (iin, iout)
       integer, intent (in) :: iin
       integer, intent (out) :: iout
          iout = iin
          isave = iin
          icount=icount+1
       end function isave
    end program demo_read_line
<br />
</pre>
      <br />
      <div class="c30"><img src="images/read_line.3m_io.gif" /></div>
    </div>
  </div>
</body>
