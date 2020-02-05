<?
<body>
  <a name="top" id="top"></a>
  <h5><a href="download.html">[UP]</a></h5>
  <div id="Container">
    <div id="Content">
      <div class="c26">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>read_all(3f)</b> - [M_io] read a line from specified LUN into allocatable string up to line length limit <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNTAX</a></h3>
      <blockquote>
        function <b>read_all</b>(line,lun) <b>result</b>(ier)
        <pre>
   character(len=:),allocatable,intent(out) :: line
   integer,intent(in),optional              :: lun
   integer,intent(out)                      :: ier
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        <p>Read a line of any length up to programming environment's maximum line length. Requires Fortran 2003+.</p>
        <p>It is primarily expected to be used when reading input which will then be parsed.</p>
        <p>The simple use of a loop that repeatedly re-allocates a character variable in addition to reading the input file one buffer at a time could
        (depending on the programming environment used) be inefficient, as it could reallocate and allocate memory used for the output string with each
        buffer read.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c27" width="6%" nowrap="nowrap">LINE</td>
            <td valign="bottom">line read</td>
          </tr>
          <tr valign="top">
            <td class="c27" width="6%" nowrap="nowrap">LUN</td>
            <td valign="bottom">optional LUN (Fortran logical I/O unit) number. Defaults to stdin.</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c27" width="6%" nowrap="nowrap">IER</td>
            <td valign="bottom">zero unless an error occurred. If not zero, LINE returns the I/O error message.</td>
          </tr>
        </table>
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        Sample program:
        <pre>
   program demo_read_all
   use M_io, only : read_all
   implicit none
   character(len=:),allocatable :: line
      INFINITE: do while (read_all(line)==0)
         write(*,'(a)')'['//line//']'
      enddo INFINITE
   end program demo_read_all
</pre>
      <br />
      <div class="c26"><img src="images/read_all.3m_io.gif" /></div>
    </div>
  </div>
</body>
