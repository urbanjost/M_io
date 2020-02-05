<?
<body>
  <a name="top" id="top"></a>
  <h5><a href="download.html">[UP]</a></h5>
  <div id="Container">
    <div id="Content">
      <div class="c37">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>SLURP(3f)</b> - [M_io] read a file into a character array <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="7">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
subroutine <b>slurp</b>(filename,text)
<br />   character(len=*),intent(in) :: filename
    or
   integer,intent(in)          :: filenumber
<br />   character(len=1),allocatable,intent(out) :: text(:)
   integer,intent(out),optional :: length
   integer,intent(out),optional :: lines
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Read an entire file as a stream into memory as an array of single characters, retaining line end terminators.
        <p>NOTE:</p>
        <p>Never casually read an entire file into memory if you can process it per line or in smaller units; as large files can consume unreasonable
        amounts of memory.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c38" colspan="2">filename</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>filename to read into memory or LUN (Fortran Logical Unit Number) If a LUN, file must be opened with</td>
          </tr>
          <tr>
            <td colspan="2">
              <pre>
                    form='unformatted',access='stream'
<br />
</pre>
            </td>
          </tr>
          <tr>
            <td width="6%"></td>
            <td>
              as in
              <pre>
                   open(unit=igetunit, file=filename,     &amp;
                   &amp; action="read", iomsg=message,        &amp;
                   &amp; form="unformatted", access="stream", &amp;
                   &amp; status='old',iostat=ios)
<br />
</pre>
            </td>
          </tr>
          <tr valign="top">
            <td class="c38" width="6%" nowrap="nowrap">text</td>
            <td valign="bottom">array of characters to hold file</td>
          </tr>
          <tr valign="top">
            <td class="c38" width="6%" nowrap="nowrap">length</td>
            <td valign="bottom">length of longest line <b>read</b>(Optional).</td>
          </tr>
          <tr valign="top">
            <td class="c38" width="6%" nowrap="nowrap">lines</td>
            <td valign="bottom">number of lines <b>read</b>(Optional).</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">EXAMPLES</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c38" colspan="2">Sample program, which</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>creates test input file "inputfile":</td>
          </tr>
        </table><!-- .nf -->
        <pre>
    program demo_slurp
    use M_io, only      : slurp
    implicit none
    character(len=1),allocatable :: text(:) ! array to hold file in memory
    character(len=*),parameter :: FILENAME='inputfile' ! file to read
<br />    ! create test file
    open(file=FILENAME,unit=10)
    write(10,'(a)') new_line('A')//'esrever lliw'
    write(10,'(a)') 'margorp elpmas eht taht'
    write(10,'(a)') 'elif elpmas a si sihT'
    close(unit=10)
<br />    call slurp(FILENAME,text) ! allocate character array and copy file into it
<br />    if(.not.allocated(text))then
       write(*,*)'*rever* failed to load file '//FILENAME
    else
       ! write file reversed to stdout
       write(*,'(*(a:))',advance='no')text(size(text):1:-1)
       deallocate(text)  ! release memory
    endif
<br />    end program demo_slurp
<br />
</pre>Expected output:
        <pre>
    &gt;This is a sample file
    &gt;that the sample program
    &gt;will reverse
</pre>
      <br />
      <div class="c37"><img src="images/slurp.3m_io.gif" /></div>
    </div>
  </div>
</body>
