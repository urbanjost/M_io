<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c45">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>swallow(3f)</b> - [M_io] read a file into a character array line by line <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="7">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
subroutine <b>swallow</b>(filename,pageout)
<br />   character(len=*),intent(in) :: filename
     or
   integer,intent(in)          :: io
<br />   character(len=1),allocatable,intent(out) :: pageout(:)
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Read an entire file into memory as a character array, one character variable per line.
        <p>NOTE:</p>
        <p>Never casually read an entire file into memory if you can process it per line or in smaller units; as large files can consume unreasonable
        amounts of memory.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c46" colspan="2">filename</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>filename to read into memory, or LUN (Fortran Logical Unit Number). If filename is a LUN, file must be opened with</td>
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
            <td class="c46" colspan="2">pageout</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>array of characters to hold file</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">EXAMPLES</a></h3>
      <blockquote>
        Sample program
        <pre>
   program demo_swallow
   use M_io,      only : swallow
   use M_strings, only : notabs
   implicit none
   character(len=4096)          :: FILENAME   ! file to read
   character(len=:),allocatable :: pageout(:) ! array to hold file in memory
   integer                      :: longest, lines, i, ilen
   character(len=:),allocatable :: line
      ! get a filename
      call get_command_argument(1, FILENAME)
      ! allocate character array and copy file into it
      call swallow(FILENAME,pageout)
      if(.not.allocated(pageout))then
         write(*,*)'*demo_swallow* failed to load file '//FILENAME
      else
         ! write file from last line to first line
         longest=len(pageout)
         lines=size(pageout)
         allocate(character(len=longest)::line)
         write(*,*)'number of lines is ',lines
         write(*,*)'and length of lines is ',longest
         write(*,'(a)')repeat('%',longest+2)
         do i=lines,1,-1
            call notabs(pageout(i),line,ilen)
            write(*,'("%",a,"%")')line
         enddo
         write(*,'(a)')repeat('%',longest+2)
         deallocate(pageout)  ! release memory
      endif
   end program demo_swallow
<br />
</pre>Given
        <pre>
   first line
   second line
   third line
<br />
</pre>Expected output
        <pre>
    number of lines is 3
    and length of lines is 11
</pre>%%%%%%%%%%%%% %third line % %second line% %first line % %%%%%%%%%%%%%
      <br />
      <div class="c45"><img src="images/swallow.3m_io.gif" /></div>
    </div>
  </div>
</body>
