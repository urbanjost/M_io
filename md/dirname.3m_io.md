<?
<body>
  <a name="top" id="top"></a>
  <h5><a href="download.html">[UP]</a></h5>
  <div id="Container">
    <div id="Content">
      <div class="c1">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>dirname(3f)</b> - [M_io] strip last component from filename <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="9">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
function <b>dirname</b>(<i>FILENAME</i>) result (<i>DIRECTORY</i>)
<br />     character(len=*),intent(in)  :: FILENAME
     character(len=:),allocatable :: DIRECTORY
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Output <i>FILENAME</i> with its last non-slash component and trailing slashes removed. If <i>FILENAME</i> contains no '/' character, output
        <p>Assumes leaf separator is a slash ('/') and that filename does not contain trailing spaces.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" colspan="2">FILENAME</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>pathname to remove the last leaf from</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" colspan="2">DIRECTORY</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>directory name for pathname</td>
          </tr>
        </table>
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLES</a></h3>
      <blockquote>
        Sample program:
        <pre>
   program demo_dirname
   use M_io, only : dirname
   implicit none
   character(len=:),allocatable :: filename
   integer                      :: filename_length
   integer                      :: i
   ! get pathname from command line arguments
   do i = 1 , command_argument_count()
      call get_command_argument (i , length=filename_length)
      allocate(character(len=filename_length) :: filename)
      call get_command_argument (i , value=filename)
      write(*,'(a)')dirname(filename)
      deallocate(filename)
   enddo
   end program demo_dirname
<br />
</pre>Sample program executions:
        <pre>
     demo_dirname /usr/bin/          -&gt; "/usr"
     demo_dirname dir1/str dir2/str  -&gt; "dir1" followed by "dir2"
     demo_dirname stdio.h            -&gt; "."
<br />
</pre>
      </blockquote><a name="6"></a>
      <h3><a name="6">SEE ALSO</a></h3>
      <blockquote>
        <b>dirname</b>(3c), <b>basename</b>(3c), <b>readlink</b>(3c), <b>realpath</b>(3c)
      </blockquote><a name="7"></a>
      <hr />
      <div class="c1"><img src="images/dirname.3m_io.gif" /></div>
    </div>
  </div>
</body>
