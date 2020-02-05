<?
<body>
  <a name="top" id="top"></a>
  <h5><a href="download.html">[UP]</a></h5>
  <div id="Container">
    <div id="Content">
      <div class="c4">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>fileclose(3f)</b> - [M_io] A simple close of a sequential file <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
function <b>fileclose</b>(<i>lun</i>) <b>result</b>(<i>ios</i>)
<br />     integer,intent(in)       :: lun
     integer                  :: ios
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        A convenience command for closing a file that leaves an error message in the current journal file if active.
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTION</a></h3>
      <blockquote>
        LUN unit number to close
      </blockquote><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        IOS status value from CLOSE
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        Sample program:
        <pre>
    program demo_fileclose
    use M_io, only : fileclose, fileopen
    implicit none
    integer :: lun
    integer :: ios
       lun=fileopen('&lt;input.txt')
       ios=fileclose(lun)
    end program demo_fileclose
<br />
</pre>
      <div class="c4"><img src="images/fileclose.3m_io.gif" /></div>
    </div>
  </div>
</body>
