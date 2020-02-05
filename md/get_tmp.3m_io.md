<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c15">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>get_tmp(3f)</b> - [M_io] Return the name of the scratch directory 
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="6">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
function <b>get_tmp</b>() <b>result</b>(<i>tname</i>)
<br />     character(len=:),allocatable :: tname
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        <p>Return the name of the scratch directory set by the most common environment variables used to designate a scratch directory. $TMPDIR is the
        canonical environment variable in Unix and POSIX[1] to use to specify a temporary directory for scratch space. If $TMPDIR is not set, $TEMP,
        $TEMPDIR, and $TMP are examined in that order. If nothing is set "/tmp/" is returned. The returned value always ends in "/". No test is made that
        the directory exists or is writable.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">EXAMPLE</a></h3>
      <blockquote>
        <p>Sample:</p>
        <pre>
    program demo_get_tmp
    use M_io, only : get_tmp, uniq
    implicit none
    character(len=:),allocatable :: answer
       answer=get_tmp()
       write(*,*)'result is ',answer
       answer=get_tmp()//uniq('_scratch',create=.false.)
       write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
    end program demo_get_tmp
<br />
</pre>Sample Results:
        <pre>
    result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
</pre>
      <br />
      <div class="c15"><img src="images/get_tmp.3m_io.gif" /></div>
    </div>
  </div>
</body>
