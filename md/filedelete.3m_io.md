<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c7">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>filedelete(3f)</b> - [M_io] A simple close of an open file with STATUS='DELETE'
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
function <b>filedelete</b>(<i>lun</i>) <b>result</b>(<i>ios</i>)
<br />    integer,intent(in)    :: lun
    integer               :: ios
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        A convenience command for deleting an <b>OPEN</b>(3f) file that leaves an error message in the current journal file if active.
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTION</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c8" width="6%" nowrap="nowrap">LUN</td>
            <td valign="bottom">unit number of open file to delete</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c8" width="6%" nowrap="nowrap">IOS</td>
            <td valign="bottom">status returned by <b>CLOSE</b>().</td>
          </tr>
        </table>
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        Sample program:
        <pre>
    program demo_filedelete
    use M_io, only : filedelete, fileopen
    implicit none
    integer :: lun
    integer :: ios
       lun=fileopen('&lt;input.txt')
       ios=filedelete(lun)
    end program demo_filedelete
</pre>
      <br />
      <div class="c7"><img src="images/filedelete.3m_io.gif" /></div>
    </div>
  </div>
</body>
