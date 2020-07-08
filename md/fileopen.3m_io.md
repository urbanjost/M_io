<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c11">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>fileopen(3f)</b> - [M_io] A simple open of a sequential file
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
function <b>fileopen</b>(filename,mode,ios) <b>result</b>(<i>lun</i>)
<br />   character(len=*),intent(in)           :: filename
   character(len=*),intent(in),optional  :: mode
   integer,intent(out),optional          :: ios
   integer                               :: lun
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        <b>fileopen</b>(3f) is a convenience routine that allows you to open a file for sequential reading and writing as a text file in a form commonly
        found in C and interpreted languages such as shells. See the <b>OPEN</b>(3f) statement for more demanding I/O specifications (asynchronous, direct,
        unformatted, ... ). The documentation for the flexible and powerful <b>OPEN</b>(3f) statement can be a bit overwhelming; this routine cuts it down
        to the just the simple basic functions typically available in a scripting language such as bash, tcsh, sh, ...
        <p>Specify the file's name as the string FILENAME with a shell-like prefix specifying the access mode, or alternatively specify a plain FILENAME and
        the kind of access you need to the file with the string MODE.</p>
        <p>Three fundamental kinds of access are available: read, write, and append.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTION</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c12" colspan="2">FILENAME</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>The filename to open. If the beginning of the filename is</td>
          </tr>
          <tr>
            <td colspan="2">
              <pre>
            &lt;   open for read. File must exist
            &gt;   open for write. Will overwrite current file
            &gt;&gt;  open for append. Will append to current file
<br />            If no prefix exists to specify a file access mode, it
            will depend on the values of the MODE argument (meaning
            the default will be "readwrite").
<br />            A blank filename causes a unit number for a scratch file
            to be returned.
<br />
</pre>
            </td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">MODE</td>
            <td valign="bottom">[rwa][tb][+] An alternate way to specify the file access mode is to specify a MODE value. It should begin with one of the
            three characters "r", "w", or "a". It defaults to 'rw'. It is case-insensitive.</td>
          </tr>
        </table>
      </blockquote>
      <p><a name=""></a></p>
      <h4><a name="">READING PREFIX</a></h4>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">r,&lt;</td>
            <td valign="bottom">Open the file for reading; the operation will fail if the file does not exist, or if the host system does not permit you to
            read it.</td>
          </tr>
        </table>
      </blockquote><a name=""></a>
      <h4><a name="">WRITING PREFIXES</a></h4>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">w,&gt;</td>
            <td valign="bottom">Open a file for writing from the beginning of the file. If the file whose name you specified already existed, the call
            fails.</td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">o</td>
            <td valign="bottom">Open the file for writing from the beginning of the file: effectively, this always creates a new file. If the file whose
            name you specified already existed, its old contents are discarded.</td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">a,&lt;&lt;</td>
            <td valign="bottom">Initially open the file for appending data (ie. writing at the end of file).</td>
          </tr>
        </table>
      </blockquote><a name=""></a>
      <h4><a name="">SUFFIX</a></h4>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">b</td>
            <td valign="bottom">Append a "b" to any of the three modes above to specify that you are opening the file as a "binary file" (the default is to
            open the file as a sequential formatted text file. This switch changes to to an unformatted stream).</td>
          </tr>
          <tr>
            <td colspan="2">
              <pre>
                  open( ... access='stream';form='unformatted')
<br />
</pre>
            </td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">t</td>
            <td valign="bottom">Append a "t" to any of the three modes (rwa) to specify a formatted stream</td>
          </tr>
          <tr>
            <td colspan="2">
              <pre>
                  open( ... access='stream';form='formatted')
<br />
</pre>
            </td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">+</td>
            <td valign="bottom">Finally, you might need to both read and write from the same file. You can specify "rw" or you can append a '+' to any of
            the three primary modes ("rwa") to permit "readwrite" access</td>
          </tr>
          <tr valign="top">
            <td class="c12" width="6%" nowrap="nowrap">v</td>
            <td valign="bottom">Additionally, "v" selects verbose mode, which prints the <b>OPEN</b>(3f) options explicitly selected</td>
          </tr>
        </table>
      </blockquote><a name=""></a>
      <h4><a name="">NOTES</a></h4>
      <blockquote>
        <p>If you want to append both 'b' and '+', you can do it in either order: for example, "rb+" means the same thing as "r+b" when used as a mode
        string.)</p>
      </blockquote>
      <table cellpadding="3">
        <!-- tsb: If you want to append both &#145;b&#146; and &#145;+&#146;, you can do it in
 -->
        <tr valign="top">
          <td class="c12" width="6%" nowrap="nowrap">IOS</td>
          <td valign="bottom">The error code returned by the <b>OPEN</b>(3f) statement ultimately executed by this function. If not present the program
          stops on an error.</td>
        </tr>
      </table><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        <b>FILEOPEN</b>(3f) returns a Fortran unit number which you can use for other file operations, unless the file you requested could not be opened; in
        that situation, the result is <b>-1</b> (a reserved value that cannot be returned as a NEWUNIT value on an <b>OPEN</b>(3f)) and IOS will be
        non-zero.
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        Common usage
      </blockquote><a name=""></a>
      <h4><a name="">READ</a></h4>
      <blockquote>
        R=<b>fileopen</b>('&lt;in.txt') or R=<b>fileopen</b>('in.txt','r')
      </blockquote><a name=""></a>
      <h4><a name="">WRITE</a></h4>
      <blockquote>
        W=<b>fileopen</b>('&gt;out.txt') or W=<b>fileopen</b>('out.txt','W')
      </blockquote><a name=""></a>
      <h4><a name="">READWRITE</a></h4>
      <blockquote>
        RW=<b>fileopen</b>('inout.txt')
      </blockquote><a name=""></a>
      <h4><a name="">APPEND</a></h4>
      <blockquote>
        A=<b>fileopen</b>('&gt;&gt;inout.txt') or A=<b>fileopen</b>('inout.txt','a')
      </blockquote>
      <p>Sample program</p>
      <pre>
      program demo_fileopen
      use M_io, only : fileopen, fileclose, print_inquire
      integer :: lun
      lun=fileopen('fred.txt')
      call print_inquire(lun)
      end program demo_fileopen
<br />
</pre><a name="6"></a>
      <br />
      <div class="c11"><img src="images/fileopen.3m_io.gif" /></div>
    </div>
  </div>
</body>
