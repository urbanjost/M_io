<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c41">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>splitpath(3f)</b> - [M_io] split a Unix pathname into components
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
<b>splitpath</b>(path,dir,name,basename,ext)
<br />   integer,parameter :: maxlen=4096
   character(len=maxlen),intent(in)  :: path
   character(len=maxlen),intent(out),optional :: dir
   character(len=maxlen),intent(out),optional :: name
   character(len=maxlen),intent(out),optional :: basename
   character(len=maxlen),intent(out),optional :: ext
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        <b>splitpath</b>(3f) splits given pathname assuming a forward slash separates filename components and that the right-most period in the last leaf of
        the pathname is considered the beginning of an extension. If an extension is found it is left present in NAME but removed from BASENAME.
        <p>This routine does not check the system for the existence or type of the filename components; it merely parses a string.</p>
        <p>Assumes leaf separator is a slash ('/') and that filename does not contain trailing spaces.</p>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c42" width="6%" nowrap="nowrap">path</td>
            <td valign="bottom">
              Path to be broken into components. It is assumed
              <table width="100%" cellpadding="3">
                <!-- tsb: Path to be broken into components. It is assumed
 -->
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>Forward slashes (/) separate pathname components.</td>
                </tr>
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>the name '.' means "current directory"</td>
                </tr>
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>the name '..' means "up one directory"</td>
                </tr>
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>a pathname ending in a slash is a directory name</td>
                </tr>
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>a slash starting the pathname represents the root directory.</td>
                </tr>
                <tr valign="top">
                  <td width="3%">o</td>
                  <td>trailing spaces are insignificant.</td>
                </tr>
              </table><!-- .PP -->
            </td>
          </tr>
        </table>Using these rules helps to reduce incorrect parsing, but the routine is only intended for simple parsing of names of the form
        "[dir/]name[.extension].
      </blockquote><a name="4"></a>
      <h3><a name="4">RESULTS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c42" width="6%" nowrap="nowrap">dir</td>
            <td valign="bottom">Path of directories, including the trailing slash.</td>
          </tr>
          <tr valign="top">
            <td class="c42" width="6%" nowrap="nowrap">name</td>
            <td valign="bottom">Name of file leaf or, if no file is specified in path, name of the lowest directory.</td>
          </tr>
          <tr valign="top">
            <td class="c42" colspan="2">basename</td>
          </tr>
          <tr valign="top">
            <td width="6%"></td>
            <td>NAME with any extension removed</td>
          </tr>
          <tr valign="top">
            <td class="c42" width="6%" nowrap="nowrap">ext</td>
            <td valign="bottom">File name extension, if any, including the leading period (.).</td>
          </tr>
        </table>The path parameter can be a complete or partial file specification. The special name "." is assumed to mean the current directory, and the
        special name ".." is assumed to mean one directory above the current directory.
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        program demo_splitpath
        <pre>
   use m_io, only : splitpath
   implicit none
   integer,parameter :: maxlen=4096
   character(len=maxlen),parameter   :: file(*)=[&amp;
      &amp; 'dirs/name.ext  ', &amp;
      &amp; 'xx/IO/zz/NN.FF ', &amp;
      &amp; 'xx/IO/zz/NN    ', &amp;
      &amp; '/xx/IO/zz/NN   ', &amp;
      &amp; '/xx/IO/zz/     ', &amp;
      &amp; '/xx/IO/zz.A/   ', &amp;
      &amp; '/xx/IO/zz/.    ', &amp;
      &amp; '               ', &amp;
      &amp; './             ', &amp;
      &amp; '/              ', &amp;
      &amp; '/..            ', &amp;
      &amp; './..           ', &amp;
      &amp; 'name.          ', &amp;
      &amp; '.name          ', &amp;
      &amp; '.name.         ', &amp;
      &amp; '.              ', &amp;
      &amp; '..             ', &amp;
      &amp; '...            ']
<br />   character(len=maxlen)  :: dir
   character(len=maxlen)  :: name
   character(len=maxlen)  :: basename
   character(len=maxlen)  :: ext
   integer                :: i
   integer                :: longest
   longest=maxval(len_trim(file)) ! find longest filename
<br />   do i=1,size(file)
      call splitpath(file(i), dir, name, basename, ext)
      write(*,'(*("| ",a:))')  &amp;
      &amp; file(i)(:longest),     &amp;
      &amp; dir(:longest),         &amp;
      &amp; name(:longest),        &amp;
      &amp; basename(:longest),    &amp;
      &amp; ext(:longest)
   enddo
</pre>end program demo_splitpath
        <p>Output</p>
        <pre>
   | dirs/name.ext | dirs          | name.ext      | name          | .ext
   | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
   | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
   | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
   | /xx/IO/zz/    | /xx/IO/zz     |               |               |
   | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
   | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
   |               | .             |               |               |
   | ./            | .             |               |               |
   | /             | /             |               |               |
   | /..           | /             |               |               |
   | ./..          | ./..          |               |               |
   | name.         |               | name.         | name          | .
   | .name         |               | .name         | .name         |
   | .name.        |               | .name.        | .name         | .
   | .             | .             |               |               |
   | ..            |               |               |               |
   | ...           |               | ...           | ..            | .
<br />
</pre>
      <br />
      <div class="c41"><img src="images/splitpath.3m_io.gif" /></div>
    </div>
  </div>
</body>
