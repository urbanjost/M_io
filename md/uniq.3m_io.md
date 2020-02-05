<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c49">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>uniq(3f)</b> - [M_io] append a number to the end of filename to make a unique name if name exists <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="8">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
<b>Usage</b>
<br />      character(len=:),allocatable function uniq(name,istart,verbose,create)
      character(len=*),intent(in) :: name
      integer,intent(in),optional :: istart
      logical,intent(in),optional :: verbose
      logical,intent(in),optional :: create
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Given a filename test if it is in use or exists. If it is, or if it ends in a period add a number to the end of the name and test if the new name
        exists. If necessary, increment the number and try again up to the value 9999999. By default an empty file is created if an unused name is found.
        <table cellpadding="3">
          <!-- tsb: Given a filename test if it is in use or exists. If it is, or if it
 -->
          <tr valign="top">
            <td width="3%">o</td>
            <td>relatively non-generic;</td>
          </tr>
          <tr valign="top">
            <td width="3%">o</td>
            <td>does not try to detect io errors</td>
          </tr>
        </table>
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c50" colspan="2">name</td>
          </tr>
          <tr valign="top">
            <td width="3%"></td>
            <td>base input name used to create output filename If name ends in "." a numeric suffix is always added.</td>
          </tr>
          <tr valign="top">
            <td class="c50" colspan="2">istart</td>
          </tr>
          <tr valign="top">
            <td width="3%"></td>
            <td>number to start with as a suffix. Default is 1. Must be a positive integer less than 9999999.</td>
          </tr>
          <tr valign="top">
            <td class="c50" colspan="2">verbose</td>
          </tr>
          <tr valign="top">
            <td width="3%"></td>
            <td>writes extra messages to stdout. Defaults to .false.</td>
          </tr>
          <tr valign="top">
            <td class="c50" colspan="2">create</td>
          </tr>
          <tr valign="top">
            <td width="3%"></td>
            <td>create file if new name is successfully found. Defaults to .true. .</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">RETURNS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c50" colspan="2">uniq</td>
          </tr>
          <tr valign="top">
            <td width="3%"></td>
            <td>A unique filename that is the same as the NAME input parameter except with a number appended at the end if needed. If could not find a
            unique name a blank is returned.</td>
          </tr>
        </table>
      </blockquote><a name="5"></a>
      <h3><a name="5">EXAMPLE</a></h3>
      <blockquote>
        Sample program
        <pre>
      program demo_uniq
      use M_io, only : uniq
      implicit none
      character(len=4096) :: myname
      integer             :: i
         myname=uniq('does_not_exist')
         open(unit=10,file='does_exist')
         write(*,*)'name stays the same ',trim(myname)
         myname=uniq('does_exist')
         write(*,*)'name has suffix added ',trim(myname)
         do i=1,10
            myname=uniq('does_exist')
            write(*,*) 'FILENAME:',trim(myname)
            open(unit=20+i,file=myname)
         enddo
      end program demo_uniq
<br />
</pre>Expected output
        <pre>
    name stays the same does_not_exist
    name has suffix added does_exist0001
    FILENAME:does_exist0002
    FILENAME:does_exist0003
    FILENAME:does_exist0004
    FILENAME:does_exist0005
    FILENAME:does_exist0006
    FILENAME:does_exist0007
    FILENAME:does_exist0008
    FILENAME:does_exist0009
    FILENAME:does_exist0010
    FILENAME:does_exist0011
<br />
</pre>
      <br />
      <div class="c49"><img src="images/uniq.3m_io.gif" /></div>
    </div>
  </div>
</body>
