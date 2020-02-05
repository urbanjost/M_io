<?
<body>
  <a name="top" id="top"></a>
  <div id="Container">
    <div id="Content">
      <div class="c22">
      </div><a name="0"></a>
      <h3><a name="0">NAME</a></h3>
      <blockquote>
        <b>print_inquire(3f)</b> - [M_io] Do INQUIRE on file by name/number and print results <b>(LICENSE:PD)</b>
      </blockquote><a name="contents" id="contents"></a>
      <h3><a name="7">SYNOPSIS</a></h3>
      <blockquote>
        <pre>
Definition:
<br />   subroutine print_inquire(lun)
     or
   subroutine print_inquire(name)
   integer,intent(in),optional          :: lun
   character(len=*),intent(in),optional :: name
<br />
</pre>
      </blockquote><a name="2"></a>
      <h3><a name="2">DESCRIPTION</a></h3>
      <blockquote>
        Given either a Fortran file-unit-number or filename, call the <b>INQUIRE</b>(3f) intrinsic and print typical status information.
      </blockquote><a name="3"></a>
      <h3><a name="3">OPTIONS</a></h3>
      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c23" width="6%" nowrap="nowrap">lun</td>
            <td valign="bottom">if lun is not equal to <b>-1</b> then query by number and ignore filename even if present</td>
          </tr>
          <tr valign="top">
            <td class="c23" width="6%" nowrap="nowrap">name</td>
            <td valign="bottom">if lun = <b>-1</b> or is not present then query by this filename</td>
          </tr>
        </table>
      </blockquote><a name="4"></a>
      <h3><a name="4">EXAMPLE</a></h3>
      <blockquote>
        Sample program:
        <pre>
   program demo_print_inquire
   use M_io, only : print_inquire, fileopen
   implicit none
   character(len=4096)  :: filename
   character(len=20)    :: mode
   integer              :: ios
   character(len=256)   :: message
   integer              :: lun
      do
         write(*,'(a)',advance='no')'enter filename&gt;'
         read(*,'(a)',iostat=ios)filename
         if(ios.ne.0)exit
         write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]&gt;'
         read(*,'(a)',iostat=ios)mode
         if(ios.ne.0)exit
         lun=fileopen(filename,mode,ios)
         if(ios.eq.0)then
            write(*,*)'OPENED'
         else
            write(*,*)'ERROR: IOS=',ios
         endif
         if(lun.ne.-1)then
            call print_inquire(lun,'')
            close(lun,iostat=ios,iomsg=message)
            if(ios.ne.0)then
               write(*,'(a)')trim(message)
            endif
         endif
      enddo
   end program demo_print_inquire
<br />
</pre>
      <br />
      <div class="c22"><img src="images/print_inquire.3m_io.gif" /></div>
    </div>
  </div>
</body>
