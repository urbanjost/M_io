program runtest
use M_msg
use M_verify
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_io()
end program runtest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_io()
use M_io, only : dirname, get_tmp, notopen, print_inquire, rd, getline, read_line, read_table, slurp, splitpath, uniq
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   unit_check_level=0

   call test_dirname()
   call test_get_tmp()
   call test_print_inquire()
   call test_rd()
   call test_getline()
   call test_read_line()
   call test_read_table()
   call test_slurp()
   call test_splitpath()
   call test_uniq()
   call test_notopen()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dirname()

call unit_check_start('dirname',msg='')
call unit_check('dirname',  dirname('/usr/bin/') .eq. '/usr', '/usr/bin ==>',dirname('/usr/bin'))
call unit_check('dirname',  dirname('dir1/str/') .eq. 'dir1', 'dir1/str ==>',dirname('dir1/str/'))
call unit_check('dirname',  dirname('stdio.h')   .eq. '.',    '/stdio.h ==>',dirname('stdio.h'))
call unit_check_done('dirname',msg='')
end subroutine test_dirname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_tmp()

   call unit_check_start('get_tmp',msg='')
   !!call unit_check('get_tmp', 0.eq.0, 'checking',100)
   call unit_check_done('get_tmp',msg='')
end subroutine test_get_tmp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notopen()

integer :: i, ierr, ierr2

   call unit_check_start('notopen',msg='')
   call unit_check_msg('notopen','check for preassigned files from unit 0 to unit 1000')
   call unit_check_msg('notopen','assume 5 and 6 always return -1')

   do i=0,1000
      if(notopen(i,i,ierr) .ne. i)then
         call unit_check_msg('notopen','INUSE:',i,ierr, notopen(i,i,ierr2) )
      endif
   enddo
   call unit_check('notopen', notopen(5,6,ierr)           .eq. -1 ,'preassigned')

   do i=10,30,1
     open(unit=i,status="scratch")
   enddo

   close(25)
   close(28)
   call unit_check('notopen', notopen(10,30)           .eq. 25 )
   call unit_check('notopen', notopen()                .eq. 25 )
   call unit_check('notopen', notopen(start=12,end=30) .eq. 25 )
   call unit_check('notopen', notopen(26)              .eq. 28 )
   call unit_check('notopen', notopen(26,99)           .eq. 28 )

   call unit_check_done('notopen',msg='')

end subroutine test_notopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_inquire()

   call unit_check_start('print_inquire',msg='')
   !!call unit_check('print_inquire', 0.eq.0, 'checking',100)
   call unit_check_done('print_inquire',msg='')
end subroutine test_print_inquire
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rd()

   call unit_check_start('rd',msg='')
   !!call unit_check('rd_character', 0.eq.0, 'checking',100)
   call unit_check_done('rd',msg='')
end subroutine test_rd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getline()

   call unit_check_start('getline',msg='')
   !!call unit_check('getline', 0.eq.0, 'checking',100)
   call unit_check_done('getline',msg='')
end subroutine test_getline
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_line()

   call unit_check_start('read_line',msg='')
   !!call unit_check('read_line', 0.eq.0, 'checking',100)
   call unit_check_done('read_line',msg='')
end subroutine test_read_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_table()

   call unit_check_start('read_table',msg='')
   !!call unit_check('read_table', 0.eq.0, 'checking',100)
   call unit_check_done('read_table',msg='')
end subroutine test_read_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_slurp()

   call unit_check_start('slurp',msg='')
   !!call unit_check('slurp', 0.eq.0, 'checking',100)
   call unit_check_done('slurp',msg='')
end subroutine test_slurp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splitpath()

integer,parameter      :: maxlen=4096
character(len=maxlen)  :: dir
character(len=maxlen)  :: name
character(len=maxlen)  :: basename
character(len=maxlen)  :: ext
   call unit_check_start('splitpath',msg='')
   call splitpath('/usr/local/bin/test.exe', dir, name, basename, ext)
   call unit_check('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)
   call unit_check('splitpath', name=='test.exe', 'name','test.exe',name)
   call unit_check('splitpath', basename=='test', 'basename','test',basename)
   call unit_check('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', dir=dir)
   call unit_check('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)

   call splitpath('/usr/local/bin/test.exe', name=name)
   call unit_check('splitpath', name=='test.exe', 'name','test.exe',name)

   call splitpath('/usr/local/bin/test.exe', ext=ext)
   call unit_check('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', basename=basename)
   call unit_check('splitpath', basename=='test', 'basename','test',basename)

   call unit_check_done('splitpath',msg='')

end subroutine test_splitpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniq()

   call unit_check_start('uniq',msg='')
   !!call unit_check('uniq', 0.eq.0, 'checking',100)
   call unit_check_done('uniq',msg='')
end subroutine test_uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end subroutine test_suite_M_io
