           program demo_fileclose
           use M_io, only : fileclose, fileopen
           implicit none
           integer :: lun
           integer :: ios
              lun=fileopen('<input.txt')
              ios=fileclose(lun)
           end program demo_fileclose
