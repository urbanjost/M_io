             program demo_fileopen
             use M_io, only : fileopen, fileclose, print_inquire
             integer :: lun
             lun=fileopen('fred.txt')
             call print_inquire(lun)
             end program demo_fileopen
