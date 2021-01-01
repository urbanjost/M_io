           program demo_which
           use M_io, only : which
           implicit none
              write(*,*)'ls is ',which('ls')
              write(*,*)'dir is ',which('dir')
           end program demo_which
