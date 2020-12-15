          program demo_getline
          use M_io, only : getline
          implicit none
          character(len=:),allocatable :: line
             INFINITE: do while (getline(line)==0)
                write(*,'(a)')'['//line//']'
             enddo INFINITE
          end program demo_getline
