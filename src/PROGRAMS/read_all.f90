          program demo_read_all
          use M_io, only : read_all
          implicit none
          character(len=:),allocatable :: line
             INFINITE: do while (read_all(line)==0)
                write(*,'(a)')'['//line//']'
             enddo INFINITE
          end program demo_read_all
