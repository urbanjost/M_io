    program demo_get_arg
    use M_io, only : get_arg
    implicit none
    integer                       :: i
    character(len=:), allocatable :: expanded

       print '(A)', 'The program name is '//get_arg(0)

       do i = 1,command_argument_count()
          expanded=get_arg(i)
          print '(A)', expanded
       enddo
    
    end program demo_get_arg
