           program demo_read_line
           use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
           use,intrinsic :: iso_fortran_env, only : stderr => error_unit
           use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
           use M_io, only : read_line
           implicit none
           character (len =: ), allocatable :: line
           integer  ::  ios, icount=0
              INFINITE: do while (isave (read_line (line), ios) == 0)
                 write (*, '(*(g0))') icount,' [',line,']'
              enddo INFINITE
              if ( .not.is_iostat_end(ios) ) then
                 write (stderr, '(*(g0))') 'error: line ',icount,'==>',trim (line)
              endif
           contains
              integer function isave (iin, iout)
              integer, intent (in) :: iin
              integer, intent (out) :: iout
                 iout = iin
                 isave = iin
                 icount=icount+1
              end function isave
           end program demo_read_line
