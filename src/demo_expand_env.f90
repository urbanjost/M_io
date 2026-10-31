    program demo_expand_env
    use M_io, only : expand_env
    implicit none
    integer                       :: i
    character(len=:), allocatable :: expanded
    character(len=*), parameter   :: tests(*) = [ character(len=80) :: &
     "-------------- Testing Pure Fortran Word Expansion --------------",&
     " scratch space is $TMP for $USER with home directory  ${HOME}",    &
     " DATE is $DATE TIME is ${TIME} ZONE is $ZONE TIMESTAMP $TIMESTAMP",&
     " FILE is $FILE ARGS: 0: $0 1: $1 2: $2 3: $3",                     &
     " COMPILER is $COMPILER COMPILER_OPTIONS are ${COMPILER_OPTIONS}",  &
     " ${0} versus ${FILE}  $0 versus $FILE",                            &
     " All arguments is ${0} ${*}",                                      &
     " unknown $UNKNOWN incorrect ${ A B C} but syntax OK",              &
     " Bad $  ${  and stuff $UNKNOWN ${ A B C}",                         &
     ""]
    
       do i = 1,size(tests)
          expanded=expand_env(tests(i))
          print '(A)', "Input:  "//trim(tests(i))
          print '(A)', "Result: "//expanded
          print '(A)', ""
       enddo
    
    end program demo_expand_env
