program echo_number
    implicit none
    integer :: arg_count
    character(len=100) :: arg1
    integer :: valid

    ! How many arguments did the user pass?
    arg_count = command_argument_count()
    if (arg_count < 1) then
        print *, "Usage: ./program <number>"
        stop
    end if

    ! Fetch the 1th (0-based) item of the given command line
    ! and populate arg1 with it
    call get_command_argument(1, arg1)

    ! Convert string -> integer
    read(arg1, *, iostat=valid)
    if (valid /= 0) then
        print *, "Invalid number:", trim(arg1)
        stop
    end if

    ! Print the message
    print *, "You said ", trim(arg1), "!"
end program echo_number
