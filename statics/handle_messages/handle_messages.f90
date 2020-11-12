module handle_messages

    ! Module to handle messages in the program, outputs

    implicit none 

    private 

    ! public subroutines and functions
    public :: error_message, info_message, success_message, flat_message

    ! global variables
    integer :: status ! 0 error, 1 info, 2 success, 3 flat

    contains 

        ! Subroutine to generate a message of error and finish the program
        !
        ! Inputs:
        !   --- message: string, with message to show
        !   
        ! Returns:
        !   --| message in terminal and in photonics.log and finish of the program

        subroutine error_message(message)

            character(len=*), intent(in) :: message 

            write(*, *) ''
            write(200, *) ''

            write(*, *) 'Error: '//message
            write(200, *) 'Error: '//message

            ! exit program
            call exit(0)

        end subroutine error_message

        ! Subroutine to generate a message of info 
        ! Inputs:
        !   --- message: string, with message to show
        !   
        ! Returns:
        !   --| message in terminal and in photonics.log

        subroutine info_message(message)

            character(len=*), intent(in) :: message 

            ! verify status
            if (status /= 1) then 
                write(*, *) ''
                write(200, *) ''
            end if 

            write(*, *) 'Info: '//message
            write(200, *) 'Info: '//message

            ! update status
            status = 1

        end subroutine info_message

        ! Subroutine to generate a message of success 
        ! Inputs:
        !   --- message: string, with message to show
        !   
        ! Returns:
        !   --| message in terminal and in photonics.log

        subroutine success_message(message)

            character(len=*), intent(in) :: message 

            ! verify status 
            if (status /= 2) then 
                write(*, *) ''
                write(200, *) ''
            end if 

            write(*, *) 'Success: '//message
            write(200, *) 'Success: '//message

            ! update status 
            status = 2

        end subroutine success_message

        ! Subroutine to generate a flat message 
        ! Inputs:
        !   --- message: string, with message to show 
        !
        ! Returns:
        !   --| message in terminal and in photonics.log

        subroutine flat_message(message)

            character(len=*), intent(in) :: message 

            ! verify status 
            if (status /= 3) then 
                write(*, *) '' 
                write(200, *) '' 
            end if 

            write(*, *) message 
            write(200, *) message

            ! update status 
            status = 3

        end subroutine flat_message

end module 