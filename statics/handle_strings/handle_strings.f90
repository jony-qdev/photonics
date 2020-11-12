module handle_strings

    ! Module to handle strings functions

    implicit none 

    private 

    public :: strip_spaces

    contains 

        ! Subroutine to delete the whitespaces in a string
        !
        ! Inputs: 
        !   --- string: string, string to remove whitespaces
        !
        ! Returns:
        !   --| same string but without whitespaces

        subroutine strip_spaces(string)
            
            ! inputs
            character(len=*) :: string

            ! variables to use
            integer :: string_len 
            integer :: last, current
        
            ! initialize variables
            string_len = len (string)
            last = 1
            current = 1
        
            ! remove whitespaces
            do while (current < string_len)
                if (string(last:last) == ' ') then
                    current = current + 1
                    string(last : last) = string(current : current)
                    string(current : current) = ' '
                else
                    last = last + 1
                    if (current < last) current = last
                endif
            end do
        
        end subroutine strip_spaces

end module handle_strings 