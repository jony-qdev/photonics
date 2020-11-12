module math_utils

    ! Module with math functions

    implicit none 

    private 

    public :: coprimes, get_measurable_angle

    contains 

        ! Subroutine to calculate if two integer numbers are coprimes
        !
        ! Inputs 
        !   --- n1: integer, first number
        !   --- n2: integer, second number
        !   
        ! Returns 
        !   --- result: logical, if values is true, the numbers are coprimes else no

        subroutine coprimes(n1, n2, result)

            ! inputs
            integer, intent(in) :: n1, n2 
            logical, intent(inout) :: result 

            ! variables - to use
            integer :: i, max 

            ! get max 
            if (n1 > n2) then 
                max = n1 
            else 
                max = n2 
            end if 

            ! verify 
            do i = max, 2, -1

                ! if the condition is true, the numbers are not coprimes
                if (mod(n1, i) == 0 .and. mod(n2, i) == 0) then 
                    result = .false.
                    return
                end if 

            end do 

            ! if the numbers are coprimes
            result = .true. 

        end subroutine coprimes

        ! Function to calculate measurable angle to bravais moire
        !
        ! Inputs 
        !   --- r: integer, r of bravais moire 
        !   --- s: integer, s of bravais moire 
        !   
        ! Returns
        !   --- angle: real, measurable angle

        function get_measurable_angle(r, s) result(angle)

            ! inputs 
            integer, intent(in) :: r, s 

            ! outputs
            real :: angle

            angle = acos(2.0 * r * s / (r ** 2.0 + s ** 2.0))

        end function get_measurable_angle


end module math_utils