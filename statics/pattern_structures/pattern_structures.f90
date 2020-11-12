module pattern_structures 

    ! Module to get patterns of structures

    use :: handle_messages

    implicit none 

    private 

    public :: get_pattern_thue_morse_fractal, get_pattern_2d_rudin_shapiro

    contains 

        ! Function to get pattern of thue morse fractal
        !
        ! Inputs: 
        !   --- n: integer, level of fractal
        !
        ! Returns:
        !   --- m: matrix, fill of zeros and ones with shape of the fractal

        recursive function get_pattern_thue_morse_fractal(n) result(m)

            ! inputs 
            integer, intent(in) :: n 

            ! outputs
            integer :: m(2**n, 2**n)

            if (n == 0) then 
                m = 1
            else if (n < 0) then 
                call error_message('n for thue morse fractal should be positive number')
            else

                ! get pattern
                m(1 : 2**(n - 1), 1 : 2**(n - 1)) = get_pattern_thue_morse_fractal(n - 1)
                m(2**(n - 1) + 1 : 2**n, 1 : 2**(n - 1)) = mod(m(1 : 2**(n - 1), 1 : 2**(n - 1)) + 1, 2)
                m(2**(n - 1) + 1 : 2**n, 2**(n - 1) + 1 : 2**n) = get_pattern_thue_morse_fractal(n - 1)
                m(1 : 2**(n - 1), 2**(n - 1) + 1 : 2**n) = mod(m(2**(n - 1) + 1 : 2**n, 2**(n - 1) + 1 : 2**n) + 1, 2)
            end if

        end function get_pattern_thue_morse_fractal

        ! Function to get pattern of 2d rudin-shapiro
        !
        ! Inputs: 
        !   --- n: integer, level of fractal
        !
        ! Returns:
        !   --- m: matrix, fill of zeros and ones with shape of the fractal

        function get_pattern_2d_rudin_shapiro(n) result(m)

            ! inputs
            integer, intent(in) :: n 

            ! outputs
            integer, dimension(:, :), allocatable :: m

            ! variables - to use
            integer, dimension(:, :), allocatable :: matrix_init, aux
            integer, dimension(4) :: aa = [0, 0, 0, 1], ab = [0, 0, 1, 0], ba = [1, 1, 0, 1], bb = [1, 1, 1, 0]
            integer :: i, j, k, count

            if (n < 1) call error_message("Verify n of Rudin-Shapiro")

            ! allocate
            allocate(matrix_init(2, 2))

            ! fill matrix init 
            matrix_init(1, 1) = 0
            matrix_init(1, 2) = 1
            matrix_init(2, 1) = 1
            matrix_init(2, 2) = 0

            if (n == 1) m = matrix_init
        
            ! loop in n 
            do k = 1, n - 1

                count = size(matrix_init, 1)

                ! allocate m 
                allocate(m(count * 2, count * 2))

                ! loop in rows
                do i = 1, count

                    ! loop in columns 
                    do j = 1, count, 2

                        ! check conditions
                        if (matrix_init(i, j) == 0 .and. matrix_init(i, j + 1) == 0) then 
                            m(i, j * 2 - 1 : j * 2 + 2) = aa 

                        else if (matrix_init(i, j) == 0 .and. matrix_init(i, j + 1) == 1) then 
                            m(i, j * 2 - 1 : j * 2 + 2) = ab 

                        else if (matrix_init(i, j) == 1 .and. matrix_init(i, j + 1) == 0) then 
                            m(i, j * 2 - 1 : j * 2 + 2) = ba

                        else if (matrix_init(i, j) == 1 .and. matrix_init(i, j + 1) == 1) then 
                            m(i, j * 2 - 1 : j * 2 + 2) = bb

                        end if 
                    end do
                end do 

                ! deallocate and reshape matrix init
                deallocate(matrix_init)
                allocate(matrix_init(count * 2, count * 2))
                
                matrix_init = m

                ! loop in columns
                do j = 1, count * 2

                    ! loop in rows
                    do i = 1, count, 2

                        ! check conditions
                        if (matrix_init(i, j) == 0 .and. matrix_init(i + 1, j) == 0) then 
                            m(i * 2 - 1 : i * 2 + 2, j) = aa

                        else if (matrix_init(i, j) == 0 .and. matrix_init(i + 1, j) == 1) then 
                            m(i * 2 - 1 : i * 2 + 2, j) = ab 
                        
                        else if (matrix_init(i, j) == 1 .and. matrix_init(i + 1, j) == 0) then 
                            m(i * 2 - 1 : i * 2 + 2, j) = ba

                        else if (matrix_init(i, j) == 1 .and. matrix_init(i + 1, j) == 1) then 
                            m(i * 2 - 1 : i * 2 + 2, j) = bb

                        end if 


                    end do 

                end do 

                ! rewrite matrix_init 
                matrix_init = m

                ! deallocate m 
                if (k /= n - 1) deallocate(m)

            end do

            ! change positions
            
            if (mod(n, 2) == 0) then 
                aux = m
                i = 1

                do j = size(m, 1), 1, -1 
                    
                    m(:, i) = aux(:, j)

                    i = i + 1

                end do 
            end if

            ! deallocate
            deallocate(matrix_init)

        end function get_pattern_2d_rudin_shapiro

end module pattern_structures 