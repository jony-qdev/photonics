module fill_matrix_circles

    ! Module to fil matrix with circles given the centers

    use :: statics 
    use :: global_elements

    implicit none 

    private 

    public :: get_matrix_structure_circles

    contains 

        ! Subroutine to fill matrix with circles given the centers
        ! Inputs
        !   --- centers_structure: array, centers with attributes
        !   --- matrix_structure: matrix, to fill
        ! 
        ! Results 
        !   --- matrix_structure: matrix, filled

        subroutine get_matrix_structure_circles(centers_structure, matrix_structure)

            ! inputs 
            real, dimension(:, :), allocatable, intent(in) :: centers_structure

            ! outputs
            real, dimension(nx, ny), intent(inout) :: matrix_structure

            ! to use 
            integer :: i, j, k
            real :: nvalid_points

            ! fill matrix 
            do k = 1, size(centers_structure, 1)

                ! validate that centers is a circle
                if (int(centers_structure(k, 2)) /= 1) call error_message('Bad centers, verify')

                ! points in circle
                nvalid_points = 0

                do i = 1, nx 
                    do j = 1, ny 

                        if (sqrt((i * dx - centers_structure(k, 4)) ** 2 + &
                                 (j * dy - centers_structure(k, 5)) ** 2) <= centers_structure(k, 1)) then 

                            matrix_structure(i, j) = centers_structure(k, 3)
                            nvalid_points = nvalid_points + 1

                        end if 
                    end do 
                end do 
                
                ! validate points in circle
                if (nvalid_points < int(centers_structure(k, 2))) &
                    call error_message('nr is too small or center outside the structure')

            end do 

        end subroutine get_matrix_structure_circles

end module fill_matrix_circles