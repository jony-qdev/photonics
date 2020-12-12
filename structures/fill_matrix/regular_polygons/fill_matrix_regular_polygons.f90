module fill_matrix_regular_polygons

    ! Module to fil matrix with regular polygons given the centers

    use :: statics
    use :: global_elements 

    implicit none 

    private 

    public :: get_matrix_structure_regular_polygons 

    contains 

        ! Subroutine to fill matrix with regular_polygons given the centers
        ! Inputs
        !   --- centers_structure: array, centers with attributes
        !   --- matrix_structure: matrix, to fill
        ! 
        ! Results 
        !   --- matrix_structure: matrix, filled

        subroutine get_matrix_structure_regular_polygons(centers_structure, matrix_structure)

            ! inputs 
            real, dimension(:, :), allocatable, intent(in) :: centers_structure

            ! outputs
            real, dimension(nx, ny), intent(inout) :: matrix_structure

            ! to use 
            integer :: i, j, k, l, nconditions, idx, nvalid_points
            real :: radio, m, init_value, eq_value
            real, dimension(2, 2) :: matrix_rot
            real, dimension(:, :), allocatable :: matrix_vertices

            do l = 1, size(centers_structure, 1)

                ! verify sides
                if (int(centers_structure(l, 2)) < 3) then 
                    
                    if (rank == 0) then 
                        call error_message('Bad centers, verify')
                    else 
                        call exit(0)
                    end if 

                end if 

                ! allocate
                allocate(matrix_vertices(int(centers_structure(l, 2)), 2))

                ! generate vertices of polygon
                radio = centers_structure(l, 1) / (2.0 * sin(PI / centers_structure(l, 2)))
                matrix_rot = get_matrix_rotation_2d(2.0 * PI / centers_structure(l, 2))

                matrix_vertices(1, :) = [0.0, 0.0] 
                matrix_vertices(1, 2) = matrix_vertices(1, 2) + radio

                do i = 2, int(centers_structure(l, 2))
                    matrix_vertices(i, :) = matmul(matrix_vertices(i - 1, :), matrix_rot)
                end do 

                ! rotation
                matrix_rot = get_matrix_rotation_2d(centers_structure(l, 6))
                matrix_vertices = matmul(matrix_vertices, matrix_rot)

                ! count points in a center
                nvalid_points = 0

                do i = 1, nx 

                    do j = 1, ny 

                        ! count conditions of point in the polygon
                        nconditions = 0

                        do k = 1, int(centers_structure(l, 2))

                            idx = k + 1

                            if (idx == int(centers_structure(l, 2)) + 1) idx = 1

                            ! slope 
                            m = (matrix_vertices(idx, 2) - matrix_vertices(k, 2)) / &
                                (matrix_vertices(idx, 1) - matrix_vertices(k, 1))

                            ! to know the sign of zero point
                            init_value = -matrix_vertices(k, 2) -m * (-matrix_vertices(k, 1))

                            ! to know the sign of the current point
                            eq_value = (j * dy - centers_structure(l, 5) - matrix_vertices(k, 2)) -m * &
                                        (i * dx - centers_structure(l, 4) - matrix_vertices(k, 1))

                            ! verify if both have the same sign
                            init_value = init_value * eq_value 

                            ! verify if current point have the condition 
                            if (init_value >= 0.0) nconditions = nconditions + 1

                        end do 

                        ! if current point have the nsides conditions its a value of the structure
                        if (nconditions == centers_structure(l, 2)) then 
                            matrix_structure(i, j) = centers_structure(l, 3)
                            nvalid_points = nvalid_points + 1
                        end if

                    end do 
                end do  

                ! verify if nr is correct
                if (nvalid_points < centers_structure(l, 2)) then 

                    if (rank == 0) then 
                        call error_message('nr is too small or center outside the structure')
                    else 
                        call exit(0)
                    end if 

                end if

                ! deallocate
                deallocate(matrix_vertices)   

            end do 

        end subroutine get_matrix_structure_regular_polygons

end module fill_matrix_regular_polygons