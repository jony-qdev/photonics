module patterns 

    use :: global_elements 
    use :: statics
    use :: unit_cell
    use :: bravais_moire
    use :: from_files

    implicit none

    private

    public :: get_centers_structure_pattern

    contains 

        ! Function to get centers of pattern thue morse or rudin_shapiro
        ! Inputs 
        !
        ! Results
        !   --- centeres, array(:, 5), with shape_param, valid_points, permittivity, x, y

        function get_centers_structure_pattern(ff_opt) result(centers)

            ! input
            real, optional :: ff_opt

            ! output 
            real, dimension(:, :), allocatable :: centers

            ! to use 
            real, dimension(:, :), allocatable :: centers_main, centers_back
            real :: a, b
            integer, dimension(:, :), allocatable :: matrix_pattern
            integer :: status_bm, ncenters_main, ncenters_back, i, j, count_centers, init, fin
            character(len=:), allocatable :: structure_type, substructure_type, background_type
            logical :: inputs_right, has_background, is_found, complement

            ! initialization
            inputs_right = .true.
            status_bm = -1

            ! get structures types
            call json%get('structure.type', structure_type, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure.n', n, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure.substructure.type', substructure_type, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure.background.type', background_type, has_background)

            ! verify inputs json 
            if (.not. inputs_right) then 
                
                if (rank == 0) then 
                    call error_message('Verifya structure inputs')
                else 
                    call exit(0)
                end if 

            end if 

            ! verify n
            if (structure_type == 'rudin_shapiro' .and. n < 1) then 

                if (rank == 0) then 
                    call error_message('Verify n of rudin shapiro')
                else 
                    call exit(0)
                end if
 
            else if(structure_type == 'thue_morse' .and. n < 0) then 
                
                if (rank == 0) then 
                    call error_message('Verify n of thue morse')
                else 
                    call exit(0)
                end if

            end if 

            ! verify if bravais moire is here
            if (substructure_type == 'bravais_moire') then 

                ! get centers and re-scale a and b
                if (present(ff_opt)) then 
                    centers_main = get_centers_bravais_moire('substructure', ff_opt)
                else 
                    centers_main = get_centers_bravais_moire('substructure')
                end if 

                a = side_x  
                b = side_y

                side_x = side_x * 2 ** n
                side_y = side_y * 2 ** n

                status_bm = 0

            else if (background_type == 'bravais_moire') then 

                ! get centers and re-scale a and b
                if (present(ff_opt)) then 
                    centers_back = get_centers_bravais_moire('background', ff_opt)
                else 
                    centers_back = get_centers_bravais_moire('background')
                end if 

                a = side_x  
                b = side_y

                side_x = side_x * 2 ** n
                side_y = side_y * 2 ** n

                status_bm = 1

            else 

                call json%get('structure.a', a, is_found); if(.not. is_found) inputs_right = .false.
                call json%get('structure.b', b, is_found); if(.not. is_found) b = a

                ! re-scale a and b 
                side_x = a * 2 ** n 
                side_y = b * 2 ** n 

            end if 

            ! verify inputs json 
            if (.not. inputs_right) then 
                
                if (rank == 0) then 
                    call error_message('Verify structure inputs')
                else 
                    call exit(0)
                end if 

            end if

            ! define nx, ny, dx and dy 
            nx = ceiling(side_x / (lambda / nr))
            ny = ceiling(side_y / (lambda / nr))
            dx = side_x / nx 
            dy = side_y / ny

            ! get centers to main
            if (status_bm /= 0) then 

                select case(substructure_type)

                case ('circle_center', 'regular_polygon_center')

                    if (present(ff_opt)) then 
                        centers_main = get_center_unit_cell('substructure', a, b, ff_opt)
                    else 
                        centers_main = get_center_unit_cell('substructure', a, b)
                    end if 

                case ('from_files')

                    if (present(ff_opt)) then 
                        centers_main = get_centers_from_files('substructure', a, b, ff_opt)
                    else 
                        centers_main = get_centers_from_files('substructure', a, b)
                    end if 

                case default 

                    if (rank == 0) then 
                        call error_message('Substructure type does not exist')
                    else 
                        call exit(0)
                    end if

                end select 

            end if 

            ! get centers to back 
            if (status_bm /= 1 .and. has_background) then 

                select case(background_type)

                case ('circle_center', 'regular_polygon_center')

                    if (present(ff_opt)) then 
                        centers_back = get_center_unit_cell('background', a, b, ff_opt)
                    else 
                        centers_back = get_center_unit_cell('background', a, b)
                    end if 

                case ('from_files')

                    if (present(ff_opt)) then 
                        centers_back = get_centers_from_files('background', a, b, ff_opt)
                    else 
                        centers_back = get_centers_from_files('background', a, b)
                    end if 

                case default 

                    if (rank == 0) then 
                        call error_message('Background type does not exist')
                    else 
                        call exit(0)
                    end if

                end select 

            end if 

            ! get pattern
            select case (structure_type)
            case ('thue_morse')
                matrix_pattern = get_pattern_thue_morse_fractal(n)
            case ('rudin_shapiro')
                matrix_pattern = get_pattern_2d_rudin_shapiro(n)
            end select

            ! get if have complement 
            call json%get('structure.complement', complement, is_found) 

            if (is_found .and. complement) matrix_pattern = mod(matrix_pattern + 1, 2)

            ! get number of centers
            ncenters_main = size(centers_main, 1)

            if (has_background) then 
                ncenters_back = size(centers_back, 1)
            else 
                ncenters_back = 0
            end if

            ! allocate 
            allocate(centers(ncenters_back * (2 ** (2 * n) - sum(matrix_pattern))  + ncenters_main * sum(matrix_pattern), 6))

            ! init count centers
            count_centers = 1

            ! fill centers
            do i = 1, 2 ** n 
                do j = 1, 2 ** n 

                    if (matrix_pattern(i, j) == 1) then 

                        init = count_centers
                        fin = count_centers + ncenters_main - 1
                        
                        centers(init: fin, 1:3) = centers_main(:, 1:3)
                        centers(init: fin, 4) = centers_main(:, 4) + (i - 1) * a 
                        centers(init: fin, 5) = centers_main(:, 5) + (j - 1) * b 
                        centers(init: fin, 6) = centers_main(:, 6)

                        count_centers = fin + 1

                    else if(matrix_pattern(i, j) == 0 .and. has_background) then
                        
                        init = count_centers
                        fin = count_centers + ncenters_back - 1

                        centers(init: fin, 1:3) = centers_back(:, 1:3)
                        centers(init: fin, 4) = centers_back(:, 4) + (i - 1) * a 
                        centers(init: fin, 5) = centers_back(:, 5) + (j - 1) * b 
                        centers(init: fin, 6) = centers_back(:, 6)

                        count_centers = fin + 1

                    end if 
                end do 
            end do 

            ! deallocate 
            deallocate(centers_main, matrix_pattern)
            if (has_background) deallocate(centers_back)

        end function get_centers_structure_pattern

end module patterns