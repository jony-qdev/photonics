module bravais_moire 

    ! Module to structure of bravais moire in square

    use :: global_elements 
    use :: statics 

    implicit none 

    private 

    public :: get_centers_bravais_moire

    contains 

        ! Function to get centers of bravais moire
        ! Inputs 
        !   --- source_data_opt : string, at what level should we get the inputs?
        !
        ! Result 
        !   --- centers : matrix(ncenters, 5), shape_param, valid_points, permittivity, x, y

        function get_centers_bravais_moire(source_data_opt, ff_opt) result(centers)

            ! inputs
            character(len=*), optional :: source_data_opt 
            real, optional :: ff_opt

            ! output 
            real, dimension(:, :), allocatable :: centers
            
            ! to use 
            logical :: is_found, inputs_right, are_coprimes
            character(len=:), allocatable :: source_data, structure_type_rot, structure_type_not_rot
            integer :: r, s, t, nrows, ncols, ncenters, i, j, idx, valid_points_rot_param, valid_points_not_rot_param, nconds
            real :: measurable_angle, m , init_value, eq_value, permittivity_rot, permittivity_not_rot, shape_param_rot, &
                    shape_param_not_rot, angrot, units, angrot_rot, angrot_not_rot
            real, dimension(4, 2) :: vertices
            real, dimension(:, :), allocatable :: matrix_centers, matrix_rotation, matrix_points

            ! initialization
            inputs_right = .true. 
            angrot_rot = 0 
            angrot_not_rot = 0

            ! verify source of data
            if (present(source_data_opt)) then 
                source_data = '.'//source_data_opt 
            else 
                source_data = ''
            end if 

            ! get r and s 
            call json%get('structure'//source_data//'.r', r, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.s', s, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.units', units, is_found); if(.not. is_found) inputs_right = .false.

            ! verify inputs json 
            if (.not. inputs_right) then 
                
                if (rank == 0) then 
                    call error_message('Verify structure inputs')
                else 
                    call exit(0)
                end if 
            
            end if

            ! verify r and s values 
            if (r <= 1) then 
                
                if (rank == 0) then 
                    call error_message('Verify r value')
                else 
                    call exit(0)
                end if 

            end if 

            if (s <= 0) then 
                
                if (rank == 0) then 
                    call error_message('Verify s value')
                else 
                    call exit(0)
                end if 
            
            end if
            
            if (s >= r) then 

                if (rank == 0) then 
                    call error_message('Verify r and s values')
                else 
                    call exit(0)
                end if 
            
            end if

            call coprimes(r, s, are_coprimes)

            if(.not. are_coprimes) then 
                
                if (rank == 0) then 
                    call error_message('The numbers r and s are not coprimes')
                else 
                    call exit(0)
                end if 
            
            end if

            ! rows and cols 
            t = 2 * r
            nrows = 2 * t + 1
            ncols = 2 * t + 1
            
            ! vertices
            vertices(1, :) = [r, s]
            vertices(2, :) = [-s, r] 
            vertices(3, :) = [-r, -s]
            vertices(4, :) = [s, -r]

            ! allocate
            allocate(matrix_points(2 * nrows * ncols, 2))
            allocate(matrix_centers(2 * nrows * ncols, 3))

            ! calculate centers
            do i = 1, nrows 
                matrix_points((i - 1) * ncols + 1 : i * ncols, 1) = [(j, j = -t, t)]
                matrix_points((i - 1) * ncols + 1 : i * ncols, 2) = [(i - t - 1, j = 1, ncols)]
            end do 

            ! move centers
            matrix_points = matrix_points + 0.5 

            ! measurable angle and matrix_rotation
            measurable_angle = get_measurable_angle(r, s)
            matrix_rotation = get_matrix_rotation_2d(measurable_angle)

            ! fill with rotated
            matrix_points(nrows * ncols + 1 : , :) = matmul(matrix_points(1 : nrows * ncols, :), matrix_rotation)

            ! select points in the square 
            ncenters = 0

            ! loop in points
            do i = 1, 2 * nrows * ncols 

                nconds = 0

                ! loop in lines 
                do j = 1, 4

                    ! next point 
                    idx = j + 1

                    if (j == 4) idx = 1

                    ! slope 
                    m = (vertices(idx, 2) - vertices(j, 2)) / (vertices(idx, 1) - vertices(j, 1))

                    ! line evaluated in zero
                    init_value = -vertices(j, 2) + m * vertices(j, 1)

                    ! line evaluated in point 
                    eq_value = (matrix_points(i, 2) - vertices(j, 2)) - m * (matrix_points(i, 1) - vertices(j, 1)) 

                    ! verify if point is valid 
                    if (init_value * eq_value >= 0) nconds = nconds + 1

                end do 

                ! add point
                if (nconds == 4) then 
                    ncenters = ncenters + 1

                    matrix_centers(ncenters, 1:2) = matrix_points(i, :)

                    ! add type rot or not rot
                    if (i < nrows * ncols + 1) then
                        matrix_centers(ncenters, 3) = 0
                    else
                        matrix_centers(ncenters, 3) = 1
                    end if 

                end if 
            end do 

            ! doing rotations
            ! slope 
            m = (vertices(2, 2) - vertices(1, 2)) / (vertices(2, 1) - vertices(1, 1))
            angrot = atan(m)

            matrix_rotation = get_matrix_rotation_2d(angrot)
            vertices = matmul(vertices, matrix_rotation)

            side_x = abs(vertices(1, 1)) * 2

            matrix_centers(1 : ncenters, 1:2) = matmul(matrix_centers(1 : ncenters, 1:2), matrix_rotation) 

            ! we take to the origin and put units
            vertices = (vertices + side_x / 2.0) * units
            matrix_centers(1 : ncenters, 1:2) = (matrix_centers(1 : ncenters, 1:2) + side_x / 2.0) * units
            side_x = side_x * units 
            side_y = side_x

            ! to principal structure
            if (.not. present(source_data_opt)) then 

                nx = ceiling(side_x / (lambda / nr))
                ny = ceiling(side_y / (lambda / nr))
                dx = side_x / nx 
                dy = side_y / ny

            end if 

            ! get structure type 
            call json%get('structure'//source_data//'.rotated.type', structure_type_rot, is_found); 
                if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.not_rotated.type', structure_type_not_rot, is_found); 
                if(.not. is_found) inputs_right = .false.

            ! get permittivity 
            call json%get('structure'//source_data//'.rotated.eps_1', permittivity_rot, is_found); 
                if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.not_rotated.eps_1', permittivity_not_rot, is_found); 
                if(.not. is_found) inputs_right = .false.

            ! get shape param rot 
            if (structure_type_rot == 'circles') then 

                ! for ff_opt
                if (present(ff_opt)) then 
                    shape_param_rot = sqrt(ff_opt * side_x * side_y / (ncenters * PI))
                else 
                    call json%get('structure'//source_data//'.rotated.factor_radio', shape_param_rot, is_found); 
                        if(.not. is_found) inputs_right = .false.
                end if 

                valid_points_rot_param = 1

            else if (structure_type_rot == 'regular_polygons') then 
                
                call json%get('structure'//source_data//'.rotated.nsides', valid_points_rot_param, is_found); 
                    if(.not. is_found) inputs_right = .false.

                ! for ff_opt
                if (present(ff_opt)) then 
                    shape_param_rot = sqrt(ff_opt * side_x * side_y / (ncenters * PI))
                    shape_param_rot = 2.0 * shape_param_rot * sin(PI / valid_points_rot_param)
                else 
                    call json%get('structure'//source_data//'.rotated.factor_side', shape_param_rot, is_found); 
                        if(.not. is_found) inputs_right = .false.
                end if 

                call json%get('structure'//source_data//'.rotated.angle_rotation', angrot_rot, is_found);
                    if(is_found) angrot_rot = angrot_rot * PI / 180 
            end if 

            ! get shape param not rot 
            if (structure_type_not_rot == 'circles') then 

                ! for ff_opt
                if (present(ff_opt)) then 
                    shape_param_not_rot = sqrt(ff_opt * side_x * side_y / (ncenters * PI))
                else 
                    call json%get('structure'//source_data//'.not_rotated.factor_radio', shape_param_not_rot, is_found); 
                        if(.not. is_found) inputs_right = .false.
                end if

                valid_points_not_rot_param = 1

            else if (structure_type_not_rot == 'regular_polygons') then
                call json%get('structure'//source_data//'.not_rotated.nsides', valid_points_not_rot_param, is_found); 
                    if(.not. is_found) inputs_right = .false.

                ! for ff_opt 
                if (present(ff_opt)) then 
                    shape_param_not_rot = sqrt(ff_opt * side_x * side_y / (ncenters * PI))
                    shape_param_not_rot = 2.0 * shape_param_not_rot * sin(PI / valid_points_not_rot_param)
                else 
                    call json%get('structure'//source_data//'.not_rotated.factor_side', shape_param_not_rot, is_found); 
                        if(.not. is_found) inputs_right = .false.
                end if 
                
                call json%get('structure'//source_data//'.not_rotated.angle_rotation', angrot_not_rot, is_found);
                    if(is_found) angrot_not_rot = angrot_not_rot * PI / 180
            end if 

            ! verify inputs json 
            if (.not. inputs_right) then 

                if (rank == 0) then 
                    call error_message('Verify structure inputs')
                else 
                    call exit(0)
                end if

            end if

            ! fix shape_param 
            if (.not. present(ff_opt)) then 
                shape_param_rot = shape_param_rot * side_x 
                shape_param_not_rot = shape_param_not_rot * side_x
            end if

            ! get centers 
            allocate(centers(ncenters, 6))

            ! count in centers 
            j = 1

            ! to rotated
            do i = 1, ncenters

                ! 0 not rot, 1 rot
                if (matrix_centers(i, 3) == 0) then 

                    centers(j, 1) = shape_param_not_rot
                    centers(j, 2) = valid_points_not_rot_param
                    centers(j, 3) = permittivity_not_rot 
                    centers(j, 4) = matrix_centers(i, 1)
                    centers(j, 5) = matrix_centers(i, 2)
                    centers(j, 6) = angrot_not_rot

                    j = j + 1

                end if 
            end do 

            ! to not rotated
            do i = 1, ncenters

                ! 0 not rot, 1 rot
                if (matrix_centers(i, 3) == 1) then 

                    centers(j, 1) = shape_param_rot
                    centers(j, 2) = valid_points_rot_param
                    centers(j, 3) = permittivity_rot 
                    centers(j, 4) = matrix_centers(i, 1)
                    centers(j, 5) = matrix_centers(i, 2)
                    centers(j, 6) = angrot_rot

                    j = j + 1

                end if 
            end do 

            ! deallocate 
            deallocate(matrix_centers, matrix_rotation, matrix_points)

        end function get_centers_bravais_moire 

end module bravais_moire 