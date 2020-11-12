module fill_matrix_functions

    ! Module to call other modules of fill matrices

    use :: global_elements
    use :: fill_matrix_circles
    use :: fill_matrix_regular_polygons

    contains 

        ! Function to fill matrix with structure 
        ! Inputs 
        !   --- centers_structure: array, with centers and attributes 
        !   
        ! Results 
        !   --- matrix_structure: matrix, with structure filled

        function fill_matrix(centers_structure, id_file_opt) result(matrix_structure)

            ! inputs 
            real, dimension(:, :), allocatable :: centers_structure 
            character(len=*), optional :: id_file_opt

            ! outputs 
            real, dimension(nx, ny) :: matrix_structure 

            ! to use 
            real, dimension(:, :), allocatable :: centers_circles, centers_regular_polygons, matrix_rotation
            real :: angle_rotation, min_percentage_move, max_percentage_move, random_move, random_perc
            integer :: i, j, k, ncenters_circles, ncenters_regular_polygons
            character(len=:), allocatable :: id_file, path_file, path_plot, path_centers_file, structure_type
            logical :: to_save, to_plot, is_found, inputs_right

            ! initialization 
            inputs_right = .true.

            ! verify rotation
            call json%get('structure.angle_rotation', angle_rotation, is_found)

            if (is_found) then 

                matrix_rotation = get_matrix_rotation_2d(PI / 180 * angle_rotation)
                centers_structure(:, 4:5) = matmul(centers_structure(:, 4:5), matrix_rotation)

                ! deallocate 
                deallocate(matrix_rotation)

            end if 

            ! verify random movement
            call json%get('structure.random_movement.min_percentage', min_percentage_move, is_found); 
            
            if(is_found) then 

                call json%get('structure.random_movement.max_percentage', max_percentage_move, is_found); 

                ! verify inputs json 
                if (.not. is_found) call error_message('Verify structure inputs')

                ! calculate random movement
                do i = 1, size(centers_structure, 1)
                    ! in x
                    call random_number(random_perc)
                    random_perc = (min_percentage_move + random_perc * (max_percentage_move - min_percentage_move)) / 100

                    call random_number(random_move)
                    centers_structure(i, 4) = centers_structure(i, 4) * (1 + 2 * random_move * random_perc - random_perc)

                    ! in y 
                    call random_number(random_perc)
                    random_perc = (min_percentage_move + random_perc * (max_percentage_move - min_percentage_move)) / 100

                    call random_number(random_move)
                    centers_structure(i, 5) = centers_structure(i, 5) * (1 + 2 * random_move * random_perc - random_perc)

                end  do 

            else 
                call json%get('structure.random_movement.max_percentage', max_percentage_move, is_found)

                ! verify inputs json 
                if (is_found) call error_message('Verify structure inputs')

            end if 

            ! verify id file execution and remove whitespaces
            if (present(id_file_opt)) then 
                id_file = id_file_opt 
            else 
                call json%get('saving.structure.id_file', id_file, is_found); if(.not. is_found) id_file = ''
            end if 

            call strip_spaces(id_file)

            ! get structure type
            call json%get('structure.type', structure_type, is_found)
                if(.not. is_found) call error_message('Verify structure inputs')

            ! initialization 
            ncenters_circles = 0 
            ncenters_regular_polygons = 0
            matrix_structure = back_permittivity 

            ! get ncenters 
            do i = 1, size(centers_structure, 1)

                if (int(centers_structure(i, 2)) == 1) then 
                    ncenters_circles = ncenters_circles + 1
                else 
                    ncenters_regular_polygons = ncenters_regular_polygons + 1
                end if
            end do 

            ! allocate 
            if (ncenters_circles > 0) allocate(centers_circles(ncenters_circles, 6))
            if (ncenters_regular_polygons > 0) allocate(centers_regular_polygons(ncenters_regular_polygons, 6))

            ! counts of circles and regular polygons 
            j = 0
            k = 0

            ! separate centers
            do i = 1, size(centers_structure, 1)
                if(int(centers_structure(i, 2)) == 1) then 
                    j = j + 1

                    centers_circles(j, :) = centers_structure(i, :)
                else 
                    k = k + 1
                    
                    centers_regular_polygons(k, :) = centers_structure(i, :)
                end if 
            end do 

            ! fill matrix_structure
            if (ncenters_circles > 0) call get_matrix_structure_circles(centers_circles, matrix_structure)
            if (ncenters_regular_polygons > 0) &
                call get_matrix_structure_regular_polygons(centers_regular_polygons, matrix_structure)

            ! to save 
            call json%get('saving.structure.status', to_save, is_found); if(.not. is_found) to_save = .false.

            if (to_save) then 

                ! path files to structure and centers
                path_file = 'wdir/'//name_output_folder//'/structure_'//structure_type//'_'//trim(id_file)//'.dat'
                path_centers_file = 'wdir/'//name_output_folder//'/centers_'//structure_type//'_'//trim(id_file)//'.dat'

                ! save structure
                call save_structure(path_file, matrix_structure, nx, ny, dx, dy, space_each_x_opt=.true.)

                ! save centers
                call save_xy_plot(path_centers_file, centers_structure(:, 4:5), size(centers_structure, 1))

                ! to plot
                call json%get('plots.structure.status', to_plot, is_found); if(.not. is_found) to_plot = .false.

                if (to_plot) then 
                    path_plot = 'wdir/'//name_output_folder//'/plot_structure_'//structure_type//'_'//trim(id_file)

                    call plot_structure(path_file, path_plot)

                end if 

            end if

            !delete
            print *, nx, ny

            ! deallocate
            if (ncenters_circles > 0) deallocate(centers_circles)
            if (ncenters_regular_polygons > 0) deallocate(centers_regular_polygons)

        end function fill_matrix

end module fill_matrix_functions