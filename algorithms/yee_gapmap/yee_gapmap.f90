module yee_gapmap 

    use :: global_elements
    use :: centers_functions
    use :: fill_matrix_functions
    use :: statics
    use :: yee 

    implicit none 

    private 

    public :: run_yee_gapmap

    contains 

        subroutine run_yee_gapmap 

            ! to use 
            integer :: n_ff, i
            real :: init_ff, fin_ff, step_ff
            real, dimension(:), allocatable :: filling_fractions
            real, dimension(:, :), allocatable :: matrix_centers, matrix_structure
            character(len=5) :: id_file
            character(len=:), allocatable :: structure_type
            logical :: is_found, inputs_right 

            ! initialization
            inputs_right = .true.

            ! get json inputs to gapmaps
            call json%get('parameters.filling_fraction.start', init_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.filling_fraction.finalize', fin_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.filling_fraction.nfractions', n_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('structure.type', structure_type, is_found); if (.not. is_found) inputs_right = .false.

            ! verify inputs
            if (.not. inputs_right) call error_message('Verify inputs to yee gapmap') 

            ! calculate step_ff 
            step_ff = (fin_ff - init_ff) / n_ff

            ! allocate and  calculate filling fractions
            allocate(filling_fractions(n_ff))

            filling_fractions = [(init_ff + i * step_ff, i = 1, n_ff)]

            ! loop over n_ff
            do i = 1, n_ff 

                ! get id_file
                write(id_file, '(I0.5)') i 

                ! structure centers orchestator
                select case(structure_type) 

                case('circle_center', 'regular_polygon_center')
        
                    ! get_matrix_centers
                    matrix_centers = get_center_unit_cell(ff_opt=filling_fractions(i))
        
                case('bravais_moire')
        
                    ! get_matrix_centers
                    matrix_centers = get_centers_bravais_moire(ff_opt=filling_fractions(i))
        
                case('from_files')
        
                    ! get_matrix_centers
                    matrix_centers = get_centers_from_files(ff_opt=filling_fractions(i))
        
                case ('thue_morse', 'rudin_shapiro')
        
                    ! get_matrix_centers
                    matrix_centers = get_centers_structure_pattern(ff_opt=filling_fractions(i))

                case default 

                    call error_message('Structure type does not exist')

                end select 

                ! allocate 
                allocate(matrix_structure(nx, ny))

                ! fill matrix structure
                matrix_structure = fill_matrix(matrix_centers, id_file_opt=id_file) 

                ! run yee
                !call run_yee(matrix_structure, id_file_opt=id_file)

                ! allocate 
                deallocate(matrix_structure)

            end do 

        end subroutine run_yee_gapmap

end module yee_gapmap