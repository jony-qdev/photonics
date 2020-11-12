

program photonics 

    use :: global_elements
    use :: statics 
    use :: centers_functions
    use :: fill_matrix_functions
    use :: algorithms

    implicit none

    ! variables 
    character(len=125) :: input_file
    character(len=:), allocatable :: structure_type, id_file, path_file, path_plot
    real, dimension(:, :), allocatable :: matrix_structure, matrix_centers
    logical :: is_found, inputs_right, to_save_structure, to_plot_structure

    ! verify arguments
    if (iargc() /= 1) call error_message('Only path input is neccesary as argument!')

    ! save input file argument
    call getarg(1, input_file)

    ! config json to get variables
    call json%initialize()
    call json%load_file(input_file)

    if (json%failed()) call error_message('JSON does not found')

    ! init variables 
    inputs_right = .true.
    call init()

    ! Welcome 
    call flat_message('Welcome to photonics UdeM!')

    ! get local variables json
    call json%get('structure.type', structure_type, is_found); if(.not. is_found) inputs_right = .false.

    ! verify inputs json 
    if (.not. inputs_right) call error_message('Verify structure inputs')

    ! orchestator generator

    select case(job)

    case('generate_structure', 'yee', 'yee_specific_values')

        select case(structure_type) 

        case('circle_center', 'regular_polygon_center')

            ! get_matrix_centers
            matrix_centers = get_center_unit_cell()

            ! get matrix_structure
            matrix_structure = fill_matrix(matrix_centers)

        case('bravais_moire')

            ! get_matrix_centers
            matrix_centers = get_centers_bravais_moire()

            ! get matrix_structure
            matrix_structure = fill_matrix(matrix_centers)

        case('from_files')

            ! get_matrix_centers
            matrix_centers = get_centers_from_files()

            ! get matrix_structure
            matrix_structure = fill_matrix(matrix_centers)

        case ('thue_morse', 'rudin_shapiro')

            ! get_matrix_centers
            matrix_centers = get_centers_structure_pattern()

            ! get matrix_structure
            matrix_structure = fill_matrix(matrix_centers)

        case default 
            call error_message('Structure type does not exist')
        end select 
    end select 

    ! orchestator saving and plot

    select case(job)

    case('generate_structure')

        call flat_message('Now you can change the job to run your task')

    case ('yee', 'yee_specific_values')

        call run_yee(matrix_structure)

        call success_message('All tasks ran!')

    case ('yee_gapmap')

        call run_yee_gapmap()

        call success_message('All tasks ran!')
    end select 

    ! close log file 
    close(200)

    ! destroy json 
    call json%destroy()

end program photonics