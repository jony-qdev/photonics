program photonics 

    use :: global_elements
    use :: statics 
    use :: centers_functions
    use :: fill_matrix_functions
    use :: algorithms
    use :: mpi

    implicit none

    ! variables 
    character(len=125) :: input_file
    character(len=4) :: rank_string
    character(len=:), allocatable :: structure_type, id_file, path_file, path_plot
    real, dimension(:, :), allocatable :: matrix_structure, matrix_centers
    real :: id_number
    integer :: i
    logical :: is_found, inputs_right, to_save_structure, to_plot_structure

    ! initialize mpi
    call MPI_INIT(ierr)

    ! Setup Comunicator Size
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr) 

    ! Setup Ranks/IDs for each process 
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

    ! verify arguments
    if (iargc() /= 1) then 
        if (rank == 0) then 
            call error_message('Only path input is neccesary as argument!')
        else 
            call exit(0)
        end if 
    end if 
    
    ! save input file argument
    call getarg(1, input_file)

    ! config json to get variables
    call json%initialize()
    call json%load_file(input_file)

    if (json%failed()) then 
        
        if (rank == 0) then 
            call error_message('JSON does not found')
        else 
            call exit()
        end if 

    end if 

    ! init variables 
    inputs_right = .true.

    ! execution id 
    if (rank == 0) then 
        call random_number(id_number)
    end if 

    call MPI_Bcast(id_number, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)

    write(id_execution, "(I10.10)") floor(id_number * 1e9)

    call init()

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    if(rank /= 0 .and. job == 'yee_specific_values') then 

        write(rank_string, '(I4.4)') rank

        ! open log file 
        open(200 + rank, file='wdir/'//trim(name_output_folder)//'/photonics'//trim(rank_string)//'.log')
    end if

    ! Welcome 
    if (rank == 0) call flat_message('Welcome to photonics UdeM!')

    ! get local variables json
    call json%get('structure.type', structure_type, is_found); if(.not. is_found) inputs_right = .false.

    ! verify inputs json 
    if (.not. inputs_right) then 

        if (rank == 0) then 
            call error_message('Verify structure inputs')
        else 
            call exit()
        end if

    end if 

    ! orchestator generator
    select case(job)

    case('generate_structure', 'yee', 'yee_specific_values')

        select case(structure_type) 

        case('circle_center', 'regular_polygon_center')

            ! get_matrix_centers
            matrix_centers = get_center_unit_cell()

        case('bravais_moire')

            ! get_matrix_centers
            matrix_centers = get_centers_bravais_moire()

        case('from_files')

            ! get_matrix_centers
            matrix_centers = get_centers_from_files()

        case ('thue_morse', 'rudin_shapiro')

            ! get_matrix_centers
            matrix_centers = get_centers_structure_pattern()

        case default 

            if (rank == 0) then 
                call error_message('Structure type does not exist')
            else 
                call exit(0)
            end if 

        end select 
    end select 

    ! orchestator saving and plot

    select case(job)

    case('generate_structure')

        ! get matrix_structure
        matrix_structure = fill_matrix(matrix_centers)

        if (rank == 0) call flat_message('Now you can change the job to run your task')

    case ('yee', 'yee_specific_values')

        ! get matrix_structure
        matrix_structure = fill_matrix(matrix_centers)

        call run_yee(matrix_structure)

        if (rank == 0 .and. job == 'yee') call success_message('All tasks ran!')
        if (rank == nprocs - 1 .and. job == 'yee_specific_values') call success_message('All tasks ran!', rank_opt=rank)

    case ('yee_gapmap')

        call run_yee_gapmap()

        if (rank == 0) call success_message('All tasks ran!')
    end select 

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! close log file 
    if (rank == 0) then 

        if (job == 'yee_specific_values') then 

            do i = 1, nprocs - 1

                write(rank_string, '(I4.4)') i

                call execute_command_line('cat wdir/'//trim(name_output_folder)//'/photonics'//trim(rank_string)//&
                    '.log >> wdir/'//trim(name_output_folder)//'/photonics.log')
                call execute_command_line('rm wdir/'//trim(name_output_folder)//'/photonics'//trim(rank_string)//'.log')

            end do 

        end if

        close(200)
    else if (job == 'yee_specific_values') then 
        close(200 + rank) 
    end if 

    ! destroy json 
    call json%destroy()

    ! finalize MPI 
    call MPI_FINALIZE(ierr)

end program photonics