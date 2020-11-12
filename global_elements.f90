module global_elements

    use :: statics

    implicit none 

    ! general variables
    type(json_file) :: json

    ! variables to structures
    !real, dimension(:, :), allocatable :: centers_structure
    real :: back_permittivity, lambda, side_x, side_y, dx, dy
    character(len=:), allocatable :: name_execution, name_output_folder, job
    character(len=10) :: id_execution
    integer :: nr, nx, ny, n

    contains 

        subroutine init 

            ! to use 
            logical :: is_found, inputs_right
            real :: id_number

            ! initialization
            inputs_right = .true.

            call json%get('job', job, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure.eps_2', back_permittivity, is_found); if(.not. is_found) back_permittivity = 1.0
            call json%get('structure.lambda', lambda, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure.nr', nr, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('name', name_execution, is_found); if(.not. is_found) name_execution = 'current_task'

            ! verify inputs json 
            if (.not. inputs_right) call error_message('Verify structure inputs')

            ! remove whitespaces of the name execution
            call strip_spaces(name_execution)

            ! execution id 
            call random_number(id_number)
            write(id_execution, "(I10.10)") floor(id_number * 1e9)

            ! name output folder
            name_output_folder = id_execution//'_'//name_execution

            ! create output folder
            call execute_command_line('mkdir wdir/'//trim(name_output_folder))

            ! open log file 
            open(200, file='wdir/'//trim(name_output_folder)//'/photonics.log')

        end subroutine init 

end module global_elements