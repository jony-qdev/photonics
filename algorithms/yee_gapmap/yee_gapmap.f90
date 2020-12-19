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
            integer :: n_ff, i, j, n_ff_local, k_npoints, nfreqs
            real :: init_ff, fin_ff, step_ff, minimum_width, aux
            real, dimension(:), allocatable :: filling_fractions, dispersion
            real, dimension(:, :), allocatable :: matrix_centers, matrix_structure, hist
            character(len=5) :: id_file
            character(len=:), allocatable :: structure_type, subjob
            character(len=125) :: name_file
            logical :: is_found, inputs_right 

            ! initialization
            inputs_right = .true.

            ! get json inputs to gapmaps
            call json%get('subjob', subjob, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.k.npoints', k_npoints, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.frequencies_per_k', nfreqs, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.minimum_width', minimum_width, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.filling_fraction.start', init_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.filling_fraction.finalize', fin_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.filling_fraction.nfractions', n_ff, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('structure.type', structure_type, is_found); if (.not. is_found) inputs_right = .false.

            ! verify inputs
            if (.not. inputs_right) call error_message('Verify inputs to yee gapmap') 

            ! for parallel process
            n_ff_local = ceiling(n_ff / float(nprocs))
            n_ff = n_ff_local * nprocs

            ! calculate step_ff 
            step_ff = (fin_ff - init_ff) / n_ff

            ! allocate and  calculate filling fractions
            allocate(filling_fractions(n_ff))

            filling_fractions = [(init_ff + i * step_ff, i = 1, n_ff)]

            ! loop over n_ff
            do i = rank * n_ff_local + 1, n_ff_local * (rank + 1)

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
                call run_yee(matrix_structure, id_file_opt=id_file)

                ! allocate 
                deallocate(matrix_structure)

            end do 

            if (rank == 0) then

                allocate(dispersion(k_npoints * nfreqs))

                do i = 1, n_ff 

                    ! get id_file
                    write(id_file, '(I0.5)') i 

                    name_file = 'wdir/'//name_output_folder//'/yee_'//subjob//'_dispersion_'//structure_type//'_'&
                    //trim(id_file)//'.dat'

                    open(100, file=name_file, status='old')

                        do j = 1, k_npoints * nfreqs
                            read(100, *) aux, dispersion(j)
                        end do

                    close(100)

                    hist = get_histogram(dispersion, minimum_width)

                    do j = 1, size(hist, 1)

                        if (int(hist(j, 1)) == 0) then 
                            print *, i, hist(j, 2:)
                        end if 

                    end do 

                    if (i == 2) exit
                end do

            end if 

        end subroutine run_yee_gapmap


        function get_histogram(data, step) result(x) 

            ! inputs 
            real, dimension(:), intent(in) :: data 
            real, intent(in) :: step 

            ! output
            real, dimension(:, :), allocatable :: x

            ! to use 
            integer :: n_class, i, j
            real, dimension(:), allocatable :: range

            ! initialize
            n_class = ceiling((maxval(data) - minval(data)) / step)

            ! allocate
            allocate(range(n_class))
            allocate(x(n_class, 3))

            ! fill range 
            range = [(minval(data) + (i - 1)*step, i = 1, n_class)]

            ! initialize
            x(:, 1) = 0.0

            ! fill ranges in x 
            do i = 1, n_class

                if (i == 1 .or. i == n_class) then
                    x(i, 2 : 3) = [range(i), range(i)] 
                else 
                    x(i, 2 : 3) = [range(i - 1), range(i)]
                end if
            end do

            ! fill count x
            do i = 1, size(data, 1)
                do j = 1, n_class

                    if (data(i) < range(j)) then 
                        x(j, 1) = x(j, 1) + 1
                        exit
                    end if

                end do 
            end do 


        end function get_histogram

end module yee_gapmap
