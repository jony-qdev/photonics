module yee 

    ! Module with algorithm for the numerical solution of Maxwell's equations
    ! in an arbitrary region of space, by means of central finite differences
    ! with second order of precision. This algorithm is commonly known as the
    ! method of finite differences in the time domain.

    use :: statics 
    use :: global_elements

    implicit none 

    private 

    public :: run_yee

    ! global variables
    complex, dimension(:, :), allocatable :: matrix_z, matrix_x, matrix_y
    integer, dimension(2) :: excitement_point
    real :: width, start_time, amplitude
    character(len=:), allocatable :: structure_type

    ! for fft 
    double complex, dimension(:), allocatable :: array_in_fft

    contains 

        ! Subroutine to calculate yee algorithm with FTDT
        !
        ! Inputs : 
        !   --- matrix_structure: matrix, matrix with structure
        !   --- id_file_opt: string, optional, string with a number to put in the finish of the name of the file
        !   
        ! Returns:
        !   --| dispersion data, plt and png files to tm or te if job is yee
        !   --| field profiles data plt and png files if job is yee_specific_values

        subroutine run_yee(matrix_structure, id_file_opt)

            ! inputs 
            real, dimension(:, :), intent(in) :: matrix_structure
            character(len=*), optional :: id_file_opt

            ! to use 
            complex :: rotate_x, rotate_y
            real :: mu, k_add, dt, mu_x, mu_y, freq, omega, t, kx, ky,  re_rotate_x, im_rotate_x, re_rotate_y, im_rotate_y
            real, dimension(:), allocatable :: array_ks, array_frequencies, xlim, ylim
            real, dimension(:, :), allocatable :: matrix_eps_x, matrix_eps_y, matrix_dispersion, matrix_kvalues
            integer :: nt, nfreqs, k_npoints, k, q
            logical :: is_found, inputs_right, to_plot, has_xlim, has_ylim
            character(len=:), allocatable :: source_type, subjob, id_file, path_file, path_plot
            character(len=5) :: str_k, str_freq

            ! initialization 
            inputs_right = .true.

            ! get json inputs
            call json%get('subjob', subjob, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('structure.type', structure_type, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.mu', mu, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.dimensions.nt', nt, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('parameters.frequencies_per_k', nfreqs, is_found); if (.not. is_found) inputs_right = .false.
            call json%get('source.type', source_type, is_found); if (.not. is_found) inputs_right = .false.

            ! verify id file execution and remove whitespaces
            if (present(id_file_opt)) then 
                id_file = id_file_opt 
            else 
                call json%get('saving.structure.id_file', id_file, is_found); if(.not. is_found) id_file = ''
            end if 

            call strip_spaces(id_file)

            ! type task for getting inputs
            select case (job)

            case ('yee', 'yee_gapmap')

                call json%get('parameters.k.npoints', k_npoints, is_found); if (.not. is_found) inputs_right = .false.
                call json%get('parameters.k.add_to_kpath', k_add, is_found); if(.not. is_found) k_add = 0.0

            case ('yee_specific_values')

                call json%get('parameters.specific_ks', array_ks, is_found); if (.not. is_found) inputs_right = .false.
                call json%get('parameters.specific_frequencies', array_frequencies, is_found); &
                    if (.not. is_found) inputs_right = .false.

                if (size(array_ks) /= size(array_frequencies)) call error_message('specific_ks and specific_frequencies &
                                                                                    should be have the same size')

                ! calculate variables
                k_npoints = size(array_ks)

            end select
            
            ! verify inputs
            if (.not. inputs_right) call error_message('Verify inputs to yee') 

            ! get source inputs
            select case (source_type)

            case('gaussian_source')
                call json%get('source.width', width, is_found); if (.not. is_found) inputs_right = .false.
                call json%get('source.start_time', start_time, is_found); if (.not. is_found) inputs_right = .false.
                call json%get('source.amplitude', amplitude, is_found); if (.not. is_found) inputs_right = .false.

                if (.not. inputs_right) call error_message('Verify inputs source yee') 

            case default

                call error_message('Source type does not exist')

            end select 

            ! type structure for getting excitement point and inputs
            select case(structure_type)
            case ('circle_center', 'regular_polygon_center', 'from_files', 'bravais_moire')

                excitement_point(1) = nint(nx / 3.0)
                excitement_point(2) = nint(ny / 4.0)

            case ('thue_morse', 'rudin_shapiro')
                
                excitement_point(1) = nint(nx / 3.0)
                excitement_point(2) = nint(ny / 4.0)

            end select 

            ! allocate
            allocate(matrix_eps_x(nx, ny), matrix_eps_y(nx, ny))
            allocate(matrix_z(nx, ny), matrix_x(nx, ny), matrix_y(nx, ny))
            allocate(matrix_dispersion(k_npoints * nfreqs, 2))

            allocate(array_in_fft(nt))

            allocate(matrix_kvalues(k_npoints, 2))

            ! variables to calculate
	        dt = 1.0 / (C * (1.0 / (dx * dx) + 1.0 / (dy * dy)) ** (0.5))
	        mu_x = C * dt / (mu * dx * ETA)
            mu_y = C * dt / (mu * dy * ETA)

            ! calculate matrices of eps x and eps y
            matrix_eps_x = C * dt * ETA / (dx * matrix_structure)
            matrix_eps_y = C * dt * ETA / (dy * matrix_structure)

            ! calculate kvectors, omega and freq 
            if (job == 'yee' .or. job == 'yee_gapmap') then 
                freq = C / lambda
                omega = 2.0 * PI * freq

                call get_kvectors_yee(matrix_kvalues, k_npoints, array_ks)

            else 
                ! calculate kvectors yee_specific_values

                call get_kvectors_yee_specific_values(matrix_kvalues, k_npoints, array_ks)

            end if 

            ! calculate yee for each k
            do k = 1, k_npoints 

                ! calculate omega an freq
                if (job == 'yee_specific_values') then 
                    if (structure_type == 'thue_morse' .or. structure_type == 'rudin_shapiro') then 
                        freq = array_frequencies(k) * C / (side_x / 2 ** n)
                    else    
                        freq = array_frequencies(k) * C / side_x
                    end if 

                    omega = 2.0 * PI * freq
                end if 

                ! initialize matrices an time
                matrix_z = 0.0
                matrix_x = 0.0 
                matrix_y = 0.0 

                t = 0.0 

                ! save kx and ky
                kx = matrix_kvalues(k, 1)
                ky = matrix_kvalues(k, 2)

                ! calculate variables of rotate
                re_rotate_x = exp(0.0) * cos(kx * side_x)
	            im_rotate_x = exp(0.0) * sin(kx * side_x)
	            rotate_x = cmplx(re_rotate_x, im_rotate_x)

	            re_rotate_y = exp(0.0) * cos(ky * side_x)
	            im_rotate_y = exp(0.0) * sin(ky * side_x)
                rotate_y = cmplx(re_rotate_y, im_rotate_y)

                ! loop over time
                do q = 1, nt 

                    ! view subjob and run the subroutine
                    select case(subjob) 
                    case('tm')
                        call subjob_tm(t, omega, mu_x, mu_y, matrix_eps_x, matrix_eps_y, rotate_x, rotate_y)
                    case('te')
                        call subjob_te(t, omega, mu_x, mu_y, matrix_eps_x, matrix_eps_y, rotate_x, rotate_y)
                    case default 
                        call error_message('subjob algorithm does not exist')
                    end select 

                    array_in_fft(q) = matrix_z(nint(nx / 3.0), nint(ny / 7.0))
                    
                    t = q * dt

                end do

                ! calculate freqs to yee 

                if (job == 'yee' .or. job == 'yee_gapmap') then 

                    call get_dispersion(matrix_dispersion, k, nt, dt, array_ks(k), nfreqs)

                else 

                    ! save profile field
                    write(str_k, '(F5.3)') array_ks(k)
                    write(str_freq, '(F5.3)') array_frequencies(k)

                    path_file = 'wdir/'//name_output_folder//'/yee_'//subjob//'_k_' &
                                //trim(str_k)//'_freq_'//trim(str_freq)//'_profile_field_' &
                                //trim(structure_type)//'_'//trim(id_file)//'.dat'

                    call save_structure(path_file, real(matrix_z), nx, ny, dx, dy, space_each_x_opt=.true.)

                    ! plot profile field
                    call json%get('plots.field_profile.status', to_plot, is_found); if(.not. is_found) to_plot = .false.

                    if (to_plot) then 
                    
                        path_plot = 'wdir/'//name_output_folder//'/plot_yee_'//subjob//'_k_' &
                                    //trim(str_k)//'_freq_'//trim(str_freq)//'_profile_field_' &
                                    //trim(structure_type)//'_'//trim(id_file)

                        call plot_structure(path_file, path_plot)

                    end if
                end if 

            end do

            if (job == 'yee' .or. job == 'yee_gapmap') then 
            
                ! save dispersion 
                path_file = 'wdir/'//name_output_folder//'/yee_'//subjob//'_dispersion_' &
                            //trim(structure_type)//'_'//trim(id_file)//'.dat'


                call save_xy_plot(path_file, matrix_dispersion, k_npoints * nfreqs, add_x_opt=k_add)

                ! plot dispersion

                call json%get('plots.dispersion.status', to_plot, is_found); if(.not. is_found) to_plot = .false.

                if (to_plot) then 

                    path_plot = 'wdir/'//name_output_folder//'/yee_'//subjob//'_dispersion_' &
                                    //trim(structure_type)//'_'//trim(id_file)

                    ! get lims 
                    call json%get('plots.dispersion.xlim', xlim, has_xlim)
                    call json%get('plots.dispersion.ylim', ylim, has_ylim)

                    ! plot
                    if (has_xlim .and. has_ylim) then 
                        call plot_xy(path_file, path_plot, xlim, ylim)
                    else if (has_xlim) then 
                        call plot_xy(path_file, path_plot, xlim_opt=xlim)
                    else if (has_ylim) then 
                        call plot_xy(path_file, path_plot, ylim_opt=ylim)
                    else 
                        call plot_xy(path_file, path_plot)
                    end if 

                end if 
            else 

                ! deallocate
                deallocate(array_frequencies)

            end if

            ! deallocate
            deallocate(matrix_eps_x, matrix_eps_y, matrix_dispersion, matrix_kvalues)
	        deallocate(matrix_z, matrix_x, matrix_y)
	        deallocate(array_in_fft, array_ks)

        end subroutine run_yee 

        ! Subroutine to calculate dispersion and get frequencies
        !
        ! Inputs : 
        !   --- matrix_disperion: matrix, where we going to save the values of the dispersion
        !   --- k: integer, counter of k values
        !   --- nt: integer, times in time
        !   --- kvalue: real, value of k 
        !   --- nfreqs: number of frequencies per k
        !   
        ! Returns:
        !   --- matrix_dispersion: matrix, with values filled

        subroutine get_dispersion(matrix_dispersion, k, nt, dt, kvalue, nfreqs)

            ! inputs 
            integer, intent(in) :: k, nt, nfreqs
            real, intent(in) :: dt, kvalue

            ! outputs
            real, dimension(:, :), intent(inout) :: matrix_dispersion
            integer :: q, i

            ! to use
            real, dimension(:), allocatable :: array_fnorm, array_freqs_fourier
            real(16), dimension(:), allocatable :: array_fourier
            real :: setting_param
            logical :: is_found

            ! get param adjustment
            call json%get('settings.dispersion.setting_parameter', setting_param, is_found)
                if(.not. is_found) setting_param = 1.001

            ! allocate
            allocate(array_fourier(0 : nt))
            allocate(array_fnorm(0 : nt))
            allocate(array_freqs_fourier(nfreqs))

            ! calculate fast fourier transform
            call fft(array_in_fft)
            array_fourier = abs(array_in_fft)
            
            ! normalize
            array_fnorm = (2.0 * PI / dt) * [(q, q = 1, nt)] / nt 

            ! initialization counter
            i = 1 

            ! get first frequency
            if ((array_fourier(1) / array_fourier(2)) .gt. setting_param) then 
                array_freqs_fourier(i) =  array_fnorm(1)
            end if

            ! get frequencies
            do q = 2, nt - 1

                if (((array_fourier(q) / array_fourier(q - 1)) .gt. setting_param) .and. &
                    ((array_fourier(q) / array_fourier(q + 1)) .gt. setting_param)) then

                    array_freqs_fourier(i) = array_fnorm(q)

                    select case(structure_type)

                    case('thue_morse', 'rudin_shapiro')

                        matrix_dispersion(i + (k - 1) * nfreqs, :) = [kvalue, &
                            array_fnorm(q) * (side_x / 2 ** n) / (2.0 * PI * C)]
                    case default 

                        matrix_dispersion(i + (k - 1) * nfreqs, :) = [kvalue, &
                            array_fnorm(q) * side_x / (2.0 * PI * C)]

                    end select
                    
                    i = i + 1

                end if

                if (i == nfreqs + 1) exit
            end do 

            ! deallocate
            deallocate(array_fnorm, array_freqs_fourier, array_fourier)

        end subroutine get_dispersion

        ! Subroutine to calculate k vector based in k values
        !
        ! Inputs : 
        !   --- matrix_kvalues: matrix, where we going to save k vectors
        !   --- k_npoints: integer, number of values of k in the path
        !   --- array_kvalues: array, with k values to calculate k vectors
        !   
        ! Returns:
        !   --- matrix_kvalues: matrix, with values filled of kvectors

        subroutine get_kvectors_yee(matrix_kvalues, k_npoints, array_kvalues) 

            ! inputs
            integer, intent(in) :: k_npoints

            ! outputs 
            real, dimension(:, :), intent(inout) :: matrix_kvalues
            real, dimension(:), allocatable, intent(inout) :: array_kvalues

            ! to use 
            character(len=:), allocatable :: k_path
            real, dimension(2) :: r_path, x_path, m_path, p_init, p_fin 
            real :: kvalue
            integer :: idx, i
            logical :: is_found

            ! get json inputs
            call json%get('parameters.k.path', k_path, is_found); if (.not. is_found) call error_message('Verify yee inputs')

            ! initialization and allocate
            r_path = [0.0, 0.0]
            x_path = [PI / side_x, 0.0]
            m_path = [PI / side_x, PI / side_x]

            allocate(array_kvalues(k_npoints))

            kvalue = 0.0

            ! loop in k 
            do i = 1, k_npoints            
                array_kvalues(i) = kvalue

                idx = int(kvalue) + 1

                if (k_path(idx: idx) == 'r') then 
                    p_init = r_path 
                    p_fin = x_path 
                else if (k_path(idx: idx) == 'x') then 
                    p_init = x_path 
                    p_fin = m_path 
                else if (k_path(idx: idx) == 'm') then 
                    p_init = m_path 
                    p_fin = r_path
                else 
                    call error_message('Character unrecognized in k path')
                end if 

                if (kvalue < 1.0) then 
                    matrix_kvalues(i, :) = p_init + kvalue * (p_fin - p_init)
                else if ((kvalue >= 1.0) .and. (kvalue < 2.0)) then
                    matrix_kvalues(i, :) = p_init + (kvalue - 1) * (p_fin - p_init)
                else if ((kvalue >= 2.0) .and. (kvalue < 3.0)) then 
                    matrix_kvalues(i, :) = p_init + (kvalue - 2) * (p_fin - p_init)
                end if 

                kvalue = kvalue + (len(k_path) - 1.0) / k_npoints 

            end do


        end subroutine get_kvectors_yee

        ! Subroutine to calculate k vector based in k values in specific values
        !
        ! Inputs : 
        !   --- matrix_kvalues: matrix, where we going to save k vectors
        !   --- k_npoints: integer, number of values of k in the path
        !   --- array_kvalues: array, with k values to calculate k vectors
        !   
        ! Returns:
        !   --- matrix_kvalues: matrix, with values filled of kvectors

        subroutine get_kvectors_yee_specific_values(matrix_kvalues, k_npoints, array_kvalues)

            ! inputs
            integer, intent(in) :: k_npoints
            real, dimension(:), intent(in) :: array_kvalues

            ! outputs 
            real, dimension(:, :), intent(inout) :: matrix_kvalues

            ! to use 
            integer :: i 
            real, dimension(2) :: r_path, x_path, m_path, p_init, p_fin 
            real :: kvalue

            ! initialization
            r_path = [0.0, 0.0]
            x_path = [PI / side_x, 0.0]
            m_path = [PI / side_x, PI / side_x]
            kvalue = 0.0

            ! loop in k 
            do i = 1, k_npoints

                if (array_kvalues(i) < 1.0) then 
                    p_init = r_path 
                    p_fin = x_path

                    kvalue = array_kvalues(i)
                else if ((array_kvalues(i)  >= 1.0) .and. (array_kvalues(i)  < 2.0)) then
                    p_init = x_path 
                    p_fin = m_path 

                    kvalue = array_kvalues(i) - 1
                else if ((array_kvalues(i)  >= 2.0) .and. (array_kvalues(i)  < 3.0)) then 
                    p_init = m_path 
                    p_fin = r_path

                    kvalue = array_kvalues(i) - 2
                else
                    call error_message('k value invalid in specific_ks')
                end if 

                matrix_kvalues(i, :) = p_init + kvalue * (p_fin - p_init)

            end do

        end subroutine get_kvectors_yee_specific_values


        ! Subroutine to calculate tm modes
        !
        ! Inputs : 
        !   --- t: real, time value 
        !   --- omega: real, value calculate based in the frecuency
        !   --- mu_x: real, value of mu in x 
        !   --- mu_y: real, value of mu in y 
        !   --- matrix_eps_x: matrix, values of structure in x 
        !   --- matrix_eps_y: matrix, values of structure in y 
        !   --- rotate_x: complex, value of rotation in x 
        !   --- rotate_y: complex, value of rotation in y 
        !   
        ! Returns:
        !   --! overwrite matrices in x, y and z of the fields

        subroutine subjob_tm(t, omega, mu_x, mu_y, matrix_eps_x, matrix_eps_y, rotate_x, rotate_y)

            ! inputs
            real, intent(in) :: t, omega, mu_x, mu_y
            complex, intent(in) :: rotate_x, rotate_y 
            real, dimension(nx, ny), intent(in) :: matrix_eps_x, matrix_eps_y 

            ! initial excitement
            matrix_z(excitement_point(1), excitement_point(2)) = matrix_z(excitement_point(1), excitement_point(2)) + &
                        cmplx(&
                            exp(-(t - start_time) ** 2.0 / width) * amplitude * cos(omega * t), &
                            exp(-(t - start_time) ** 2.0 / width) * amplitude * sin(omega * t) &
                        ) 

            ! electric field 
            matrix_z(1, 1) = matrix_z(1, 1) + & 
                        matrix_eps_x(1, 1) * (matrix_y(1, 1) - matrix_y(nx, 1) / rotate_y) - &
                        matrix_eps_y(1, 1) * (matrix_x(1, 1) - matrix_x(1, ny) / rotate_x)

            matrix_z(2 : nx, 1) = matrix_z(2 : nx, 1) + &
                        matrix_eps_x(2 : nx, 1) * (matrix_y(2 : nx, 1) - matrix_y(1 : nx - 1, 1)) - & 
                        matrix_eps_y(2 : nx, 1) * (matrix_x(2 : nx, 1) - matrix_x(2 : nx, ny)/ rotate_x)

            matrix_z(1, 2 : ny) = matrix_z(1, 2 : ny) + & 
                        matrix_eps_x(1, 2 : ny) * (matrix_y(1, 2 : ny) - matrix_y(nx, 2 : ny) / rotate_y) - &
                        matrix_eps_y(1, 2 : ny) * (matrix_x(1, 2 : nx) - matrix_x(1, 1 : ny - 1))

            matrix_z(2 : nx, 2 : ny) = matrix_z(2 : nx, 2 : ny) +  &
                        matrix_eps_x(2 : nx, 2 : ny) * (matrix_y(2 : nx, 2 : ny) - matrix_y(1 : nx - 1, 2 : ny)) - & 
                        matrix_eps_y(2 : nx, 2 : ny) * (matrix_x(2 : nx, 2 : ny) - matrix_x(2 : nx, 1 : ny - 1))

            ! magnetic field 
            ! h_x 
            matrix_x(1 : nx, ny) = matrix_x(1 : nx, ny) - &
                        mu_y * (matrix_z(1 : nx, 1) * rotate_x - matrix_z(1 : nx, ny))
            matrix_x(1 : nx, 1 : ny - 1) = matrix_x(1 : nx, 1 : ny - 1) - & 
                        mu_y * (matrix_z(1 : nx, 2 : ny) - matrix_z(1 : nx, 1 : ny - 1))

            ! h_y 
            matrix_y(nx, 1 : ny) = matrix_y(nx, 1 : ny) + &
                        mu_x * (matrix_z(1, 1 : ny) * rotate_y - matrix_z(nx, 1 : ny))
            matrix_y(1 : nx - 1, 1 : ny) = matrix_y(1 : nx - 1, 1 : ny) + &
                        mu_x * (matrix_z(2 : nx, 1 : ny) - matrix_z(1 : nx - 1, 1 : ny))

        end subroutine subjob_tm

        ! Subroutine to calculate te modes
        !
        ! Inputs : 
        !   --- t: real, time value 
        !   --- omega: real, value calculate based in the frecuency
        !   --- mu_x: real, value of mu in x 
        !   --- mu_y: real, value of mu in y 
        !   --- matrix_eps_x: matrix, values of structure in x 
        !   --- matrix_eps_y: matrix, values of structure in y 
        !   --- rotate_x: complex, value of rotation in x 
        !   --- rotate_y: complex, value of rotation in y 
        !   
        ! Returns:
        !   --! overwrite matrices in x, y and z of the fields

        subroutine subjob_te(t, omega, mu_x, mu_y, matrix_eps_x, matrix_eps_y, rotate_x, rotate_y)

            ! inputs
            real, intent(in) :: t, omega, mu_x, mu_y
            complex, intent(in) :: rotate_x, rotate_y 
            real, dimension(nx, ny), intent(in) :: matrix_eps_x, matrix_eps_y 

            ! initial excitement
            matrix_z(excitement_point(1), excitement_point(2)) = matrix_z(excitement_point(1), excitement_point(2)) + &
                        cmplx(&
                            exp(-(t - start_time) ** 2.0 / width) * amplitude * cos(omega * t), &
                            exp(-(t - start_time) ** 2.0 / width) * amplitude * sin(omega * t) &
                        ) 

            ! magnetic field
            matrix_z(1 : nx - 1, 1 : ny - 1) = matrix_z(1 : nx - 1, 1 : ny - 1) - &
                        mu_x * (matrix_y(2 : nx, 1 : ny - 1) - matrix_y(1 : nx - 1, 1 : ny - 1)) + &
                        mu_y * (matrix_x(1 : nx - 1, 2 : ny) - matrix_x(1 : nx - 1, 1: ny - 1))

            matrix_z(nx, ny) = matrix_z(nx, ny) - &
                        mu_x * (matrix_y(1, ny) * rotate_y - matrix_y(nx, ny)) + &
                        mu_y * (matrix_x(nx, 1) * rotate_x - matrix_x(nx,ny))

            matrix_z(1 : nx - 1, ny) = matrix_z(1 : nx - 1, ny) - &
                        mu_x * (matrix_y(2 : nx, ny) - matrix_y(1 : nx - 1, ny)) + &
                        mu_y * (matrix_x(1 : nx - 1, 1) * rotate_x - matrix_x(1 : nx - 1, ny))

            matrix_z(nx, 1 : ny - 1) = matrix_z(nx, 1 : ny - 1) - &
                        mu_x * (matrix_y(1, 1 : ny - 1) * rotate_y - matrix_y(nx, 1 : ny - 1)) + &
                        mu_y * (matrix_x(nx, 2 : ny) - matrix_x(nx, 1 : ny - 1))
            

            ! electric field 
            ! e_x 
            matrix_x(1 : nx, 1) = matrix_x(1 : nx, 1) + &
                        matrix_eps_y(1 : nx, 1) * (matrix_z(1 : nx, 1) - matrix_z(1 : nx, ny) / rotate_x)
            matrix_x(1 : nx, 2 : ny) = matrix_x(1 : nx, 2 : ny) + &
                        matrix_eps_y(1 : nx, 2 : ny) * (matrix_z(1 : nx, 2 : ny) - matrix_z(1 : nx, 1 : ny - 1))

            ! e_y 
            matrix_y(1, 1 : ny) = matrix_y(1, 1 : ny) - &
                        matrix_eps_x(1, 1 : ny) * (matrix_z(1, 1 : ny) - matrix_z(nx, 1 : ny) / rotate_y)


            matrix_y(2 : nx, 1 : ny) = matrix_y(2 : nx, 1 : ny) - &
                        matrix_eps_x(2 : nx, 1 : ny) * (matrix_z(2 : nx, 1 : ny) - matrix_z(1 : nx - 1, 1 : ny))

        end subroutine subjob_te

end module yee