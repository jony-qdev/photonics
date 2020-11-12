module from_files 

    ! Module to structure of centers from files in center of square

    use :: global_elements
    use :: statics 

    implicit none 

    private 

    public :: get_centers_from_files

    contains 

        ! Function to get centers from files
        ! Inputs 
        !   --- source_data_opt: string, at what level should we get the inputs?
        !   --- a_opt: real, red constant, x-axis
        !   --- b_opt: real, red constant, y-axis
        !
        ! Results
        !   --- centeres, array(total_centers, 5), with shape_param, valid_points, permittivity, x, y

        function get_centers_from_files(source_data_opt, a_opt, b_opt, ff_opt) result(centers)

            ! inputs 
            character(len=*), optional :: source_data_opt
            real, optional :: a_opt, b_opt, ff_opt

            ! output 
            real, dimension(:, :), allocatable :: centers

            ! to use
            character(len=:), allocatable :: source_data
            character(len=125), dimension(:), allocatable :: files
            real, dimension(:), allocatable :: permitivities, shape_params, factors, angles_rotation
            real :: a, b
            integer, dimension(:), allocatable :: valid_points_params, ncenters
            integer :: i,j, k, io, total_centers
            logical :: is_found, inputs_right

            ! initialization
            inputs_right = .true.

            ! verify source of data
            if (present(source_data_opt)) then 
                source_data = '.'//source_data_opt

                ! get parameters
                a = a_opt 
                b = b_opt 

            else 
                source_data = ''

                ! get parameters
                call json%get('structure.a', a, is_found); if(.not. is_found) inputs_right = .false.
                call json%get('structure.b', b, is_found); if(.not. is_found) b = a

                ! save global variables
                side_x = a 
                side_y = b
                nx = ceiling(side_x / (lambda/ nr))
                ny = ceiling(side_y / (lambda/ nr))
                dx = side_x / nx 
                dy = side_y / ny 

            end if

            ! get permitivities, structure types and name files
            call json%get('structure'//source_data//'.eps_1', permitivities, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.files', files, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.angles_rotation', angles_rotation, is_found); 
                if(.not. is_found) angles_rotation = [(0.0, i = 1, size(files))]
            call json%get('structure'//source_data//'.nsides', valid_points_params, is_found)
                if(.not. is_found) inputs_right = .false.

            ! get sides, radios and factors
            if (.not. present(ff_opt)) then 
                call json%get('structure'//source_data//'.sides/radios', shape_params, is_found)
        
                ! if not sides/radios, verify factors
                if(.not. is_found) then 
                    shape_params = [(0.0, i = 1, size(files))]

                    call json%get('structure'//source_data//'.factors', factors, is_found)
                    if (.not. is_found) call error_message('sides/radios or factors is required')

                else 
                    
                    call json%get('structure'//source_data//'.factors', factors, is_found)
                    if (.not. is_found) factors = [(0.0, i = 1, size(files))]

                end if 

                ! update params
                factors = factors * a 
                shape_params = shape_params + factors

            end if 

            ! verify inputs json 
            if (.not. inputs_right) call error_message('Verify structure inputs')
            
            do i = 1, size(files) 
                if (valid_points_params(i) == 0) valid_points_params(i) = 1
            end do 

            ! allocate
            allocate(ncenters(size(files)))

            ! get number of centers 
            total_centers = 0

            do i = 1, size(files)

                ! count centers of the current file
                open(100, file=files(i), status='old')
                do 
                    read(100, *, iostat=io)

                    if (io /= 0) exit 
                    total_centers = total_centers + 1
                end do
                close(100)

                if (i == 1) then 
                    ncenters(i) = total_centers
                else 
                    ncenters(i) = total_centers - ncenters(i - 1)
                end if 

            end do  

            ! if ff_opt 
            if (present(ff_opt)) then 
                ! allocate
                allocate(shape_params(size(files)))

                do i = 1, size(files)
                    ! to circles
                    shape_params(i) = sqrt(ff_opt * a * b / (total_centers * PI))
                    
                    ! to regular polygons
                    if (valid_points_params(i) > 1) shape_params(i) = 2.0 * shape_params(i) * sin(PI / valid_points_params(i))

                end do 
            end if 

            ! allocate 
            allocate(centers(total_centers, 6))

            ! init count of centers
            k = 1

            ! fill matrix centers 
            do i = 1, size(files)

                open(100, file=files(i), status='old')
                do j = 1, ncenters(i)

                    centers(k, 1) = shape_params(i)
                    centers(k, 2) = valid_points_params(i)
                    centers(k, 3) = permitivities(i)
                    
                    read(100, *) centers(k, 4:5)

                    centers(k, 6) = angles_rotation(i) * PI / 180

                    k = k + 1

                end do
                close(100)

            end do 

            ! deallocate 
            if (.not. present(ff_opt)) deallocate(factors)
            deallocate(files, permitivities, shape_params, valid_points_params, ncenters)

        end function get_centers_from_files

end module from_files 