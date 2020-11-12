module unit_cell

    ! Module to structure of circle in center of square

    use :: global_elements
    use :: statics 

    implicit none 

    private 

    public :: get_center_unit_cell

    contains 

        ! Function to get centers of unit cell 
        ! Inputs 
        !   --- source_data_opt: string, at what level should we get the inputs?
        !   --- a_opt: real, red constant, x-axis
        !   --- b_opt: real, red constant, y-axis
        !
        ! Results
        !   --- centeres, array(1, 5), with shape_param, valid_points, permittivity, x, y

        function get_center_unit_cell(source_data_opt, a_opt, b_opt, ff_opt) result(centers)

            ! inputs 
            character(len=*), optional :: source_data_opt
            real, optional :: a_opt, b_opt, ff_opt

            ! output 
            real, dimension(1, 6) :: centers

            ! to use
            character(len=:), allocatable :: source_data, structure_type
            real :: a, b, permittivity, shape_param, valid_points_param, angrot
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

            ! get permittivity and structure type
            call json%get('structure'//source_data//'.eps_1', permittivity, is_found); if(.not. is_found) inputs_right = .false.
            call json%get('structure'//source_data//'.type', structure_type, is_found); if(.not. is_found) inputs_right = .false.

            ! get radio or side
            if (structure_type == 'circle_center') then 

                if (present(ff_opt)) then 
                    shape_param = sqrt(ff_opt * a * b / PI)
                else 

                    call json%get('structure'//source_data//'.radio', shape_param, is_found)

                    ! if not radio, verify factor of radio 
                    if(.not. is_found) then 
                        call json%get('structure'//source_data//'.factor_radio', shape_param, is_found)
                        if (.not. is_found) inputs_right = .false.

                        shape_param = shape_param * a 

                    end if 

                end if

                ! valid point to the structure
                valid_points_param = 1

            else if (structure_type == 'regular_polygon_center') then

                ! get number of sides   
                call json%get('structure'//source_data//'.nsides', valid_points_param, is_found)
                if(.not. is_found) inputs_right = .false.

                ! get side
                if (present(ff_opt)) then 

                    shape_param = sqrt(4 * ff_opt * a * b * tan(PI / valid_points_param) / valid_points_param)

                else 

                    call json%get('structure'//source_data//'.side', shape_param, is_found)
                    
                    ! if not side, verify factor side
                    if(.not. is_found) then 
                        call json%get('structure'//source_data//'.factor_side', shape_param, is_found)
                        if(.not. is_found) inputs_right = .false.

                        shape_param = shape_param * a

                    end if 

                end if 

                ! get angle of rotation 
                call json%get('structure'//source_data//'.polygon_angle_rotation', angrot, is_found)
                if(.not. is_found) angrot = 0

            end if 

            ! verify inputs json 
            if (.not. inputs_right) call error_message('Verify structure inputs')

            ! get centers 
            centers(1, 1) = shape_param 
            centers(1, 2) = valid_points_param
            centers(1, 3) = permittivity
            centers(1, 4) = a / 2.0 
            centers(1, 5) = b / 2.0 
            centers(1, 6) = angrot * PI / 180
             

        end function get_center_unit_cell

end module unit_cell