module rotations

    ! Module with functions to get rotation matrices

    implicit none 

    private 

    public :: get_matrix_rotation_2d

    contains

        ! Function to generate a rotation matrix given a angle
        !
        ! Inputs:
        !   --- angle: real, angle of rotation
        !
        ! Returns:
        !   --- rot: matrix, matrix 2x2 to rotate a vector 2d in an angle given

        function get_matrix_rotation_2d(angle) result(rot)

            ! inputs
            real, intent(in) :: angle 

            ! outputs
            real, dimension(2, 2) :: rot 

            ! calculate rotation matrix
            rot(1, 1) = cos(angle)
            rot(1, 2) = -sin(angle)
            rot(2, 1) = sin(angle)
            rot(2, 2) = cos(angle)

        end function get_matrix_rotation_2d

end module rotations