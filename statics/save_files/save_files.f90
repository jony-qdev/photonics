module save_files

    ! Module to handle saved of files

    use :: handle_messages

    implicit none 

    private 

    public :: save_structure, save_xy_plot, save_gapmap_plot

    contains 

        ! Subroutine to save structure given a matrix
        !
        ! Inputs:
        !   --- path_file: string, path where the file will be saved
        !   --- matrix: matrix, with data of the structure
        !   --- nx: integer, number of rows of the matrix
        !   --- ny: integer, number of columns of the matrix
        !   --- dx: real, step in the x-axis of the structure
        !   --- dy: real, step in the y-axis of the structure
        !   --- space_each_x_opt: logical, optional, if true one whitespace is added for each x
        !
        ! Returns:
        !   --| file with structure with columns --> i*dx, j*dy, matrix(i, j)

        subroutine save_structure(path_file, matrix, nx, ny, dx, dy, space_each_x_opt)

            ! inputs
            character(len=*), intent(in) :: path_file 
            real, dimension(:, :), intent(in) :: matrix 
            real, intent(in) :: dx, dy
            integer, intent(in) :: nx, ny 
            logical, optional :: space_each_x_opt

            ! variables to use
            integer :: i, j

            ! open file 
            open(100, file=path_file)

                ! traverse matrix and save in the file
                do i = 1, nx 
                    do j = 1, ny 

                        write(100, *) i * dx, j * dy, matrix(i, j)

                    end do 

                    if (present(space_each_x_opt)) write(100, *) ''

                end do

            close(100)

            call success_message('Generated '//path_file)

        end subroutine save_structure


        ! Subroutine to save xy plot given matrix
        !
        ! Inputs:
        !   --- path_file: string, path where the file will be saved
        !   --- matrix: matrix, with data of the xy plot
        !   --- rows: integer, number of points in the xy plot 
        !
        ! Returns:
        !   --| file with structure with columns --> x, y

        subroutine save_xy_plot(path_file, matrix, rows, add_x_opt)

            ! inputs
            character(len=*), intent(in) :: path_file 
            real, dimension(:, :), intent(in) :: matrix
            integer, intent(in) :: rows 
            real, optional :: add_x_opt

            ! variables to use
            integer :: i
            real :: add_x

            ! verify add x
            if (present(add_x_opt)) then 
                add_x = add_x_opt 
            else 
                add_x = 0.0 
            end if

            ! open file 
            open(100, file=path_file)

                ! traverse matrix and save in the file
                do i = 1, rows 

                    write(100, *) matrix(i, 1) + add_x, matrix(i, 2)

                end do

            close(100)

            call success_message('Generated '//path_file)

        end subroutine save_xy_plot


        ! Subroutine to save gapmap plot given matrix
        !
        ! Inputs:
        !   --- path_file: string, path where the file will be saved
        !   --- matrix: matrix, with data of the xy plot
        !   --- rows: integer, number of points in the xy plot 
        !
        ! Returns:
        !   --| file with structure with columns --> filling_fraction, maxvalue, minvalue

        subroutine save_gapmap_plot(path_file, matrix, rows)

            ! inputs
            character(len=*), intent(in) :: path_file 
            real, dimension(:, :), intent(in) :: matrix
            integer, intent(in) :: rows

            ! variables to use
            integer :: i, j, ngaps

            ! view number of gaps
            ngaps = nint(maxval(matrix(:, 1)))

            ! open file 
            open(100, file=path_file)

                ! traverse matrix and save in the file

                do j = 1, ngaps
                    do i = 1, rows 
                        if (nint(matrix(i, 1)) == j) then 
                            write(100, *) matrix(i, 2 :)
                        end if 
                    end do

                    write(100, *) ''

                end do

            close(100)

            call success_message('Generated '//path_file)

        end subroutine save_gapmap_plot

end module save_files