module handle_plots

    ! Module to handle the plots with gnuplot

    use :: handle_messages

    implicit none 

    private 

    public :: plot_structure, plot_xy, plot_gapmap

    contains 

        ! Subroutine to generate a png image with structure and gnuplot file to execute
        !
        ! Inputs: 
        !   --- path_file: string, with relative path of the file with data to plot
        !   --- path_plot: string, name to plot in png and file of gnuplot
        !
        ! Returns:
        !   --| file in png with structure and gnuplot file to execute and generate this image

        subroutine plot_structure(path_file, path_plot, rank_opt)

            character(len=*), intent(in) :: path_file, path_plot
            integer, optional :: rank_opt 
            integer rank

            if(present(rank_opt)) then 
                rank = rank_opt 
            else 
                rank = 0 
            end if

            ! open file plt (gnuplot file)
            open(100, file=path_plot//'.plt')

                ! write gnuplot configuration to plot structure
                write(100, *) 'set term png'
                write(100, *) "set output '"//trim(path_plot)//".png'"
                write(100, *) 'set pm3d'
                write(100, *) 'unset surface'
                write(100, *) 'set view map'
                write(100, *) 'set size ratio 1'
                write(100, *) 'set pm3d interpolate 0, 0'
                write(100, *) "splot '"//path_file//"' u 1:2:3"

            close(100)

            ! message of success plt file
            call success_message('Generated '//trim(path_plot)//'.plt', rank_opt=rank)

            ! execute gnuplot file to generate png image
            call execute_command_line("gnuplot -p '"//trim(path_plot)//".plt'")

            ! message of success png file
            call success_message('Generated '//trim(path_plot)//'.png', rank_opt=rank)

        end subroutine plot_structure

        ! Subroutine to generate a png image with xy plot and gnuplot file to execute
        !
        ! Inputs: 
        !   --- path_file: string, with relative path of the file with data to plot
        !   --- path_plot: string, name to plot in png and file of gnuplot
        !   --- xlim: array(2), limits in x-axis
        !   --- ylim: array(2), limits in y-axis
        !
        ! Returns:
        !   --| file in png with xy plot and gnuplot file to execute and generate this image

        subroutine plot_xy(path_file, path_plot, xlim_opt, ylim_opt)

            ! inputs
            character(len=*), intent(in) :: path_file, path_plot
            real, dimension(2), optional:: xlim_opt, ylim_opt

            ! open file plt (gnuplot file)
            open(100, file=path_plot//'.plt')

                ! write gnuplot configuration to plot structure
                write(100, *) 'set term png'
                write(100, *) "set output '"//trim(path_plot)//".png'"

                if (present(xlim_opt)) write(100, *) 'set xrange [', xlim_opt(1), ':', xlim_opt(2),']'
                if (present(ylim_opt)) write(100, *) 'set yrange [', ylim_opt(1), ':', ylim_opt(2),']'

                write(100, *) 'set grid'
                write(100, *) 'set ylabel "Frequency"'
                write(100, *) 'set xlabel "K Values"'

                write(100, *) "plot '"//path_file//"' with points pt 7"

            close(100)

            ! message of success plt file
            call success_message('Generated '//trim(path_plot)//'.plt')

            ! execute gnuplot file to generate png image
            call execute_command_line("gnuplot -p '"//trim(path_plot)//".plt'")

            ! message of success png file
            call success_message('Generated '//trim(path_plot)//'.png')

        end subroutine plot_xy


        ! Subroutine to generate a png image with gapmap plot and gnuplot file to execute
        !
        ! Inputs: 
        !   --- path_file: string, with relative path of the file with data to plot
        !   --- path_plot: string, name to plot in png and file of gnuplot
        !   --- max_frequency: real, superior limit in y-axis
        !
        ! Returns:
        !   --| file in png with gapmap plot and gnuplot file to execute and generate this image

        subroutine plot_gapmap(path_file, path_plot, max_frequency)

            ! inputs
            character(len=*), intent(in) :: path_file, path_plot
            real, intent(in) :: max_frequency

            ! open file plt (gnuplot file)
            open(100, file=path_plot//'.plt')

                ! write gnuplot configuration to plot structure
                write(100, *) 'set term png'
                write(100, *) "set output '"//trim(path_plot)//".png'"
                write(100, *) 'set xrange [0:1]'
                write(100, *) 'set yrange [0:', max_frequency,']'

                write(100, *) "plot '"//path_file//"' with filledcurves"

            close(100)

            ! message of success plt file
            call success_message('Generated '//trim(path_plot)//'.plt')

            ! execute gnuplot file to generate png image
            call execute_command_line("gnuplot -p '"//trim(path_plot)//".plt'")

            ! message of success png file
            call success_message('Generated '//trim(path_plot)//'.png')

        end subroutine plot_gapmap

end module handle_plots