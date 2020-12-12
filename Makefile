# variables
COMPILER=mpif90
JF_LIBRARIES=-L./statics/json_fortran/lib 
JF_INCLUDE=-I./statics/json_fortran/include/ 
JF_EXTRA=./statics/json_fortran/lib/libjsonfortran.a


# compile and generate output executable
all : photonics.exe clean 

photonics.exe: photonics.f90 global_elements.o centers_functions.o fill_matrix_functions.o algorithms.o
		$(COMPILER) $(JF_INCLUDE) $(JF_LIBRARIES) -o photonics.exe photonics.f90 \
			centers_functions.o unit_cell.o bravais_moire.o from_files.o patterns.o \
			fill_matrix_functions.o fill_matrix_circles.o fill_matrix_regular_polygons.o \
			statics.o handle_messages.o handle_strings.o save_files.o handle_plots.o constants.o fast_fourier_transform.o rotations.o math_utils.o pattern_structures.o \
			global_elements.o \
			algorithms.o yee.o yee_gapmap.o \
			$(JF_EXTRA)

algorithms.o: algorithms/algorithms.f90 yee.o yee_gapmap.o 
		$(COMPILER) -c algorithms/algorithms.f90 

yee.o: algorithms/yee/yee.f90 statics.o 
		$(COMPILER) -c algorithms/yee/yee.f90

yee_gapmap.o: algorithms/yee_gapmap/yee_gapmap.f90 statics.o yee.o global_elements.o centers_functions.o fill_matrix_functions.o
		$(COMPILER) -c algorithms/yee_gapmap/yee_gapmap.f90

global_elements.o: global_elements.f90 statics.o 
		$(COMPILER) -c global_elements.f90

fill_matrix_functions.o: structures/fill_matrix/fill_matrix_functions.f90 fill_matrix_circles.o fill_matrix_regular_polygons.o global_elements.o
		$(COMPILER) -c structures/fill_matrix/fill_matrix_functions.f90

fill_matrix_circles.o: structures/fill_matrix/circles/fill_matrix_circles.f90  statics.o global_elements.o
		$(COMPILER) -c structures/fill_matrix/circles/fill_matrix_circles.f90 

fill_matrix_regular_polygons.o: structures/fill_matrix/regular_polygons/fill_matrix_regular_polygons.f90 statics.o global_elements.o 
		$(COMPILER) -c structures/fill_matrix/regular_polygons/fill_matrix_regular_polygons.f90

statics.o: statics/statics.f90 handle_messages.o handle_strings.o save_files.o handle_plots.o constants.o math_utils.o fast_fourier_transform.o rotations.o pattern_structures.o
		$(COMPILER) $(JF_INCLUDE) $(JF_LIBRARIES) -c statics/statics.f90 

save_files.o: statics/save_files/save_files.f90 
		$(COMPILER) -c statics/save_files/save_files.f90

handle_messages.o: statics/handle_messages/handle_messages.f90
		$(COMPILER) -c statics/handle_messages/handle_messages.f90 

handle_strings.o: statics/handle_strings/handle_strings.f90 
		$(COMPILER) -c statics/handle_strings/handle_strings.f90 

handle_plots.o: statics/handle_plots/handle_plots.f90 
		$(COMPILER) -c statics/handle_plots/handle_plots.f90

constants.o: statics/constants/constants.f90 
		$(COMPILER) -c statics/constants/constants.f90 

rotations.o: statics/transforms/rotations/rotations.f90 
		$(COMPILER) -c statics/transforms/rotations/rotations.f90 

fast_fourier_transform.o: statics/transforms/fast_fourier_transform/fast_fourier_transform.f90 
		$(COMPILER) -c statics/transforms/fast_fourier_transform/fast_fourier_transform.f90 

math_utils.o: statics/math_utils/math_utils.f90 
		$(COMPILER) -c statics/math_utils/math_utils.f90

pattern_structures.o: statics/pattern_structures/pattern_structures.f90 
		$(COMPILER) -c statics/pattern_structures/pattern_structures.f90

centers_functions.o: structures/centers/centers_functions.f90 unit_cell.o bravais_moire.o from_files.o patterns.o
		$(COMPILER) -c structures/centers/centers_functions.f90

unit_cell.o: structures/centers/unit_cell/unit_cell.f90 statics.o
		$(COMPILER) -c structures/centers/unit_cell/unit_cell.f90 

bravais_moire.o: structures/centers/bravais_moire/bravais_moire.f90 statics.o 
		$(COMPILER) -c structures/centers/bravais_moire/bravais_moire.f90 

from_files.o: structures/centers/from_files/from_files.f90 statics.o 
		$(COMPILER) -c structures/centers/from_files/from_files.f90 

patterns.o: structures/centers/patterns/patterns.f90 statics.o 
		$(COMPILER) -c structures/centers/patterns/patterns.f90

# install json-fortran
install_json_fortran: 
		chmod +x jf_install.sh
		./jf_install.sh 

# clean, remove all .o and .mod of all folders
clean: 
		find . -path ./statics/json_fortran -prune -false -o -name "*.mod" | xargs rm -f
		rm  *.o
		

# delete all of wdir and .outs
purge:
		rm -rf wdir/* *.out