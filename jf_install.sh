mkdir -p wdir 
mkdir -p statics
git clone https://github.com/jacobwilliams/json-fortran.git
cd json-fortran/
mkdir build && cd build/
cmake -DCMAKE_Fortran_COMPILER=/export/apps/gcc-10.1.0/bin/gfortran ..
make 
cd ../..
mkdir statics/json_fortran 
mv json-fortran/build/* statics/json_fortran 
rm -rf json-fortran