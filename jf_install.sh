mkdir -p wdir 
mkdir -p statics
git clone https://github.com/jacobwilliams/json-fortran.git
cd json-fortran/
mkdir build && cd build/
cmake ..
make 
cd ../..
mkdir statics/json_fortran 
mv json-fortran/build/* statics/json_fortran 
rm -rf json-fortran