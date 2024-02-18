build_fort() {
   gfortran -L/usr/local/lib -I/usr/local/include -I../fortran-raylib $@ ../fortran-raylib/libfortran-raylib.a -lraylib -framework OpenGL -framework Cocoa -framework IOKit -framework CoreAudio -framework CoreVideo -fno-range-check -ffree-line-length-none
}

build_fort src/class_tetromino.f90 src/main.f90
