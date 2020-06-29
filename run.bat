gfortran Parameters.f90 -c -fdefault-real-8
gfortran Nnetwork.f90 -c -fdefault-real-8
gfortran TrainData.f90 -c -fdefault-real-8
gfortran FortranNN.f90 -o a.exe -fdefault-real-8 Parameters.o Nnetwork.o TrainData.o
a