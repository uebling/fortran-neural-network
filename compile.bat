gfortran Parameters.f90 Nnetwork.f90 TrainData.f90 TestData.f90 -c -fdefault-real-8
gfortran FortranNN.f90 -o a.exe -fdefault-real-8 Parameters.o Nnetwork.o TrainData.o TestData.o

