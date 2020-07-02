MODULE TestDataModule
	USE Parameters
	IMPLICIT NONE
	REAL(8), DIMENSION(1:n_test,1:n_cols) :: testdata

	CONTAINS

	SUBROUTINE Get_Test_Data
		IMPLICIT NONE
		INTEGER :: j

! Colums are Gender (f = 1., m = 0.,)
! Average hours of sleep per day
! weight
! Human (1.) or cat (0.) 

		testdata(1,:) = [0.,7.9,89.,1.] !Otto 
		testdata(2,:) = [0.,6.3,111.,1.] !James 
		testdata(3,:) = [1.,5.5,78.,1.] !Claire 
		testdata(4,:) = [1.,12.3,4.8,0.] !Mimi
		testdata(5,:) = [0.,16.9,5.8,0.] !Mr. Bigglesworth 
		testdata(6,:) = [0.,14.6,5.1,0.] !Charlie
		testdata(7,:) = [1.,8.1,76.,1.] !Anne
		testdata(8,:) = [1.,19.7,3.7,0.] !Mitzi

	END SUBROUTINE Get_Test_Data

END MODULE TestDataModule