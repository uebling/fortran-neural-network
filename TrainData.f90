MODULE TrainDataModule
	USE Parameters
	IMPLICIT NONE
	REAL(8), DIMENSION(1:n_data,1:n_cols) :: traindata

	CONTAINS

	SUBROUTINE Fill_Data
	IMPLICIT NONE
	INTEGER :: j

! Colums are Gender (f = 1., m = 0.,)
! Average hours of sleep per day
! weight
! Human (1.) or cat (0.) 

	traindata(1,:) = [1.,6.5,61.,1.] ! Lucy
	traindata(2,:) = [0.,21.,5.1,0.] ! Ollie
	traindata(3,:) = [0.,7.2,88.,1.] ! Joe
	traindata(4,:) = [0.,6.1,71.,1.] ! Fritz
	traindata(5,:) = [1.,16.,3.5,0.] ! Mauzi
	traindata(6,:) = [0.,12.,7.,0.] ! Ratti
	traindata(7,:) = [1.,7.3,66.,1.] ! Mary
	traindata(8,:) = [0.,6.1,103.,1.] ! Peter

	!Write the data to see if it works
	WRITE(*,*) "This is the training data"
	WRITE(*,*) "Gender, sleep, weight, species"
	DO j = 1,n_data
		WRITE(*,*) traindata(j,:)
	END DO
	WRITE(*,*)

END SUBROUTINE Fill_Data

END MODULE TrainDataModule