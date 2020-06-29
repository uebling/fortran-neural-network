MODULE Parameters
	!Parameters and global variables
	IMPLICIT NONE
	REAL(8), PARAMETER :: pi = 4.*ATAN(1.)
	!Number of entries in the training data
	INTEGER, PARAMETER :: n_data = 8
	!Number of columns in each entry 
	INTEGER, PARAMETER :: n_cols = 4
	!Number of hidden layers
	INTEGER, PARAMETER :: n_hidden = 1
	!Number of neurons in the hidden layers
	INTEGER, PARAMETER :: n_neurons = 2
	!Number of inputs per neurons
	INTEGER, PARAMETER :: n_inputs = 2
	!Number of output neurons
	INTEGER, PARAMETER :: n_outputs = 1
	!weights and biases
	REAL(8), DIMENSION(1:n_hidden,1:n_neurons,1:n_inputs) :: weight
	REAL(8), DIMENSION(1:n_hidden,1:n_neurons) :: bias
	REAL(8), DIMENSION(1:n_outputs,1:n_inputs) :: weight_out 
	REAL(8), DIMENSION(1:n_outputs) :: bias_out
END MODULE Parameters

MODULE TrainDataModule
	USE Parameters
	IMPLICIT NONE
	REAL(8), DIMENSION(1:n_data,1:n_cols) :: traindata
END MODULE TrainDataModule

PROGRAM NeuralNetwork
	USE TrainDataModule
	IMPLICIT NONE
	INTEGER :: j
	REAL(8), DIMENSION(1:n_outputs) :: result
	REAL(8) :: SIGMOID

!First generate the training data
	CALL Fill_Data
!Then the initial weights and biases
	CALL InitializeWB
!I now use columns 2 and 3 as input
!subtract avaerga
	traindata(:,2)=traindata(:,2)-SUM(traindata(:,2)/REAL(n_data))
	traindata(:,3)=traindata(:,3)-SUM(traindata(:,3)/REAL(n_data))



	DO j = 1,n_data
		WRITE(*,*) "Row of data: ",j
		CALL FeedForward([traindata(j,2),traindata(j,3)],result)

		WRITE(*,*) "Output: ",result(:)
		WRITE(*,*)
	END DO
END PROGRAM NeuralNetwork


SUBROUTINE FeedForward(input,output)
	USE PARAMETERS
	IMPLICIT NONE
	REAL(8), DIMENSION(1:n_inputs), INTENT(IN) :: input
	REAL(8), DIMENSION(1:n_outputs), INTENT(OUT) :: output
	REAL(8), DIMENSION(1:n_neurons) :: hidden,h2
	REAL(8) :: NEURON
	INTEGER :: j,k

	output = 0.

	DO j = 1,n_inputs
		WRITE(*,*) input(j)
	END DO

	hidden = input
!Now loop over hidden layers and neurons
	DO j = 1,n_hidden
		DO k = 1,n_neurons
			h2(k) = NEURON(n_inputs,hidden,weight(j,k,:),bias(j,k))
		END DO
		hidden=h2
	END DO
	DO j = 1,n_outputs
		output(j) = NEURON(n_inputs,hidden,weight_out(j,:),bias_out(j))
	END DO
END SUBROUTINE FeedForward





SUBROUTINE Fill_Data
	USE TrainDataModule
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

SUBROUTINE InitializeWB
	USE PARAMETERS
	IMPLICIT NONE
	INTEGER :: j,k,l

	WRITE(*,*) "Initial weights and biases"
	DO j = 1,n_hidden
		DO k=1,n_neurons
			DO l=1,n_inputs
				weight(j,k,l) = Real(l-1)/REAL(n_inputs-1)
				WRITE(*,*) j,k,l,weight(j,k,l)
			END DO
			bias(j,k) = 0.1
		END DO
	END DO
	WRITE(*,*)

	WRITE(*,*) "Output layer"
	DO j = 1,n_outputs
		DO k = 1,n_inputs
			weight_out(j,k) = Real(k-1)/REAL(n_inputs-1)
			WRITE(*,*) j,k,weight_out(j,k)
		END DO
		bias_out(j) = 0.2
	END DO

END SUBROUTINE InitializeWB

REAL(8) FUNCTION NEURON(insize,input,weights,bias)
IMPLICIT NONE
INTEGER :: insize
REAL(8) :: bias,tmp,SIGMOID
REAL(8), DIMENSION(1:insize) :: input, weights


tmp = 0.
tmp = SUM(input(:) * weights(:)) + bias

NEURON = SIGMOID(tmp,1)


RETURN
END FUNCTION NEURON

REAL(8) FUNCTION SIGMOID(x,number)
USE Parameters
IMPLICIT NONE
REAL(8), INTENT(IN) :: x
INTEGER, INTENT(IN) :: number

SELECT CASE (number)
	CASE (1)
		SIGMOID = 1. / (1. + EXP(-x))
	CASE (2)
		SIGMOID = ERF(0.5*SQRT(pi)*x)
	CASE (3)
		SIGMOID = x / SQRT(1. + x**2)
	CASE (4)
		SIGMOID = TANH(x)
	CASE (5)
		SIGMOID = 2.*ATAN(0.5*pi*x)/pi
	CASE (6)
		SIGMOID = 1. / (1. + ABS(x))
	CASE DEFAULT
		SIGMOID = 1. / (1. + EXP(-x))
END SELECT

RETURN
END FUNCTION SIGMOID
