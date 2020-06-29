PROGRAM NeuralNetwork
	USE TrainDataModule
	USE PARAMETERS
	USE NNETWORK
	IMPLICIT NONE
	INTEGER :: j
	REAL(8), DIMENSION(1:n_outputs) :: result

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
