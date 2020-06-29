PROGRAM NeuralNetwork
	USE TrainDataModule
	USE PARAMETERS
	USE NNETWORK
	IMPLICIT NONE
	INTEGER :: j
	REAL(8), DIMENSION(1:n_outputs) :: result, truevalue

!First generate the training data
	CALL Fill_Data
!Then the initial weights and biases
	CALL InitializeWB
!I now use columns 2 and 3 as input
!subtract avaerga
	traindata(:,2)=traindata(:,2)-SUM(traindata(:,2)/REAL(n_data))
	traindata(:,3)=traindata(:,3)-SUM(traindata(:,3)/REAL(n_data))



	DO j = 1,n_data
		truevalue = traindata(j,4)
		WRITE(*,*) "Row of data: ",j
		CALL FeedForward([traindata(j,2),traindata(j,3)],result)

		WRITE(*,*) "Output: ",result(:), "True value: ",truevalue
		WRITE(*,*) "Loss (MSE):", LOSS_MSE(result,truevalue)
	END DO
END PROGRAM NeuralNetwork


