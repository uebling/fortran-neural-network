PROGRAM NeuralNetwork
	USE TrainDataModule
	USE PARAMETERS
	USE NNETWORK
	IMPLICIT NONE
	INTEGER :: j
	REAL(8), DIMENSION(1:n_inputs) :: traindata,avg
	REAL(8), DIMENSION(1:n_outputs) :: result, truevalue

!First generate the training data
	CALL Fill_Data
!Then the initial weights and biases
	CALL InitializeWB
!I now use columns 2 and 3 as input
!get average
	DO j = 1, n_inputs
		avg(j) = SUM(fulldata(:,traincolumns(j))/REAL(n_data))
	END DO



	DO j = 1,n_data

		traindata(:) = fulldata(j,traincolumns(:))-avg(:)
		truevalue = fulldata(j,truecolumn)
		WRITE(*,*) "Row of data: ",j
		CALL FeedForward(traindata(:),result)

		WRITE(*,*) "Output: ",result(:), "True value: ",truevalue
		WRITE(*,*) "Loss (MSE):", LOSS_MSE(result,truevalue)
	END DO
END PROGRAM NeuralNetwork


