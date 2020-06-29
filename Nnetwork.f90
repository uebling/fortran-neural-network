MODULE NNETWORK
USE PARAMETERS
IMPLICIT NONE

CONTAINS

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
		SIGMOID = x / (1. + ABS(x))
	CASE DEFAULT
		SIGMOID = 1. / (1. + EXP(-x))
END SELECT

RETURN
END FUNCTION SIGMOID


REAL(8) FUNCTION SIGMOID_DERIV(x,number)
USE Parameters
IMPLICIT NONE
REAL(8), INTENT(IN) :: x
INTEGER, INTENT(IN) :: number

SELECT CASE (number)
	CASE (1)
		SIGMOID_DERIV = EXP(-x)/(1+EXP(-x))**2
	CASE (2)
		SIGMOID_DERIV = EXP(-0.25*pi*x**2)
	CASE (3)
		SIGMOID_DERIV = 1. / (1. + x**2)**(1.5)
	CASE (4)
		SIGMOID_DERIV = 1. / COSH(x)**2
	CASE (5)
		SIGMOID_DERIV = 1. / (1. + (0.5*pi*x)**2)
	CASE (6)
		SIGMOID_DERIV = 1. / (1. + ABS(x))**2
	CASE DEFAULT
		SIGMOID_DERIV = EXP(-x)/(1+EXP(-x))**2
END SELECT

RETURN
END FUNCTION SIGMOID_DERIV


REAL(8) FUNCTION LOSS_MSE(out_pred,out_true)
IMPLICIT NONE
REAL(8), DIMENSION(:), INTENT(IN) :: out_pred,out_true
INTEGER :: len

len = SIZE(out_pred)
IF (len.ne.SIZE(out_true)) WRITE(*,*) "Warning: Loss function compares arrays of different size"

LOSS_MSE = SUM(out_true**2-out_pred**2)/REAL(len)

RETURN
END FUNCTION LOSS_MSE

REAL(8) FUNCTION NEURON(insize,input,weights,bias)
IMPLICIT NONE
INTEGER :: insize
REAL(8) :: bias,tmp
REAL(8), DIMENSION(1:insize) :: input, weights

tmp = SUM(input(:) * weights(:)) + bias

NEURON = SIGMOID(tmp,1)

RETURN
END FUNCTION NEURON


SUBROUTINE FeedForward(input,output)
	USE PARAMETERS
	IMPLICIT NONE
	REAL(8), DIMENSION(1:n_inputs), INTENT(IN) :: input
	REAL(8), DIMENSION(1:n_outputs), INTENT(OUT) :: output
	REAL(8), DIMENSION(1:n_neurons) :: hidden,h2
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

END MODULE NNETWORK