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
		SIGMOID = 1. / (1. + ABS(x))
	CASE DEFAULT
		SIGMOID = 1. / (1. + EXP(-x))
END SELECT

RETURN
END FUNCTION SIGMOID

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


END MODULE NNETWORK