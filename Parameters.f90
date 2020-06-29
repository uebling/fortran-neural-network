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
