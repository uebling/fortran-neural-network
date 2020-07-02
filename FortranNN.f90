PROGRAM NeuralNetwork
    USE TrainDataModule
    USE TestDataModule
    USE PARAMETERS
    USE NNETWORK
    IMPLICIT NONE
    INTEGER :: j, epoch
    REAL(8), DIMENSION(1:n_outputs) :: result, truevalue
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons) :: hidden
    REAL(8), DIMENSION(1:n_inputs) :: traindata
    REAL(8) :: TotalLoss

!First generate the training data
    CALL Fill_Data
!Then the initial weights and biases
    CALL InitializeWB
!I now use columns 2 and 3 as input
!get average, to later subtract it from the data, to get it centred
    DO j = 1, n_inputs
        avg(j) = SUM(fulldata(:, traincolumns(j))/REAL(n_data))
    END DO

!        CALL Simple_FeedForward(result)

    DO epoch = 1, n_epochs
        TotalLoss = 0.
        DO j = 1, n_data
            traindata(:) = fulldata(j, traincolumns(:)) - avg(:)
            truevalue = fulldata(j, truecolumn)
!                        WRITE(*,*) "Row of data: ",j
            CALL FeedForward(traindata, hidden, result)
!                        WRITE(*,*) "Output: ",result(:), "True value: ",truevalue
!                        WRITE(*,*) "Loss (MSE):", LOSS_MSE(result,truevalue)
            TotalLoss = TotalLoss + LOSS_MSE(result, truevalue)

            CALL BackProp(traindata, hidden, result, truevalue)

        END DO
        TotalLoss = TotalLoss/REAL(n_data)
        WRITE (*, *) "Total Loss in Epoch", epoch, ":", TotalLoss
    END DO

    CALL TestNN

END PROGRAM NeuralNetwork

SUBROUTINE TestNN
    USE TrainDataModule
    USE TestDataModule
    USE PARAMETERS
    USE NNETWORK
    IMPLICIT NONE
    INTEGER :: j
    REAL(8), DIMENSION(1:n_outputs) :: result
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons) :: hidden

    CALL Get_Test_Data

    DO j = 1, n_test
        CALL FeedForward(testdata(j, traincolumns(:)), hidden, result)
        WRITE (*, *) "NN guess: ", result(:), "True value: ", testdata(j, truecolumn)
    END DO

END SUBROUTINE TestNN

SUBROUTINE Simple_FeedForward(output)
    USE TrainDataModule
    USE PARAMETERS
    USE NNETWORK
    IMPLICIT NONE
    INTEGER :: j
    REAL(8), DIMENSION(1:n_outputs), INTENT(OUT) :: output
    REAL(8), DIMENSION(1:n_inputs) :: traindata
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons) :: hidden
    REAL(8), DIMENSION(1:n_outputs) :: truevalue

    DO j = 1, n_data
        traindata(:) = fulldata(j, traincolumns(:)) - avg(:)
        truevalue = fulldata(j, truecolumn)
        WRITE (*, *) "Row of data: ", j
        CALL FeedForward(traindata, hidden, output)

        WRITE (*, *) "Output: ", output(:), "True value: ", truevalue
        WRITE (*, *) "Loss (MSE):", LOSS_MSE(output, truevalue)
    END DO

END SUBROUTINE Simple_FeedForward

SUBROUTINE BackProp(traindata, hidden, output, truevalue)
    USE TrainDataModule
    USE PARAMETERS
    USE NNETWORK
    IMPLICIT NONE
    INTEGER :: j, k, l, m
    REAL(8), DIMENSION(1:n_inputs), INTENT(IN) :: traindata
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons), INTENT(IN) :: hidden
    REAL(8), DIMENSION(1:n_outputs), INTENT(IN) :: truevalue, output
    REAL(8), DIMENSION(0:n_hidden, 1:n_neurons) :: h2
    REAL(8), DIMENSION(1:n_outputs) :: dL_do, do_db, dL_dbo
    REAL(8), DIMENSION(1:n_outputs, 1:n_neurons) :: do_dh, do_dw, dL_dwo
    REAL(8), DIMENSION(1:n_neurons) :: dL_dh, A
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons) :: dh_db, dL_db
    REAL(8), DIMENSION(1:n_hidden, 1:n_neurons, 1:n_inputs) :: dh_dh, dh_dw, dL_dw

    h2(0, :) = traindata
    FORALL (j=1:n_inputs) h2(j, :) = hidden(j, :)

!So far only MSE-Loss
!Partials of Loss w.r.t. outputs
    dL_do = -2.*(truevalue - output)/REAL(n_outputs)

!Partials of Output w.r.t. last hidden layer, and w.r.t. output itself
    DO k = 1, n_outputs
        DO l = 1, n_neurons
            do_dh(k, l) = weight_out(k, l)*SIGMOID_DERIV(SUM(weight_out(k, :)*h2(n_hidden, :)) + bias_out(k))
            do_dw(k, l) = h2(n_hidden, l)*SIGMOID_DERIV(SUM(weight_out(k, :)*h2(n_hidden, :)) + bias_out(k))
        END DO
        do_db(k) = SIGMOID_DERIV(SUM(weight_out(k, :)*h2(n_hidden, :)) + bias_out(k))
    END DO

!I contract the output layer already
    FORALL (l=1:n_neurons) dL_dh(l) = SUM(dL_do(:)*do_dh(:, l))

    DO k = 1, n_outputs
        DO l = 1, n_neurons
            dL_dwo(k, l) = dL_do(k)*do_dw(k, l)
        END DO
        dL_dbo(k) = dL_do(k)*do_db(k)
    END DO

    DO m = 1, n_hidden
        DO k = 1, n_neurons
            DO l = 1, n_inputs
                !Partials of m-th hidden layer w.r.t. l-1st, with 0th layer the input
                dh_dh(m, k, l) = weight(m, k, l)*SIGMOID_DERIV(SUM(weight(m, k, :)*h2(m - 1, :)) + bias(m, k))
                !Partials of the m-th layer w.r.t. all weights in this layer
                dh_dw(m, k, l) = h2(m - 1, l)*SIGMOID_DERIV(SUM(weight(m, k, :)*h2(m - 1, :)) + bias(m, k))
            END DO
            !Partials of the m-th layer w.r.t. all biases in this layer
            dh_db(m, k) = SIGMOID_DERIV(SUM(weight(m, k, :)*h2(m - 1, :)) + bias(m, k))
        END DO
    END DO

!Now for further contractions
!More layers: contract to the left (output) side
        DO m = n_hidden, 1, -1
            IF (m .lt. n_hidden) THEN
                A = dL_dh
                DO l = 1, n_neurons
                    A = dL_dh
                    dL_dh(l) = SUM(A(:)*dh_dh(m, :, l))
                END DO
            END IF

            DO k = 1, n_neurons
                DO l = 1, n_inputs
                    dL_dw(m, k, l) = dL_dh(k)*dh_dw(m, k, l)
 !                                        WRITE(*,*) m,k,l,dL_dw(m,k,l)
                END DO
                dL_db(m, k) = dL_dh(k)*dh_db(m, k)
 !                                WRITE(*,*) m,k,dL_db(m,k)
            END DO
        END DO
 !   END IF
 !   WRITE(*,*) dL_dw

    weight = weight - eta*dL_dw
    bias = bias - eta*dL_db
    weight_out = weight_out - eta*dL_dwo
    bias_out = bias_out - eta*dL_dbo

    ! WRITE(*,*) weight BUG!
    ! WRITE(*,*) bias !OK
    ! WRITE(*,*) weight_out !OK
    ! WRITE(*,*) bias_out !OK


END SUBROUTINE BackProp
