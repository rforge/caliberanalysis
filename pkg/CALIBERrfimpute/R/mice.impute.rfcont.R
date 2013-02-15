mice.impute.rfcont <-
function(y, ry, x, ntree_cont=NULL, nodesize_cont=NULL, ...){
	# y is the vector of y (observed and unobserved)
	# ry is a vector of indicators as to whether y is observed
	# x is the matrix of predictors
	x <- as.matrix(x)
	bootsample <- sample(sum(ry), replace=TRUE)
	
	if (is.null(ntree_cont)){
		if (is.null(getOption('CALIBERrfimpute_ntree_cont'))){
			ntree_cont <- 10
		} else {
			ntree_cont <- getOption('CALIBERrfimpute_ntree_cont')
		}
	}

	if (is.null(nodesize_cont)){
		if (is.null(getOption('CALIBERrfimpute_nodesize_cont'))){
			nodesize_cont <- 5
		} else {
			nodesize_cont <- getOption('CALIBERrfimpute_nodesize_cont')
		}
	}

	# Only bootstrap if more than one tree, because Random Forest
	# fits to a bootstrap sample
	if (ntree_cont > 1){
		yobs <- y[ry][bootsample]
		xobs <- x[ry,][bootsample,]
	} else {
		yobs <- y[ry]
		xobs <- x[ry,]
	}
	xmiss <- x[!ry,]
	# Build a random forest
	rf <- randomForest(xobs, yobs, ntree=ntree_cont,
		nodesize=nodesize_cont, ...)
	yhat <- predict(rf, xmiss)
	# Draw imputed values from normal distributions
	# centred on the means predicted by Random Forest 
	yimp <- rnorm(length(yhat), mean=yhat, sd=sqrt(rf$mse[ntree_cont]))		
	return(yimp)
}
