mice.impute.rfcat <-
function(y, ry, x, ntree_cat=NULL, nodesize_cat=NULL, ...){
	# y is the vector of y (observed and unobserved)
	# ry is a vector of indicators as to whether y is observed
	# x is the matrix of predictors
	# Select a bootstrap sample
	x <- as.matrix(x)
	bootsample <- sample(sum(ry), replace=TRUE)
	yobs <- y[ry][bootsample]
	xobs <- x[ry,][bootsample,]
	xmiss <- x[!ry,]
	
	if (is.null(ntree_cat)){
		if (is.null(getOption('CALIBERrfimpute_ntree_cat'))){
			ntree_cat <- 10
		} else {
			ntree_cat <- getOption('CALIBERrfimpute_ntree_cat')
		}
	}

	if (is.null(nodesize_cat)){
		if (is.null(getOption('CALIBERrfimpute_nodesize_cat'))){
			nodesize_cat <- 1
		} else {
			nodesize_cat <- getOption('CALIBERrfimpute_nodesize_cat')
		}
	}

	# Build a set of trees
	trees <- lapply(1:ntree_cat, function(x){
		randomForest(xobs, yobs, ntree=1, nodesize=nodesize_cat)
	})
	# Choose a random tree and predict the outcome for each observation
	yimp <- apply(xmiss, 1, function(x){
		predict(trees[[sample(ntree_cat, 1)]], x)})
	return(yimp)
}
