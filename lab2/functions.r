# https://github.com/earowang/statsR/blob/master/splitDf.R
# Divide sample into test and training
# createDataPartition
splitdf <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/5))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}

# Covariation to iris class
cov_to_result <- function(x) {
         if (x < -EPSILON) return('setosa')
    else if (x >  EPSILON) return('virginica')
    else                   return('versicolor')
}

# Compare two vectors and return the number of equalities
calculate_correct_answers <- function(x, y) {
    sum(as.integer(x == y))
}
