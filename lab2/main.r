library(monmlp)

wideScreen()

# https://github.com/earowang/statsR/blob/master/splitDf.R
splitdf <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/5))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}

BORDER  <- 10
EPSILON <- 2*BORDER/3

dc <- rep(1, nrow(iris))
dc[iris$Species == 'virginica']  <-  BORDER
dc[iris$Species == 'setosa']     <- -BORDER
dc[iris$Species == 'versicolor'] <- 0

sample <- cbind(iris, dc)

dim(sample)
colnames(sample)
samples <- mapply(splitdf,
    Map(function(x) sample[sample$Species == x,], unique(sample[,"Species"])))

training_sample <- Reduce(function(x,y) {rbind(x,y)}, samples["trainset",])
test_sample <- Reduce(function(x,y) {rbind(x,y)}, samples["testset",])

ft <- monmlp.fit(x=as.matrix(training_sample[1:4]), y=as.matrix(training_sample[6]), hidden1=3)
pt <- monmlp.predict(x=as.matrix(test_sample[1:4]), weights = ft)

cov_to_result <- function(x) {
         if (x < -EPSILON) return('setosa')
    else if (x >  EPSILON) return('virginica')
    else                   return('versicolor')
}
calculate_correct <- function(x, y) {
    sum(as.integer(x == y))
}


result <- mapply(cov_to_result, pt)
cmp_result <- cbind(test_sample, Result=result, Cov=round(pt,1))
correct <- calculate_correct(cmp_result$Species, cmp_result$Result)
total <- nrow(cmp_result)

print("Here is comparison matrix")
cmp_result
cat("You've got", correct,
    "corrrect answers of", total,
    "which is", round(100*correct/total,2),
    "%\n")
write(round(100*correct/total,2), stderr())
