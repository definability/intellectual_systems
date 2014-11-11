library(monmlp)

#options(width=150)
wideScreen()

splitdf <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/5))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}

dc <- rep(1, nrow(iris))
BORDER  <- 10
EPSILON <- 2*BORDER/3
dc[iris$Species == 'virginica']  <-  BORDER
dc[iris$Species == 'setosa']     <- -BORDER
dc[iris$Species == 'versicolor'] <- 0

sample <- cbind(iris, dc)

samples <- cbind(head(sample, 50), tail(head(sample,100), 50), tail(sample, 50))

sample1 <- splitdf(samples[1:6])
sample2 <- splitdf(samples[7:12])
sample3 <- splitdf(samples[13:18])

training_sample <- rbind(sample1$trainset, sample2$trainset, sample3$trainset)#splits$trainset
test_sample <- rbind(sample1$testset, sample2$testset, sample3$testset)#splits$testset

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


result <- sapply(pt, cov_to_result)
cmp_result <- cbind(test_sample, result, as.integer(pt))
cmp_result
#intersect(cmp_result$Species, cmp_result$result)
#all(cmp_result$Species == cmp_result$result)
correct <- calculate_correct(cmp_result$Species, cmp_result$result)
total <- nrow(cmp_result)

cat("You've got", correct,
    "corrrect answers of", total,
    "which is", as.integer(10000*correct/total)/100,
    "%\n")
