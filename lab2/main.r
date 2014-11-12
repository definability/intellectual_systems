library(monmlp)
source("functions.r")

# http://stackoverflow.com/a/1173161/1305036
wideScreen()

# Classification constants
BORDER  <- 100
EPSILON <- 2*BORDER/3

# Convert classes names to numbers
dc <- rep(1, nrow(iris))
dc[iris$Species == 'virginica']  <-  BORDER
dc[iris$Species == 'setosa']     <- -BORDER
dc[iris$Species == 'versicolor'] <- 0
# Append numbers to iris dataset
sample <- cbind(iris, dc)

# Divide sample into training and test sample for each class separately
# to avoid irregularity
samples <- mapply(splitdf,
    Map(function(x) sample[sample$Species == x,], unique(sample[,"Species"])))
# Concatenate training samples
training_sample <- Reduce(function(x,y) {rbind(x,y)}, samples["trainset",])
# Concatenate test samples
test_sample <- Reduce(function(x,y) {rbind(x,y)}, samples["testset",])

# Train the network
ft <- monmlp.fit(x=as.matrix(training_sample[1:4]), y=as.matrix(training_sample[6]), hidden1=3)
# Try to predict
pt <- monmlp.predict(x=as.matrix(test_sample[1:4]), weights = ft)

# Translate numbers to classes
result <- mapply(cov_to_result, pt)
# Concatenate source and fit data
cmp_result <- cbind(test_sample, Result=result, Cov=round(pt,1))
# Calculate correct answers
correct <- calculate_correct_answers(cmp_result$Species, cmp_result$Result)
# Store the length of test sample
total <- nrow(cmp_result)

print("Here is comparison matrix")
cmp_result
cat("You've got", correct,
    "corrrect answers of", total,
    "which is", round(100*correct/total,2),
    "%\n")
# Log the correctness to stderr
write(round(100*correct/total,2), stderr())
