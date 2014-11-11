# Rosenblatt's perceptron
# http://rrus.wordpress.com/2011/03/28/perceptron/
# http://stackoverflow.com/questions/18306362/run-r-script-from-command-line


trainPerceptron <- function(dm, dc) {
    result <- list()
    oldW <- c()
    w <- rep(0, ncol(dm))
    while (!identical(w, oldW)) {
        oldW <- w
        for (i in 1:nrow(dm)) {
            pred <- sign(dm[i,] %*% w)
            w = w + (dc[i] - pred) * dm[i, ]
        }
        # cat(w, "\n")
        result <- c(result, list(w))
    }
    result
}

plotPerceptronSteps <- function(pcResult, dm, dc, steps) {
    for (s in steps) {
        w <- pcResult[[s]]
        plot(dm[,1:2], main=paste("step", s), pch=ifelse(dc > 0, 1, 2), sub=paste(w, collapse='; '))
        abline(-w[3] / w[2], -w[1] / w[2])
    }
}

# Prepare data
dd <- read.table("data07.csv", sep=',', head=FALSE)
dm <- data.matrix(dd[, 1:2])
dm <- cbind(dm,1)
dc <- rep(1, nrow(dm))
dc[dd$V3 == 0] <- -1
# Process data
pc <- trainPerceptron(dm, dc)
plotPerceptronSteps(pc, dm, dc, length(pc))
png("perceptron-biased.png", width=1600, height=1200)
par(mfrow=c(3,2))
plotPerceptronSteps(pc, dm, dc,
                    c(1, length(pc)%/%16, length(pc)%/%8, length(pc)%/%4,
                      length(pc)%/%2, length(pc)))
dev.off()
