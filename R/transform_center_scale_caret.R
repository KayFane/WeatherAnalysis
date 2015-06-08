require(ggplot2)
require(gridExtra)
require(reshape2)

set.seed(1)
predictors = data.frame(x1 = rnorm(1000, mean = 5, sd = 2), 
                        x2 = rexp(1000, rate=10))

p1 = ggplot(predictors) + geom_point(aes(x = x1, y = x2))

temp <- melt(predictors, measured = c("x1", "x2"))
p2 = ggplot(temp) + geom_histogram(aes(x=value)) + 
  facet_grid(. ~ variable, scales = "free_x")

grid.arrange(p1, p2)

require(caret)

trans <- preProcess(predictors, c("BoxCox", "center", "scale"))
predictors_trans <- data.frame(trans = predict(trans, predictors))

p1 = ggplot(predictors_trans) + geom_point(aes(x = trans.x1, y = trans.x2))
temp <- melt(predictors_trans, measured = c("trans.x1", "trans.x2"))
p2 = ggplot(temp) + geom_histogram(aes(x=value), data = temp) + 
  facet_grid(. ~ variable, scales = "free_x")
grid.arrange(p1, p2)
