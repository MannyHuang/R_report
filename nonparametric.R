library(ggplot2)
library(PLRModels)

# simulate data
set.seed(333)
N <- 100
n <- 200
x <- matrix(runif(N, 0, 3), nrow = n)
m <- function(t) {exp(t)}
epsilon <- rnorm(n, 0.5)
y <- m(x) + epsilon


data <- matrix(c(y, x), nrow= n)

# nonparametric regression using NW, local linear and spline
est_nw <- np.est(data)
est_ll <- np.est(data, estimator = "LLP")
est_ss <- fitted(smooth.spline(x, y))

# visualization 
new_data <- rbind(data.frame(x = x, y = m(x), classification = as.factor("m(x)")),
             data.frame(x = x, y = est_nw, classification = as.factor("NW")),
             data.frame(x = x, y = est_ll, classification = as.factor("local linear")),
             data.frame(x = x, y = est_ss, classification = as.factor("smoothing spline")))

ggplot(new_data, aes(x, y)) +
  geom_line(aes(color = classification, linetype = classification) ) + 
  ggtitle("Comparison of three nonparametric regressions", subtitle = "NW, local linear, smoothing spline") +
  xlab("x") +
  ylab("m(x)/m(x)_hat")






