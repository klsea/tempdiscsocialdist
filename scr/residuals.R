residuals <- function(data, model) {
  data$residuals <- resid(model)
  data$stand.residuals <- rstandard(model)
  data$stud.residuals <- rstudent(model)
  data$cooks.dist <- cooks.distance(model)
  data$dfbeta <- dfbeta(model)
  data$dffit <- dffits(model)
  data$leverage <- hatvalues(model)
  data$cov.ratios <- covratio(model)
  return(data)
}
