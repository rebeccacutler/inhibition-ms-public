

trial1 = data[data$trial == 1.2 ,]
trial2 = data[data$trial == 1.1 ,]


library(rstanarm)
library(bayestestR)

model <- stan_glm(
  formula = proportion ~ condition,
  data = trial2,
  prior = normal(0, 0.3, autoscale = FALSE)
)

summary(model)
BF <- bayesfactor_parameters(model, null = c(-1, 1))
BF

effectsize::interpret_bf(exp(BF$log_BF[2]), include_value = TRUE)
