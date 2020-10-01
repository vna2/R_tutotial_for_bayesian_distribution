#Bayesian Linear Regression Analysis
# " rstanarm " with "women data"
#install.packages("rstanarm")
library(rstanarm)

data = "women"

Model_A <- stan_glm(formula = height ~ weight,
                    data = women ,
                    algorithm = "sampling")

summary(Model_A)

# non Bayesian

Model_B <- glm(formula = height ~ weight,
               data = women )

summary(Model_B)

# The bayesian posterior uncertainty interval
# Credible interval & credible regions
# Frequent is vs bayesiansm

posterior_interval(Model_A, 
                   prob = 0.95 ,
                   type = "central",
                   pars = "weight")

# The confidence Interval in the Non_Bayesian Framework

confint(Model_B,
        parm  = "weight",
        level =  0.95)

# Priors

prior_summary(Model_A)

# standard deviation 
sd_x <- sd(women$weight)
sd_y <- sd(women$height)

(sd_y/sd_x)*2.5

1/sd_y

# b_intercept ~ N( mean(y) , 2.5*sd(y) ) Normal distribution 
mean(women$height)
2.5*sd_y

# visulatzing the priors

Model_A <- stan_glm(formula = height ~ weight,
                    data = women ,
                    algorithm = "sampling",
                    prior = NULL,
                    prior_intercept = NULL,
                    prior_aux = exponential(rate = 2))

prior_summary(Model_A)

posterior_vs_prior(Model_A, 
                   pars = "beta",
                   color_by = c("parameter","vs","none") )

plot(Model_A,"hist")

# Posterior Predictive check PPC
# Simulation from the original data using the Bayesian Model Proposed
# Dataset : women

#install.packages("bayesplot")
#install.packages("ggplot2")
library("ggplot2")
library("bayesplot")

#Simulation

Model_A 

y <- women$height
library("rstanarm")
women_posterior <- posterior_predict(Model_A,
                                    draws = 600)

class(women_posterior)
dim(women_posterior)
head(women_posterior,15)
dim(women)

# Graphic Posterior Predictive Distribution

color_scheme_set("brightblue")

ppc_dens_overlay(y,
                 yrep = women_posterior)

# Quadratic Modelling of the womens height

Model_Q <- stan_glm(formula = height ~ weight + I(weight^2),
                    data = women)

print(Model_Q)
summary(Model_Q)

women_posterior_Q <- posterior_predict(Model_Q,
                  draws = 600)
dim(women_posterior_Q)
head(women_posterior_Q,3)

ppc_dens_overlay(y,
                 yrep = women_posterior_Q[1:40,] )

# Cubic Modeling 

Model_Cubic <- stan_glm(formula = height ~ weight + I(weight^2) + I(weight^3),
                        data = women)

women_posterior_Cubic <- posterior_predict(Model_Cubic,
                                            draws = 600)

dim(women_posterior_Cubic)
head(women_posterior_Cubic)
ppc_dens_overlay(y,
                 yrep = women_posterior_Cubic[1:40,] )

# Histogram of the Posterir Predictive Distribution
# Model A
y = women$height
ppc_hist(y,
         yrep = women_posterior[1:30,],
         binwidth = 20)

ppc_hist(y,
         yrep= women_posterior_Q[1:30,],
         binwidth = 15)

ppc_hist(y,
         yrep= women_posterior_Cubic[1:30,],
         binwidth = 10)

# Posterior Predictive Checks with "boxplots"

# Boxplot of the linear model

pp_check(Model_A,
         plotfun = "boxplot",
         nreps = 1,
         notch = FALSE)

# Boxplot of the Quadratic model

pp_check(Model_Q ,
         plotfun = "boxplot",
         nreps = 20,
         notch = FALSE)

# Boxplot of the cubic model

pp_check(Model_Cubic ,
         plotfun = "boxplot",
         nreps = 20,
         notch = FALSE)

# MCMC Plots

mcmc_intervals(Model_A, 
               pars = "weight")

mcmc_intervals(Model_Q, 
               pars = "weight")

mcmc_intervals(Model_Cubic, 
               pars = "weight")

# Multivariable Regression Analysis in Bayesian Framework

data("state")

dim(state.x77)
head(state.x77,5)

states <- as.data.frame(state.x77[,c("Murder","Population","Income", "Illiteracy", "Area")])
class(states)
head(states,3)

# Create our Multivariable Regression

Model_A_M <- stan_glm(formula = Murder ~ Population + Income + Illiteracy + Area,
         data = states ,
         algorithm = "sampling")
 
summary(Model_A_M)

prior_summary(Model_A_M)


# MCMC Intervals for the multi_variable Bayesian Regression Models

mcmc_intervals(Model_A_M,
               pars = c("Population","Income", "Illiteracy", "Area"))

# Credible interval

posterior_interval(Model_A_M,
                   prob = 0.9,
                   pars = c("Illiteracy","Population","Income", "Area"))

#interaction terms in Regression Analysis
#install.packages("effects")
library(effects)

# upload the data

data("mtcars")

class(mtcars)
head(mtcars,5)
summary(mtcars)

# With no Interaction Terms

Model_no_inter <- glm(formula = mpg ~ hp + wt,
                      data =mtcars)
class(Model_no_inter)

summary(Model_no_inter)

# With interaction Terms

Model_with_inter <- glm(formula = mpg~ hp + wt + hp:wt,
                        data = mtcars)

class(Model_with_inter)

summary(Model_with_inter)
mean(mtcars$wt)
sd(mtcars$wt)

# visualization of the interaction term

plot(effect("hp:wt", Model_with_inter,
       list(wt=c(3.21,2.21,4.21 )),
       vcov. =vcov),
       multiline = TRUE)#2.21 4.21 is  up to mean because sd =0,99

# Importance sampling
 #install.packages("diagis")
library(diagis)

# Extracting impotance weights

 

# Pareto Smoothed Importance Sampling (PSTS)

#install.packages("loo")
library(loo)
library(diagis)
library(rstanarm)


weight_plot(W_x)
plot(density(W_x))

Model_A #basian women model

lik_women_A <- log_lik(Model_A,
       data = women)

dim(lik_women_A)
class(lik_women_A)

# Effective sample test

n_eff <- relative_eff(lik_women_A,
                      chain_id =  rep(1:4,each = 1000))

class(n_eff)
dim(n_eff)

# To fit a Pareto Distribution to our Importance weight Distribution

log_ratios <- 1-lik_women_A

psis_women <- psis(log_ratios, 
     r_eff = n_eff)

class(psis_women)

weight_smooth <- weights.importance_sampling(psis_women)
class(weight_smooth)
dim(weight_smooth)
head(weight_smooth,1)

plot(weight_smooth)
plot(psis_women$log_weights)

psis_women$diagnostics
plot(psis_women$diagnostics$pareto_k)

pareto_k_table(psis_women)

pareto_k_values(psis_women)

# Leave one_out cross validation

library(loo)

loo_women <- loo(Model_A,
    save_psis = TRUE)

plot(loo_women)

print(loo_women)

# Model comparison

library(rstanarm)

Model_no_inter

Model_no_inter_Bayesian <- stan_glm( formula = mpg~ hp + wt ,
                                     data = mtcars)

Model_with_inter

Model_with_inter_Bayesian <- stan_glm( formula = mpg~ hp + wt + hp:wt,
                                     data = mtcars)



# Compare two models
library(loo)

loo_compare(loo(Model_no_inter_Bayesian, save_psis = TRUE), 
            loo(Model_with_inter_Bayesian ,save_psis = TRUE))



# Graphical validation

loo_with_inter <- loo(Model_with_inter_Bayesian ,
                      save_psis = TRUE)

loo_no_inter <- loo(Model_no_inter_Bayesian ,
                      save_psis = TRUE)
library(bayesplot)
ppc_loo_pit_overlay(Model_no_inter_Bayesian$y,
                    posterior_predict(Model_no_inter_Bayesian),
                    lw = weights(loo_no_inter$psis_object))


ppc_loo_pit_overlay(Model_with_inter_Bayesian$y,
                    yrep= posterior_predict(Model_with_inter_Bayesian),
                    lw = weights(loo_with_inter$psis_object))

# Make Prediction of your models

head(mtcars,2)
new_data = data.frame(hp =120 , wt =3,2)

predict_new_car <- posterior_predict(Model_no_inter_Bayesian,
                                      newdata = new_data)
dim(predict_new_car)
class(predict_new_car)
predict_new_car[1:20,]

# Plot the uncertainty 

#install.packages("ggiraph")
require(ggiraph)

#devtools::install_github("cardiomoon/ggiraphExtra")
require(ggiraphExtra)

#install.packages("plyr")
require(plyr)

#install.packages("ggplot2")
library(ggplot2)

ggplot(women,aes(x=women$weight , y =women$height)) + geom_point() + geom_smooth(method = "stan_glm")

# Apply ggPredict 

ggPredict(Model_A,
          se =TRUE,
          interactive =TRUE)
