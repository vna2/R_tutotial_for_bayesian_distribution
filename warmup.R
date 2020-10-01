# Warm up with npn - bayesian Regression

data("women")

y <- women$height
x <- women$weight

# Monotne linear regression
lin.formula <- as.formula(y ~ x)
ln.model <- glm(lin.formula ,
    data = women) #model
plot(x,y)
abline(ln.model,col ="blue")


#Generalized Addictive model

#install.packages("mgcv")
library(mgcv)

spline.formula <- as.formula(y ~ s(x))

gam.model <- gam(spline.formula,
    data = women)
gam.model$converged
plot(x,y)
plot(gam.model, col ="red")

