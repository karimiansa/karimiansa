# Install and load necessary libraries
install.packages(c("drc", "nlme"))
library(drc)
library(nlme)

# Data
Sulf_Conc <- c(-3, -2.958607315, -2.698970004, -1.958607315, -0.995678626, -0.300162274, 0.699056855)
Enzyme_Activities <- c(100, 55.30724303, 69.24958323, 82.88291229, 100.627504, 104.0535174, 67.16165961)

# Model using gnls
model <- gnls(Enzyme_Activities ~ A + (D - A)/(1 + exp((C - Sulf_Conc)/B)),
              params = list(A ~ 1, D ~ 1, B ~ 1, C ~ 1), 
              start = list(A = min(Enzyme_Activities), D = max(Enzyme_Activities), B = 1, C = median(Sulf_Conc)),
              data = data.frame(Enzyme_Activities, Sulf_Conc))

# Display summary of the model
summary(model)

# Plotting
plot(Sulf_Conc, Enzyme_Activities, main="Data with Fitted Curve", xlab="Sulf_Conc", ylab="Enzyme Activities")
lines(sort(Sulf_Conc), fitted(model)[order(Sulf_Conc)], col="red")
