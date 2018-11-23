# EXPERIMENT 1 ANALYSIS
# Import Experiment 1 data set
Experiment_1 <- read.csv("C:/Users/ki_ro/Documents/misc/Uni/Queen Mary/Modules 2018-19/Statistics and bioinformatics/Coursework/Statistics/Experiment1_data_set 33 .txt", sep="")
# Check data set variables are suitable (i.e. factors/numeric)
str(Experiment_1)
# Carry out exploratory analysis by plotting the data set
Exp_plot1 <- plot(data = Experiment_1, Leaf.area ~ Hyphae * Nutrients, main = "Leaf Growth Exploratory Analysis Plot", col = c("steelblue", "orange"))
# Install and load ggplot package 
install.packages("ggplot2")
library(ggplot2)
# Scatter plot showing factor, covariate and interaction 
plot1<-ggplot(data = Experiment_1, aes(x = Hyphae, y = Leaf.area, colour = Nutrients)) + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE, lwd=0.5) + theme_bw() + scale_colour_manual(values = c("Steelblue", "orange"))
print(plot1 + ggtitle("Leaf growth and degree of Hyphae infection in different 
                      nutrient conditions"))
# Fit a linear regression model
model1 <- lm(Leaf.area ~ Hyphae * Nutrients, data = Experiment_1)
# Test whether the interaction term is significant
drop1(model1, test = "F")
# Plot the linear regression model to ensure it fits the regression assumptions
plot(model1)
# Calculate ab lines using coefficient values in model summary table
summary(model1)
# Plot scatter graphs again including calculated ab lines from the coefficient table
plot1<-ggplot(data = Experiment_1, aes(x = Hyphae, y = Leaf.area, colour = Nutrients)) + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE, lwd=0.5) + theme_bw() + scale_colour_manual(values = c("Steelblue", "orange"))
print(plot1 + ggtitle("Leaf growth and degree of Hyphae infection in different 
                      nutrient conditions"))
plot1 <- plot1 + geom_abline(intercept = 36.0919,slope =  1.3936, col = "steelblue")
plot1 <- plot1 + geom_abline(intercept = 33.983, slope = 0.1213, col = "orange")

# EXPERIMENT 2 ANALYSIS
# Import Experiment 2 data set
Experiment_2 <- read.csv("C:/Users/ki_ro/Documents/misc/Uni/Queen Mary/Modules 2018-19/Statistics and bioinformatics/Coursework/Statistics/Experiment2_data_set 33 .txt", sep="")
# Check data set variables are suitable (i.e. factors/numeric)
str(Experiment_2)
# Carry out exploratory analysis by plotting the data set
Exp_plot2 <- plot(data = Experiment_2, Leaf.area ~ Fungus * Nutrients, main = "Leaf Growth Exploratory Analysis Plot", col = c("steelblue", "orange"))
# Load ggplot package
library(ggplot2)
# Scatter plot showing two factors and interaction 
plot2 <- ggplot(data = Experiment_2, aes(x = Nutrients, y = Leaf.area, fill = Fungus)) + scale_fill_manual(values=c("steelblue", "orange")) + geom_boxplot() + theme_bw()
print(plot2 + ggtitle("Leaf growth and fungus presence in different 
                      nutrient conditions"))
# Fit a linear regression model
model2 <- lm(Experiment_2$Leaf.area~Experiment_2$Fungus*Experiment_2$Nutrients) 
# Test whether the interaction term is significant
drop1(model2, test = "F")
# Plot the linear regression model to ensure it fits the regression assumptions
plot(model2)
# Calculate estimated means from the coefficient table
summary(model2)$coefficients
tapply(Experiment_2$Leaf.area, list(Experiment_2$Fungus, Experiment_2$Nutrients), mean)
# Summary of linear regression model for probability values
summary(model2)
# Visualise the significant effect of nutrient conditions
attach(Experiment_2)
interaction.plot(Fungus, Nutrients, Leaf.area, main = "Interaction plot showing effect of nutrients and fungus
                 presence on leaf growth")
plot1 <- plot1 + geom_abline(intercept = 33.983, slope = 0.1213, col = "orange")
