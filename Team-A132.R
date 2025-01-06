getwd()
View(All_Time_Best_Batsman)
plot.design(All_Time_Best_Batsman)
plot(All_Time_Best_Batsman)
library(ggplot2)
ggplot(head(All_Time_Best_Batsman), aes(Mat, Avg))+ geom_point()+ geom_smooth()
read.csv(All_Time_Best_Batsman)
head(All_Time_Best_Batsman,2) 
head(All_Time_Best_Batsman) 
hist(Mat, Avg)
library(ggplot2)

ggplot(All_Time_Best_Batsman, aes(x = Runs, y = SR)) +
  geom_point(color = "blue", alpha = 0.6) +          # Points for data
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Linear regression line
  labs(
    title = "Relationship Between Runs and Strike Rate",
    x = "Runs",
    y = "Strike Rate (SR)"
  ) +
  theme_minimal()  # Clean theme

# Load necessary library
library(ggplot2)

# Histogram for Runs with a density curve
ggplot(All_Time_Best_Batsman, aes(x = Runs)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Runs with Density Curve",
    x = "Runs",
    y = "Density"
  ) +
  theme_minimal()

# Histogram for Strike Rate (SR) with a density curve
ggplot(All_Time_Best_Batsman, aes(x = SR)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(
    title = "Distribution of Strike Rate with Density Curve",
    x = "Strike Rate (SR)",
    y = "Density"
  ) +
  theme_minimal()
