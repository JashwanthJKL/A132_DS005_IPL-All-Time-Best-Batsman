# Load the necessary libraries
library(tidyverse)
library(readr)
data <- read_csv("C:/Users/L K Naidu/Desktop/Docs/Jashwanth/jashwanth Assignment/res/All-Time-Best-Batsman.csv")
#View(All_Time_Best_Batsman_1_)

# View the structure of the dataset
str(data)

# Clean the data
data <- data %>%
  mutate(Runs = as.numeric(gsub("[^0-9]", "", Runs)),  # Remove special characters from Runs if necessary
         SR = as.numeric(SR)) %>%
  na.omit()  # Remove rows with NA values

#Descriptive Statistics
# Summary statistics
summary(data[c("Runs", "SR")])


# Ensure you've loaded the Plotly and Tidyverse libraries
# Install the plotly package
#install.packages("plotly")
# Load the plotly library
library(ggplot2)
library(plotly)
library(tidyverse)

# Generate the scatter plot with proper axes and title
scatter_plot <- plot_ly(data, x = ~SR, y = ~Runs, type = 'scatter', mode = 'markers',
                        marker = list(color = 'rgba(255, 50, 50, 0.5)')) %>%
  layout(
    title = "Scatter Plot: Strike Rate vs Runs",
    xaxis = list(title = "Strike Rate"),
    yaxis = list(title = "Runs")
  )

# Display the scatter plot
scatter_plot

# Generate a bell-shaped density curve overlay using ggplot2
library(ggplot2)

# Density plot to overlay
density_plot <- ggplot(data, aes(x = SR)) +
  geom_density(fill = "blue", alpha = 0.3) +
  labs(
    title = "Density Curve of Strike Rate",
    x = "Strike Rate",
    y = "Density"
  ) +
  theme_minimal()

# Print the density plot
print(density_plot)

# If running in an R environment, you can view the plot by simply typing
scatter_plot

# Interactive box plot of Runs by Player
box_plot <- plot_ly(data, y = ~Runs, type = 'box',
                    marker = list(color = 'rgba(80, 180, 100, 0.6)')) %>%
  layout(title = "Box Plot of Runs",
         yaxis = list(title = "Runs"))

plot_ly(data, x = ~Runs, type = 'histogram',
        marker = list(color = 'rgba(50, 150, 250, 0.6)')) %>%
  layout(title = "Histogram of Runs",
         xaxis = list(title = "Runs"),
         yaxis = list(title = "Frequency"))


plot_ly(data, y = ~Runs, type = 'box',
        marker = list(color = 'rgba(80, 180, 100, 0.6)')) %>%
  layout(title = "Box Plot of Runs",
         yaxis = list(title = "Runs"))


# Aggregate data for plotting
team_runs <- data %>%
  group_by(Team) %>%
  summarize(TotalRuns = sum(Runs))

plot_ly(team_runs, x = ~Team, y = ~TotalRuns, type = 'bar',
        marker = list(color = 'rgba(150, 100, 250, 0.7)')) %>%
  layout(title = "Bar Plot of Total Runs by Team",
         xaxis = list(title = "Team"),
         yaxis = list(title = "Total Runs"))


# Calculate Pearson correlation
correlation <- cor.test(data$SR, data$Runs, method = "pearson")
print(correlation)

# Output the correlation coefficient and p-value
cat("Correlation coefficient:", correlation$estimate, "\n")
cat("P-value:", correlation$p.value, "\n")


# Fit a linear regression model
model <- lm(Runs ~ SR, data = data)
summary(model)

# Plot the regression line on the scatter plot
regression_plot <- ggplot(data, aes(x = SR, y = Runs)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Regression Analysis of Strike Rate on Runs",
    x = "Strike Rate",
    y = "Runs"
  )

# Display the regression plot
print(regression_plot)

# Check if the p-value from the model is significant
p_value <- summary(model)$coefficients[2, 4]  # Extract p-value for the slope coefficient
if (!is.na(p_value) && p_value < 0.05) {
  cat("Reject the Null Hypothesis: There is a significant relationship between strike rate and runs.\n")
} else {
  cat("Fail to reject the Null Hypothesis: There is no significant relationship between strike rate and runs.\n")
}

