mydata<-read.csv("C:/Users/User/Downloads/Drug_clean.csv",header=TRUE,sep=",")
mydata

library(ggplot2)

# Create a bar plot to visualize the distribution of drug types
ggplot(data = mydata, aes(x = Type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Drug Types", x = "Type of Drug", y = "Frequency")

install.packages("gridExtra")
library(gridExtra)


#scattered plot of Drug, Form and Price
ggplot(data = mydata, aes(x = Drug, y = Price, color = Form)) +
  geom_point() +
  labs(title = "Scatter Plot of Drug, Form, and Price",
       x = "Drug", y = "Price") +
  theme_minimal()
####### Zoom View  ################
ggplot(data = mydata, aes(x = Drug, y = Price, color = Form)) +
  geom_point(alpha = 0.6) +  # Adjust transparency
  labs(title = "Scatter Plot of Drug, Form, and Price",
       x = "Drug", y = "Price") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 500))  # Adjust the y-axis limits to zoom in on the region [0, 100]

###############pie chart

condition_counts <- table(mydata$Condition)

condition_data <- data.frame(Condition = names(condition_counts),
                             Frequency = as.numeric(condition_counts))

condition_data <- condition_data[order(condition_data$Frequency, decreasing = TRUE), ]

ggplot(condition_data, aes(x = "", y = Frequency, fill = Condition)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Conditions") +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Condition")

##################### violin plot

library(ggplot2)

# Create violin plots for EaseOfUse, Effective, and Satisfaction
ggplot(mydata, aes(x = "", y = EaseOfUse, fill = "EaseOfUse")) +
  geom_violin(trim = FALSE, fill = "pink") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Violin Plot of Ease of Use",
       x = NULL, y = "Ease of Use") +
  theme_minimal()

ggplot(mydata, aes(x = "", y = Effective, fill = "Effective")) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Violin Plot of Effective",
       x = NULL, y = "Effective") +
  theme_minimal()

ggplot(mydata, aes(x = "", y = Satisfaction, fill = "Satisfaction")) +
  geom_violin(trim = FALSE, fill = "yellow") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Violin Plot of Satisfaction",
       x = NULL, y = "Satisfaction") +
  theme_minimal()

### Musaic Plot of Drug

library(ggplot2)

# Create a line histogram of Reviews with zoomed-in x-axis
ggplot(mydata, aes(x = Reviews)) +
  geom_freqpoly(binwidth = 1, color = "blue") +
  labs(title = "Line Histogram of Reviews", x = "Number of Reviews", y = "Frequency") +
  theme_minimal() +
  xlim(0, 850)

# Load necessary packages
library(ggplot2)

# Assuming you have data for the line chart stored in a dataframe called `line_data`

# Create line chart EaseOfUse & Satisfaction
ggplot(mydata, aes(x = EaseOfUse, y = Satisfaction)) +
  geom_line(color = "blue") +
  labs(title = "Line Chart", x = "Ease of Use", y = "Satisfaction")

## radar chart
# Install and load necessary packages
install.packages("fmsb")
library(fmsb)

# Check the structure of mydata to ensure the "Reviews" column exists
str(mydata)

# Create the reviews dataframe
reviews_df <- data.frame(Reviews = mydata$Reviews)

# Check the structure and summary of reviews_df
str(reviews_df)
summary(reviews_df)

# Check for any missing values
any(is.na(reviews_df))

# If there are no issues, try creating the radar chart again
radarchart(rownames(reviews_df), as.matrix(reviews_df), axistype = 1, title = "Radar Chart")













