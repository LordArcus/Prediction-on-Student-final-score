#_________________ Topic ______________

############## Regression + Multi correlation ########


#############################################
######## Project Start ######################
#############################################

# necessary libraries
library(car)

#---- 1. Setup ---
# link to the dataset: 
# https://www.kaggle.com/datasets/sarveshchhetri/student-lifestyle-vs-academic-performance-dataset?select=student_performance_finalscore.csv

# Location to data
setwd("/home/lordarcus/Downloads/")

#---- 2. Data loading --------
unzip("archive.zip")
files <- list.files(pattern = "*.csv")
print(files)

#------- we are doing regression so we will be 
#   using "student_performance_finalscore.csv"

df <- read.csv("student_performance_finalscore.csv")

#----------------- For the regression analysis we will take following variables----
# dependent variable <- Final_Score
# independent variables <- Age, Hours_Studied, Attendance, Sleep_Hours, 
#                       Stress_Level, Screen_Time, Previous_GPA, 
#                       Tutoring_Sessions_per_Week, Exam_Anxiety_Score
# Here we have taken only numeric columns for regression analysis
target_var <- "Final_Score"
feature_vars <- c("Hours_Studied", "Attendance", "Sleep_Hours", 
                  "Stress_Level", "Screen_Time", "Previous_GPA", 
                  "Tutoring_Sessions_Per_Week", "Exam_Anxiety_Score")
selected_columns <- c(target_var, feature_vars)
df_final <- df[, selected_columns]

# Summary of our final data
summary(df_final)

#remove row where Previous_GPA is greater than 4.0
df_final <- df_final[df_final$Previous_GPA <= 4, ]

# Fit the linear model
model <- lm(Final_Score~., data=df_final)

# Summary of model
summary(model)

# All the variables except Age is significant in the test.
# However, we will take Age to further analysis

# Calculate VIF  for multicollinearity
vif_results <- vif(model)
print(vif_results)
# All the values of VIF are less than 2, so we will keep all the variable

# QQ plot for normality
qqPlot(model, 
       main = "Normal Q-Q Plot with 95% Confidence Envelope", 
       ylab = "Studentized Residuals", 
       xlab = "Theoretical Quantiles",
       pch = 19,           
       col = "#2c3e50",    
       col.lines = "#e74c3c", 
       id = FALSE)  

# I tried to test Shapiro test but we have large amount of data
# Since Shapiro test only work on data that is less than 5000
# we move to K-S test

# Kolmogorov_Smirnor (K-S) test for normality
res <- residuals(model)
z_res <- (res - mean(res)) / sd(res)
ks.test(z_res, "pnorm")

# we get p value less than 0.05, which means our data isnot normally distributed.
# However, we have large data, therefore, some of the outliers may have affect
# decision on p_value. We have already visualize our data and saw that our data 
# is mostly normal. And according to Central Limit Theorem, any large data will 
# tends to normal distribution.

# Histogram of residuals
hist(res, breaks = 100, probability = TRUE, 
     main = "Residual Distribution (n > 5000)", 
     col = "steelblue", border = "white")
lines(density(res), col = "red", lwd = 2) 

# test for constant variance
# 2. Mathematical Test (Breusch-Pagan)
# Null Hypothesis: Variance is constant (p > 0.05 is the goal)
ncvTest(model)


### for presentation image


# 1. Open the high-res device
png("QQPlot_Presentation.png", 
    width = 1200, 
    height = 900, 
    res = 150)

# 2. Adjusted Graphical Parameters
# mar = c(bottom, left, top, right)
# We increase the 3rd value (top) to 6 or 7 to give the title room.
par(cex.main = 1.8,  
    cex.lab = 1.6,   
    cex.axis = 1.4,  
    mar = c(5, 6, 7, 2) + 0.1) # Increased top margin to 7 and left to 6

# 3. The Plot
qqPlot(model, 
       main = "Normal Q-Q Plot with 95% Confidence Envelope", 
       ylab = "Studentized Residuals", 
       xlab = "Theoretical Quantiles",
       pch = 19,           
       col = "#2c3e50",    
       col.lines = "#e74c3c", 
       id = FALSE,
       cex = 1.0)

# 4. Close device
dev.off()


# For normally distributed residual
# 1. Open a high-resolution PNG device
png("Residual_Histogram_Presentation.png", 
    width = 1200, 
    height = 900, 
    res = 150)

# 2. Adjusted Graphical Parameters
# mar = c(bottom, left, top, right)
# Increase top (7) for the title and left (6) for the density labels
par(cex.main = 1.8,  
    cex.lab = 1.6,   
    cex.axis = 1.4,  
    mar = c(5, 6, 7, 2) + 0.1)

# 3. Create the Histogram
res <- residuals(model)
hist(res, 
     breaks = 100, 
     probability = TRUE, 
     main = "Residual Distribution (n > 5000)", 
     xlab = "Studentized Residuals",
     ylab = "Density",
     col = "steelblue", 
     border = "white",
     las = 1) # Makes y-axis numbers horizontal

# 4. Add the Density Line (The actual distribution)
lines(density(res), col = "#e74c3c", lwd = 3) # Using the same red as your QQ lines

# 5. Add a Theoretical Normal Curve (The "Perfect" Bell Curve)
# This allows the audience to compare your data to a perfect normal curve
x_seq <- seq(min(res), max(res), length = 100)
y_seq <- dnorm(x_seq, mean = mean(res), sd = sd(res))
lines(x_seq, y_seq, col = "black", lwd = 2, lty = 2) # Dashed black line

# 6. Close the device
dev.off()


## Residual Vs fitted graph
# 1. Open device
png("Res_vs_Fitted_Presentation.png", width = 1200, height = 900, res = 150)

# 2. Set margins (Top for title, Left for label)
par(cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4, mar = c(5, 6, 7, 2) + 0.1)

# 3. The Plot (using the 'which = 1' option for Res vs Fitted)
# 'which = 1' is the standard Residuals vs Fitted plot in R
plot(model, which = 1, 
     main = "Residuals vs Fitted", 
     sub = "", # Removes R's default subtext
     caption = "", # Removes default caption
     col = "#2c3e50", 
     pch = 19, 
     add.smooth = TRUE, 
     col.smooth = "#e74c3c", # Red line for the trend
     lty.smooth = 1)

# 4. Close device
dev.off()
