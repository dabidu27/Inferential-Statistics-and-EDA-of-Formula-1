#IMPORT DATASET

install.packages("readxl")
library(readxl)
dataset <- read_excel("C:/Users/David/Desktop/statistics project f1.xlsx", sheet = 4)

# Remove empty rows
dataset <- dataset[rowSums(is.na(dataset)) < ncol(dataset), ]

print(dataset)

#I. CONFIDENCE INTERVALS
#Using confidence intervals, we will estimate:

#1. The average age of a Formula 1 driver
#2. The proportion of streets circuit on the Formula 1 calendar

install.packages("dplyr")
library(dplyr)

#1. Estimating mean driver age
important_statistics <- dataset %>%
  filter(!is.na(`Constructor Tier`)) %>%
  group_by(`Constructor Tier`) %>%
  summarise(
    Average_age = mean(`Driver Age`, na.rm = TRUE),
    Stddev = sd(`Driver Age`, na.rm = TRUE),
    Variance = var(`Driver Age`, na.rm = TRUE),
    No_of_observations = n()
  )

print(important_statistics)

total <- dataset %>%
  summarise(
    `Constructor Tier` = "Total",
    No_of_observations = sum(!is.na(`Driver Age`))
  )

total_stats <- bind_rows(important_statistics, total)

total_n = total_stats$No_of_observations[total_stats$`Constructor Tier` == "Total"]
print(total_n)

ovr_stats <- dataset %>%
  summarise(
    ovr_mean = mean(`Driver Age`, na.rm = TRUE),
    MSW = sum((important_statistics$`Variance` * important_statistics$No_of_observations)/total_n),
    MSB = sum((important_statistics$Average_age - ovr_mean)^2 * important_statistics$No_of_observations)/total_n,
    MST = MSW+MSB,
    SE = sqrt(MSW)/sqrt(total_n)
  )

print(ovr_stats)

confidence_levels <- c(0.95, 0.9, 0.99)
alpha <- 1 - confidence_levels
t_values <- qt(1-alpha/2, df = total_n - 1)
ME <- ovr_stats$SE * t_values
lower_bound <- ovr_stats$ovr_mean - ME
upper_bound <- ovr_stats$ovr_mean + ME

CI <- data.frame(
  Confidence_Level = confidence_levels,
  Significance_Level = alpha,
  t_value = t_values,
  Margin_of_Error = ME,
  Lower_Bound = lower_bound,
  Upper_Bound = upper_bound
)

print(CI[, c("Confidence_Level", "Lower_Bound", "Upper_Bound")])

ci_index <- 1  # 95% confidence
upper <- CI$Upper_Bound[ci_index]
lower <- CI$Lower_Bound[ci_index]

x <- barplot(
  ovr_stats$ovr_mean,
  ylim = c(0, upper + 5),
  names.arg = "Mean Driver Age",
  col = "lightblue",
  width = 0.1  # thinner bar
)

# Now x holds the actual x-coordinate of the bar
arrows(x, lower, x, upper,
       angle = 90, code = 3, length = 0.1,
       col = "red", lwd = 2)
#We can affirm with a 95% confidence level that the average age of Formula 1 drivers is between 26.52560295 and 28.6743971 years old.

#2. Estimating proportion of street circuits (it is to be noted that for this analysis we will only count unique races)

race_data <- dataset[, c("Race", "Street or permanent circuit")]
race_data <- race_data[!duplicated(race_data$Race), ]
race_data <- race_data[-18, ]

n <- nrow(race_data); n
Number_of_street_circuits = sum(race_data$`Street or permanent circuit` == "Street circuit"); Number_of_street_circuits
phat = Number_of_street_circuits/n; phat
sd = sqrt(phat*(1-phat)); sd
se = sd/sqrt(n); se

statistics <- data.frame(
  Statistic = c("Sample size (n)", 
                "Number of street circuits", 
                "Sample proportion of street circuits (phat)",
                "Standard deviation for proportions",
                "Standard error (se)") ,
  
  Value = c(n, Number_of_street_circuits, phat, sd, se)
)

print(statistics)


z_values = qnorm(1-alpha/2); z_values
ME = statistics$Value[statistics$Statistic == "Standard error (se)"] * z_values

CI <- data.frame(
  Confidence_Level = confidence_levels,
  Significance_Level = alpha,
  z_value = z_values,
  ME = ME,
  Lower_Bound = phat - ME,
  Upper_Bound = phat + ME
)

print(CI[, 1:4])
print(CI[, c(1, 5, 6)])

ci_index = 1
lower = CI$Lower_Bound[ci_index]
upper = CI$Upper_Bound[ci_index]

x <- barplot(statistics$Value[statistics$Statistic == "Sample proportion of street circuits (phat)"], 
             ylim = c(0, upper+0.1), 
             names.arg = "Street circuits proportion",
             col = "lightblue")

arrows(x, lower, x, upper, angle = 90, code = 3, col = "red", lwd = 2)

#We are 95% sure that the proportion of streets circuits used between 2014 and 2024 is between 11.8385355% and 45.3043216% 

#II. HYPOTHESIS TESTING

#1. a) Do F1 drivers, on average, improve their position during the race?
#We will test for this using a one-sample, two-tailed t-test

#H₀: (Null hypothesis): h0:  μ = 0
#H₁ (Alternative Hypothesis): μ != 0

x0 <- 0
xbar = mean(dataset$`Position change`); xbar
stddev = sd(dataset$`Position change`); stddev
n = nrow(dataset); n
tstat = (xbar - x0) / (stddev/sqrt(n)); tstat
tcrit1 = qt(1-0.05/2, df = n-1); tcrit1
tcrit2 = -tcrit1; tcrit2

statistics <- data.frame(
  Statistic = c("Average", "Standard Deviation", "Count(n)", "T-statistic", "Tcrit1", "Tcrit2"),
  Values = c(xbar, stddev, n, tstat, tcrit1, tcrit2)
)

print(statistics)

install.packages("ggplot2")
library(ggplot2)

t_vals = seq(-4, 4, length = 1000)
t_dist = dt(t_vals, df = n-1)
t_df = data.frame(t = t_vals, density = t_dist)

ggplot(t_df, aes(x = t, y = density))+
  geom_line(color = "blue", size = 1)+
  
  geom_area(data = subset(t_df, t >= tcrit1), aes(x = t, y = density), fill = "red", alpha = 0.4)+
  geom_area(data = subset(t_df, t <= tcrit2), aes(x = t, y = density), fill = "red", alpha = 0.4)+
  
  geom_vline(xintercept = c(tcrit2, tcrit1), linetype = "solid", color = "red")+
  geom_vline(xintercept = tstat, linetype = "dashed", color = "darkgreen", size = 1)+
  
  annotate("text", x = tstat, y = 0.05, label = paste0("tstat = ", round(tstat, 2)), 
          vjust = -1, color = "darkgreen")+
  annotate("text", x = tcrit1, y = 0.05, label = paste0("tcrit = ", round(tcrit1, 3)),
           vjust = -1, color = "red")+
  annotate("text", x = tcrit2, y = 0.05, label = paste0("tcrit = ", round(tcrit2, 3)),
          vjust = -1, color = "red")+
  
  labs(x = "t-values", y = "Density", title = "t-distribution with rejection areas")+
  
  theme_minimal()
  
  
  

#2. Is the average driver age Greater than 30?
#We will test this using a one sample, right-tailed t-test

#H₀ (Null Hypothesis): The mean driver age is less than or equal to 30
#H₁ (Alternative Hypothesis): The mean driver age is greater than 30

x0 <- 30
xbar = mean(dataset$`Driver Age`); xbar
stddev = sd(dataset$`Driver Age`); stddev
n = nrow(dataset); n
tstat = (xbar - x0) / (stddev/sqrt(n)); tstat
tcrit = qt(1-0.05, df = n-1); tcrit

statistics <- data.frame(
  Statistic = c("Average", "Standard Deviation", "Sample size", "T-statistic", "Tcrit"),
  Values = c(xbar, stddev, n, tstat, tcrit)
)

print(statistics)

t_vals = seq(-5, 5, length = 1000)
t_dt = dt(t_vals, df = n-1)
t_df = data.frame(t = t_vals, density = t_dt)

ggplot(t_df, aes(x = t, y = density))+
  geom_line(color = "blue", size = 1)+
  
  geom_area(data = subset(t_df, t >= tcrit), aes(x = t, y = density), fill = "red", alpha = 0.4)+
  
  geom_vline(xintercept = tcrit, color = "red", linetype = "solid")+
  annotate("text", x = tcrit, y = 0.05, label = paste0("tcrit = ", round(tcrit, 2)), 
           vjust = -1, color = "red")+
  
  geom_vline(xintercept = tstat, color = "darkgreen", linetype = "dashed", size = 1)+
  annotate("text", x = tstat, y = 0.05, label = paste0("tstat = ", round(tstat, 2)),
           vjust = -1, color = "darkgreen") +
  
  labs(x = "t-values", y = "Density", title = "t-distribution with rejection areas")+
  theme_minimal()

  

#3. C) Do drivers under 25 have an average finishing position lower than 10? This tests the competitiveness of younger drivers. 
#We will conduct a one-sample, left-tailed t-test

#Null Hypothesis (H₀): μ >= 10 (Young drivers' average finishing position is 10th or worse)
#Alternative Hypothesis (H₁): μ < 10 (Young drivers' average finishing position is significantly better than 10th)

young_drivers_data = dataset[dataset$`Driver Age`<25, ]

x0 <- 10
xbar = mean(young_drivers_data$`Finishing position`); xbar
stddev = sd(young_drivers_data$`Finishing position`); stddev
n = nrow(young_drivers_data); n
tstat = (xbar - x0) / (stddev/sqrt(n)); tstat
tcrit = -qt(1-0.05,  df = n-1); tcrit

statistics = data.frame(
  Statistic = c("Average", "Standard deviation", "Sample size", "T statistic", "t crit"),
  Values = c(xbar, stddev, n, tstat, tcrit)
)

print(statistics)

t_vals = seq(-4, 4, length = 1000)
t_dt = dt(t_vals, df = n-1)
t_df = data.frame(t = t_vals, density = t_dt)

ggplot(t_df, aes(x = t, y = density))+
  geom_line(color = "blue", size = 2)+
  
  geom_area(data = subset(t_df, t <= tcrit), aes(x = t, y = density), fill = "red", alpha = 0.4)+
  
  geom_vline(xintercept = tcrit, color = "red", linetype = "solid")+
  annotate("text", x = tcrit, y = 0.05, label = paste0("tcrit = ", round(tcrit, 2)),
            vjust = -1, color = "red") +
  
  geom_vline(xintercept = tstat, color = "darkgreen", linetype = "dashed", size = 1)+
  annotate("text", x = tstat, y = 0.05, label = paste0("tstat = ", round(tstat, 2)),
           vjust = -1, color = "darkgreen")+
  
  labs(x = "t-values", y = "Density", title = "t-distribution with rejection areas")+
  theme_minimal()



#III. ANOVA ANALYSIS

#1. Is there a significant difference between the mean driver age across constructor tiers?
  
#Null hypothesis (H₀): All group means are equal.
#Alternative hypothesis (H₁): At least one group mean is different.

anova_results = aov(`Driver Age` ~ `Constructor Tier`, data = dataset)
summary(anova_results)

ggplot(dataset, aes(x = `Driver Age`, y = `Constructor Tier`, fill = `Constructor Tier`)) +
         geom_boxplot()+
  labs(title = "Mean driver age by constructor tier")
  
  

#2. Is there a significant difference between the mean qualifying position across age categories?
  
#Null hypothesis (H₀): All group means are equal.
#Alternative hypothesis (H₁): At least one group mean is different.

quali_by_age <-list(
  Rookie = dataset[dataset$`Driver Age` <= 25, "Quali position"],
  Experienced = dataset[dataset$`Driver Age` >25 & dataset$`Driver Age` <33, "Quali position"],
  Veteran = dataset[dataset$`Driver Age`>=33, "Quali position"]
)

quali_by_age_vec <- lapply(quali_by_age, function(df) df[[1]])

quali_df <- stack(quali_by_age_vec)
colnames(quali_df) <- c("Qualifying position", "Age category")
print(quali_df)

anova_results = aov(`Qualifying position` ~ `Age category`, data = quali_df)
summary(anova_results)

ggplot(quali_df, aes(x =`Qualifying position`, y = `Age category`, fill = `Age category`))+
  geom_boxplot()+
  labs(title = "Mean qualifying position by age category")

#3. Is there a significant difference between the mean finishing position of drivers across constructor tiers?

#Null hypothesis (H₀): All group means are equal.
#Alternative hypothesis (H₁): At least one group mean is different.

anova_results = aov(`Finishing position` ~ `Constructor Tier`, data = dataset)
summary(anova_results)

ggplot(dataset, aes(x = `Finishing position`, y = `Constructor Tier`, fill = `Constructor Tier`))+
  geom_boxplot()+
  labs(title = "Mean finishing position by constructor tier")

#IV. Simple linear regression

#1.The influence of a driver's constructor form in a season, given by the final position of the constructor in that season's constructors' championship, on the driver's final position in a race

lm_model = lm(`Finishing position` ~ `Constructor's season finishing position`, data = dataset)
summary(lm_model)

ggplot(dataset, aes(x = `Finishing position`, y = `Constructor's season finishing position`))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Driver Race Finishing Position vs Constructor Finishing Position",
       x = "Constructor's Season Finishing Position",
       y = "Driver Race Finishing Position") 
  

#2. Impact of driver age on the finishing position

lm_model = lm(`Finishing position` ~ `Driver Age`, data = dataset)
summary(lm_model)

ggplot(dataset, aes(x = `Finishing position`, y = `Driver Age`))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Driver Race Finishing Position vs Driver Age",
       x = "Driver Age",
       y = "Driver Race Finishing Position")

#3. This analysis focuses on the impact of the configuration of the circuit. whether it is a street circuit or a permanent one. Permanent circuit has been encoded with 1, and street circuit with 0. 

dataset$`Permanent circuit` = ifelse(dataset$`Street or permanent circuit` == "Permanent circuit", 1, 0)
lm_model = lm(`Finishing position` ~ `Permanent circuit`, data = dataset)
summary(lm_model)

ggplot(dataset, aes(x = `Finishing position`, y = `Permanent circuit`))+
  geom_point()+
  geom_smooth(color = "red", method = "lm", se = FALSE)+
  labs(title = "Driver Race Finishing Position vs Permanent Circuit",
       x = "Permanent Circuit",
       y = "Driver Race Finishing Position")
  

#4. Impact of qualifying (starting) position on finishing position

lm_model = lm(`Finishing position` ~ `Quali position`, data = dataset)
summary(lm_model)

ggplot(dataset, aes(x = `Finishing position`, y = `Quali position`))+
  geom_point()+
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  labs(title = "Driver Race Finishing Position vs Qualifying position",
       x = "Qualifying position",
       y = "Driver Race Finishing Position")

#V. Multiple linear regression

dataset$`Quali position X Constructor's season finishing position` = dataset$`Quali position` * dataset$`Constructor's season finishing position`
print(dataset$`Quali position X Constructor's season finishing position`)
lm_model = lm(`Finishing position` ~ `Quali position`+`Average position change for the season`+`Constructor's season finishing position`+`Dry race`+`Permanent circuit`+ `Quali position X Constructor's season finishing position`, data = dataset)
summary(lm_model)
