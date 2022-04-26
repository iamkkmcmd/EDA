# CLear everything 
rm(list = ls())
graphics.off()

# Load the libraries
library(tidyverse)
library(lubridate)
library(tidyr)
library(rstatix)
library(ggpubr)
library(stringr)

# Set working directory
setwd("C:/Users/System Administrator/Desktop/UPI PROJECT")

# Load the data
upi <- read.csv("./data/UPI apps transaction data in 2021.csv")

# View the data
# View(upi)
upi %>% head()

# Change the variable name to easily work with
# UPI Banks -> upi_banks
# Volume Mn By Costumers -> cvol_mn
# Volume Cr By Costumers -> cval_cr
# Volume Mn -> vol_mn
# Volume Cr -> val_cr
# Month -> month
# Year -> year

# Change the column names
names(upi) <- c('upi_bank','cvol_mn','cval_cr','vol_mn','val_cr','month','year')

# Lets check variable data type
upi %>% glimpse()

# change the bank name as factor
upi$upi_bank <- as.factor(upi$upi_bank)
upi$month <- factor(upi$month, labels = month.abb, ordered = T)

# change month and year as date time format
upi$year <- year(upi$year)
upi$month <- month(upi$month)

# Lets check variable data type
upi %>% glimpse()

# How many year's data is there
unique(upi$year)
# As there are one year 2021, then we can remove the year column
upi <- upi %>% select(-'year')

# are there are any missing value
sum(is.na(upi))

# How many banks are available
length(levels(upi$upi_bank))

# dimension of the data
dim(upi)
# ----------------------- Data Cleaning End -----------------------------------

# write.csv(upi, './data/upi_final_data.csv')
upi <- read.csv('./data/upi_final_data.csv')

# Let's check which bank have not 12 months data
banks <- upi %>% 
  group_by(upi_bank, month) %>% 
  summarise(sum = n()) %>% 
  summarise(count = sum(sum)) %>% 
  filter(count == 12)

upi <- upi %>% 
  filter(upi_bank %in% banks$upi_bank)

# Top 4 bank have high volume
top_4_vol <- upi %>% 
  group_by(upi_bank) %>% 
  summarise(tot_vol = sum(cvol_mn)) %>% 
  arrange(desc(tot_vol)) %>% 
  top_n(4)

# Top 4 bank have high value
top_4_value <- upi %>% 
  group_by(upi_bank) %>% 
  summarise(tot_value = sum(val_cr)) %>% 
  arrange(desc(tot_value)) %>% 
  top_n(4)


# Plotting ----------------------------------------------------------------
# ---------------------------- Volume -------------------------------------
# Boxplot of volumes 
upi %>% 
  group_by(upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cvol_mn), y = cvol_mn))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Volumn (Million)",
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Boxplot of top 4 banks having maximum volume
upi %>% 
  group_by(upi_bank) %>% 
  filter(upi_bank %in% top_4_vol$upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cvol_mn), y = cvol_mn))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Volumn (Million)",
       title = '') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.title = element_text(hjust = 0.5))
# Boxplot without top 4 banks
upi %>% 
  group_by(upi_bank) %>% 
  filter(!upi_bank %in% top_4_vol$upi_bank) %>%
  ggplot(aes(x = reorder(upi_bank, -cvol_mn), y = cvol_mn))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Volumn (Million)",
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Month wise comparison
upi %>%
  group_by(upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cvol_mn), y = cvol_mn, 
             fill = factor(month, labels = month.abb))) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x = "Upi Banks", y = "Volumn (Million)", fill = 'Month',
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = 'Paired')
# Month wise comparison of top 4 bank
upi %>% 
  group_by(upi_bank) %>% 
  filter(upi_bank %in% top_4_vol$upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cvol_mn), y = cvol_mn, 
             fill = factor(month, labels = month.abb))) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) + 
  labs(x = "Upi Banks", y = "Volumn (Million)", fill = 'Month',
       title = '') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_brewer(palette = 'Paired')

# -------------------------------- Value ---------------------------------------
# Boxplot of Values 
upi %>% 
  group_by(upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cval_cr), y = cval_cr))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Value (Crore)",
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Boxplot of top 4 banks having maximum Value
upi %>% 
  group_by(upi_bank) %>% 
  filter(upi_bank %in% top_4_value$upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cval_cr), y = cval_cr))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Value (Crore)",
       title = '') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.title = element_text(hjust = 0.5))
# Boxplot without top 4 banks
upi %>% 
  group_by(upi_bank) %>% 
  filter(!upi_bank %in% top_4_value$upi_bank) %>%
  ggplot(aes(x = reorder(upi_bank, -cval_cr), y = cval_cr))+
  geom_boxplot() + 
  labs(x = "Upi Banks", y = "Value (Crore)",
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Month wise comparison
upi %>%
  group_by(upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cval_cr), y = cval_cr, 
             fill = factor(month, labels = month.abb))) +
  geom_bar(stat = 'identity', position = 'stack') + 
  labs(x = "Upi Banks", y = "Value (Crore)", fill = 'Month',
       title = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = 'Paired')
# Month wise comparison of top 4 bank
upi %>% 
  group_by(upi_bank) %>% 
  filter(upi_bank %in% top_4_value$upi_bank) %>% 
  ggplot(aes(x = reorder(upi_bank, -cval_cr), y = cval_cr, 
             fill = factor(month, labels = month.abb))) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) + 
  labs(x = "Upi Banks", y = "Value (Crore)", fill = 'Month',
       title = '') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_brewer(palette = 'Paired')


# ANOVA (volume)-------------------------------------------------------------------

anova_vol <- upi %>% 
  filter(upi_bank %in% top_4_vol$upi_bank) %>% 
  select(upi_bank, month, cvol_mn) 
# summary statistics
anova_vol %>% 
  group_by(upi_bank) %>% 
  get_summary_stats(cvol_mn, type = 'mean_sd')
# Visualization
ggboxplot(anova_vol, x = 'upi_bank', y = 'cvol_mn') +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 10)) +
  labs(x = 'UPI Banks', y = 'Volume (in crore)', title = 'Boxplot of volumne (in crore)') +
  theme(plot.title = element_text(hjust = 0.5))
# Identify outliers
anova_vol %>% 
  group_by(upi_bank) %>% 
  identify_outliers(cvol_mn)
# There are no outlier in the data.

# Normality assumption (Model residual plot)
model <- lm(cvol_mn ~ upi_bank, data = anova_vol)
ggqqplot(residuals(model)) + 
  labs(title = 'Normal QQ-plot of Residuals') +
  theme(plot.title = element_text(hjust = 0.5))
# In qqplot all the points fall approximately along the reference 
# line. Also approximately all points are in the 2XSE region. So, 
# the data statisfy the normality assumption.
shapiro_test(residuals(model))
#---------------- Althogh sample size is not enough to do -------------------------
anova_vol %>% 
  group_by(upi_bank) %>% 
  shapiro_test(cvol_mn)
# All p-values are greater than 0.05, then we failed to reject the null hypothesis,
# we conclude that group-wise data is normally distributed.
ggqqplot(anova_vol, 'cvol_mn', facet.by = 'upi_bank') + 
  labs(title = 'Normal QQ-plot of Residuals', subtitle = '(Bank wise)') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# ---------------------------------------------------------------------------------

par(mfrow = c(3,2))
plot(model, 1:6)
# In the plot above, there is no evident relationships between residuals and 
# fitted values (the mean of each groups), which is good. So, we can assume the 
# homogeneity of variances.

summary(model)
model
anova(model)

# Post-hoc test
anova_vol %>% tukey_hsd(cvol_mn ~ upi_bank)

# It can be seen form the output that all differences are significant.


# ANOVA (value)--------------------------------------------------------------------
anova_value <- upi %>% 
  filter(upi_bank %in% top_4_value$upi_bank) %>% 
  select(upi_bank, month, cval_cr) 
# summary statistics
anova_value %>% 
  group_by(upi_bank) %>% 
  get_summary_stats(cval_cr , type = 'mean_sd')
# Visualization
ggboxplot(anova_value, x = 'upi_bank', y = 'cval_cr') +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 10)) +
  labs(x = 'UPI Banks', y = 'Value (in crore)', title = 'Boxplot of value (in crore)') +
  theme(plot.title = element_text(hjust = 0.5))
# Identify outliers
anova_value %>% 
  group_by(upi_bank) %>% 
  identify_outliers(cval_cr)
# There are no outlier in the data.

# Normality assumption (Model residual plot)
model <- lm(cval_cr  ~ upi_bank, data = anova_value)
ggqqplot(residuals(model)) + 
  labs(title = 'Normal QQ-plot of Residuals') +
  theme(plot.title = element_text(hjust = 0.5))
# In qqplot all the points fall approximately along the reference 
# line. Also approximately all points are in the 2XSE region. So, 
# the data statisfy the normality assumption.
shapiro_test(residuals(model))
#---------------- Althogh sample size is not enough to do -------------------------
anova_value %>% 
  group_by(upi_bank) %>% 
  shapiro_test(cval_cr)
# All p-values are greater than 0.05, then we failed to reject the null hypothesis,
# we conclude that group-wise data is normally distributed.
ggqqplot(anova_value, 'cval_cr', facet.by = 'upi_bank') + 
  labs(title = 'Normal QQ-plot of Residuals', subtitle = '(Bank wise)') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# ---------------------------------------------------------------------------------

par(mfrow = c(3,2))
plot(model, 1:6)
# In the plot above, there is no evident relationships between residuals and 
# fitted values (the mean of each groups), which is good. So, we can assume the 
# homogeneity of variances.

summary(model)
model
anova(model)

# Post-hoc test
anova_value %>% tukey_hsd(cval_cr  ~ upi_bank)

# It can be seen form the output that all differences except between 
# cred and paytm bank are significant.



