## -----------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
#install.packages('DT')
library(DT)


## -----------------------------------------------------------------------------
expense <- read.csv("BEI_Operating_Expenses.csv", header = TRUE)
summary(expense)

#View(expense)

#remove the meta-categories(?)

#change column categories
expense$Meta_Category_Name = as.factor(expense$Meta_Category_Name)
expense$Meta_Category_Number = as.factor(expense$Meta_Category_Number)
expense$Category_Name = as.factor(expense$Category_Name)
expense$Category_Number = as.factor(expense$Category_Number)
expense$Month = as.factor(expense$Month)
#expense$Amount = as.numeric(as.factor(expense$Amount))

summary(expense)
View(expense)


## -----------------------------------------------------------------------------

#summary by month
expense_summary <- expense %>% group_by(Category_Name, Category_Number, Month) %>%
                                  summarise(Cost = sum(Amount))

wide_expense <- pivot_wider(expense_summary, 
                            names_from = Month, 
                            values_from = Cost)

#View(wide_expense)

wide_expense[c("Category_Name", "January", "February", "March", "April", "May ", "June ", "July")]


#summary by category
expense_cat <- expense %>% group_by(Category_Name) %>%
                              summarise(Total = sum(Amount))

#View(expense_cat)


#Join expense_cat and wide_expense
Expense_Summary_1 <- full_join(wide_expense, expense_cat)


## -----------------------------------------------------------------------------

expense_table <- datatable(Expense_Summary_1, rownames = FALSE, class = 'cell-border stripe')

DT::saveWidget(expense_table, "expense_table.html")


## -----------------------------------------------------------------------------

#summary by month
month_expense_summary <- expense %>% group_by(Month) %>%
                                  summarise(Cost = sum(Amount))

wide_month_expense <- pivot_wider(month_expense_summary, 
                            names_from = Month, 
                            values_from = Cost)

#View(month_expense_summary)

wide_month_expense$Category_Name = "Month_total"
wide_month_expense$Category_Number = "00000"
wide_month_expense$Total = sum(month_expense_summary$Cost)


wide_month_expense <- wide_month_expense[c("Category_Name", "Category_Number", "January", "February", "March", "April", "May ", "June ", "July", "Total")]

View(wide_month_expense)

#summary by category
#expense_cat <- expense %>% group_by(Category_Name) %>%
#                              summarise(Total = sum(Amount))

#View(expense_cat)


#Join expense_cat and wide_expense
Month_Expense_Summary_1 <- full_join(wide_month_expense, Expense_Summary_1)


## -----------------------------------------------------------------------------

month_expense_table <- datatable(Month_Expense_Summary_1, rownames = FALSE, class = 'cell-border stripe')
month_expense_table

DT::saveWidget(month_expense_table, "month_expense_table.html")


## -----------------------------------------------------------------------------

average_table <- expense %>% group_by(Category_Name) %>%
                        summarise(min = min(Amount),
                                  average = round(mean(Amount), digits=2),
                                  sd = round(sd(Amount), digits=2), 
                                  med = median(Amount),
                                  max = max(Amount))

average_table  

# make a standard error column

average_table$se = round(average_table$sd/(sqrt(7)), digits=2)

average_table

average_table_sub <- average_table[c("Category_Name", "min", "average", "se", "med", "max")]

average_table_sub


## -----------------------------------------------------------------------------
avg_expense_table <- datatable(average_table_sub, rownames = FALSE, class = 'cell-border stripe')

avg_expense_table

DT::saveWidget(avg_expense_table, "avg_expense_table.html")


## -----------------------------------------------------------------------------
expense_plot <- ggplot(expense, aes(x=Category_Name, y=Amount)) +
  geom_point(cex = 2, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data = average_table, aes(x = Category_Name, y = average))

expense_plot

all_expenses_plot <- 
  expense_plot + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Monthly Expenses Jan - July 2021",
       x = "Expense Category", 
       y = "Amount (USD)")
all_expenses_plot

ggsave(all_expenses_plot, file = "all_expenses.png")


## -----------------------------------------------------------------------------
expense$Category_Name
  
expenses_minus_farmlease <- expense[-c(225:231),]
average_minus_farmlease <- average_table[-(13),]

expense_plot_minus_farmlease <- ggplot(expenses_minus_farmlease, aes(x=Category_Name, y=Amount)) +
  geom_point(cex = 2, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data = average_minus_farmlease, aes(x = Category_Name, y = average))

expense_plot_minus_farmlease

no_farmlease_expenses_plot <- 
  expense_plot_minus_farmlease + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Monthly Expenses Jan - July 2021 (minus farmlease expense)",
       x = "Expense Category", 
       y = "Amount (USD)")
no_farmlease_expenses_plot

ggsave(no_farmlease_expenses_plot, file = "no_farmlease_expenses.png")


## -----------------------------------------------------------------------------
expenses_below_10k <- expense[expense$Amount<10000,]
summary(expenses_below_10k)

average_below_10k <- average_table[average_table$average<10000,]

expense_plot_below_10k <- ggplot(expenses_below_10k, aes(x=Category_Name, y=Amount)) +
  geom_point(cex = 2, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data = average_below_10k, aes(x = Category_Name, y = average))

expense_plot_below_10k

below_10k_expenses_plot <- 
  expense_plot_below_10k + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Monthly Expenses <$10k Jan - July 2021",
       x = "Expense Category", 
       y = "Amount (USD)")
below_10k_expenses_plot

ggsave(below_10k_expenses_plot, file = "below_10k_expenses.png")


## -----------------------------------------------------------------------------
names(average_table)

averages <- average_table[,c(1, 3, 4)]
view(averages)

averages_table <- datatable(averages, rownames = FALSE, class = 'cell-border stripe')
DT::saveWidget(averages_table, "month_expense_table.html")


## -----------------------------------------------------------------------------
projected <- lm(Amount ~ Category_Name, data = expense)
summary(projected)

##okay so this was pointless because it does the group intercept 


## -----------------------------------------------------------------------------
average_monthly_expenses <- mean(month_expense_summary$Cost)
average_monthly_expenses


## -----------------------------------------------------------------------------
income <- read.csv("income_janjuly2021.csv", header = TRUE)
summary(income)

income$Category_Name = as.factor(income$Category_Name)
income$Category_Number = as.factor(income$Category_Number)
income$Month = as.factor(income$Month)


## -----------------------------------------------------------------------------
#summary by month
income_summary <- income %>% group_by(Category_Name, Category_Number, Month) %>%
                                  summarise(Cost = sum(Amount))

wide_income <- pivot_wider(income_summary, 
                            names_from = Month, 
                            values_from = Cost)

#View(wide_income)

wide_income <- wide_income[c("Category_Name", "January", "February", "March", "April", "May", "June", "July")]


#summary by category
income_cat <- income %>% group_by(Category_Name) %>%
                              summarise(Total = sum(Amount))

View(income_cat)


#Join income_cat and wide_income
income_Summary_1 <- full_join(wide_income, income_cat)


#make a datatable
month_income_table <- datatable(income_Summary_1, rownames = FALSE, class = 'cell-border stripe')

DT::saveWidget(month_income_table, "month_income_table.html")



## -----------------------------------------------------------------------------
#summary by month
month_income_summary <- income %>% group_by(Month) %>%
                                  summarise(Cost = sum(Amount))

wide_month_income <- pivot_wider(month_income_summary, 
                            names_from = Month, 
                            values_from = Cost)

#View(month_income_summary)

wide_month_income$Category_Name = "Month_total"
wide_month_income$Category_Number = "00000"
wide_month_income$Total = sum(month_income_summary$Cost)


wide_month_income <- wide_month_income[c("Category_Name", "Category_Number", "January", "February", "March", "April", "May", "June", "July", "Total")]

View(wide_month_income)

#summary by category
#income_cat <- income %>% group_by(Category_Name) %>%
#                              summarise(Total = sum(Amount))

#View(income_cat)


#Join income_cat and wide_income
Month_income_Summary_1 <- full_join(wide_month_income, income_Summary_1)

Month_income_Summary_1

#make a datatable
month_income_table <- datatable(Month_income_Summary_1, rownames = FALSE, class = 'cell-border stripe')
month_income_table

DT::saveWidget(month_income_table, "month_income_table.html")


## -----------------------------------------------------------------------------
average_monthly_income <- mean(month_income_summary$Cost)
average_monthly_income


## -----------------------------------------------------------------------------
average_income_table <- income %>% group_by(Category_Name) %>%
                        summarise(min = min(Amount),
                                  average = round(mean(Amount), digits=2),
                                  sd = round(sd(Amount), digits=2), 
                                  med = median(Amount),
                                  max = max(Amount))

average_income_table  

# make a standard error column

average_income_table$se = round(average_income_table$sd/(sqrt(7)), digits=2)

average_income_table <- average_income_table[c("Category_Name", "min", "average", "se", "med", "max")]

average_income_table


## -----------------------------------------------------------------------------
avg_income_dattable <- datatable(average_income_table, rownames = FALSE, class = 'cell-border stripe')

avg_income_dattable

DT::saveWidget(avg_income_dattable, "avg_income_table.html")


## -----------------------------------------------------------------------------
income_plot <- ggplot(income, aes(x=Category_Name, y=Amount)) +
  geom_point(cex = 2, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data = average_income_table, aes(x = Category_Name, y = average))

income_plot

all_income_plot <- 
  income_plot + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Monthly Income Jan - July 2021",
       x = "Income Category", 
       y = "Amount (USD)")
all_income_plot

ggsave(all_income_plot, file = "all_income.png")


## -----------------------------------------------------------------------------
average_monthly_income - average_monthly_expenses

