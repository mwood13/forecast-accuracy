# run difference estimators

library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest, lubridate)

df <- fread("Data//scanner_hur.csv")%>% as.data.frame()


df <- df %>% mutate(
  month =  month(ymd(df$week_end)),
  year = year(ymd(df$week_end))
)

# regression for treatment assignment ------------------------------------------

lm1 <- feols(data = df, 
      log(total_rev) ~  Landfall + temp_mean|
        fips + month + year, 
      cluster = c("fips"))


# any treatment regression
lm2 <- feols(data = df, 
       log(total_rev) ~ anyTG + Landfall + temp_mean|
         fips + month + year, 
         cluster = c("fips"))

# early treatment regression
lm3 <- feols(data = df, 
           log(total_rev) ~ earlyTG + Landfall + temp_mean|
           fips + month + year, 
           cluster = c("fips"))


# mid treatment regression
lm4 <- feols(data = df, 
      log(total_rev) ~ midTG + Landfall + temp_mean|
        fips + month + year, 
      cluster = c("fips"))


# late treatment regression
lm5 <- feols(data = df, 
      log(total_rev) ~ lateTG + Landfall + temp_mean|
        fips + month + year, 
      cluster = c("fips"))



# early mid and late in same regression
lm6 <- feols(data = df, 
      log(total_rev) ~ earlyTG +midTG + lateTG + Landfall + temp_mean|
        fips + month + year, 
      cluster = c("fips"))



esttex(lm2, lm3, lm4, lm5, lm6, 
       #headers =  c("1", "2", "3", "4", "5", "6", "7", "8"),
       title = "Effect of Current Forecast on Sales", 
       fitstat = ~n+r2)

