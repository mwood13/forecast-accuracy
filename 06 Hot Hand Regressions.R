# run regressions to test for hot hand 

library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest)

df <- fread("Data//scanner_with_past.csv")%>%as.data.frame()

df <- df %>% mutate(
  month =  month(ymd(df$week_end)),
  year = year(ymd(df$week_end))
)


# build out regression variables
df <- df %>% mutate(
  
  acc_streak = ifelse(hit_streak > 0, hit_streak, -1*miss_streak),
    
  anyTG_miss = anyTG*miss_streak,
  land_miss = Landfall*miss_streak,
  anyTG_miss_sq = anyTG*miss_streak*miss_streak,
  land_miss_sq = Landfall*miss_streak*miss_streak,
  
  anyTG_hit = anyTG*hit_streak,
  land_hit = Landfall*hit_streak,
  anyTG_hit_sq = anyTG*hit_streak*hit_streak,
  land_hit_sq = Landfall*hit_streak*hit_streak,
  
  anyTG_acc = anyTG*acc_streak,
  land_acc = Landfall*acc_streak,
  anyTG_acc_sq = anyTG*acc_streak*acc_streak,
  land_acc_sq = Landfall*acc_streak*acc_streak,
  
  anyTG_acc_cont = anyTG*acc_streak_cont,
  land_acc_cont = Landfall*acc_streak_cont,
  anyTG_acc_cont_sq = anyTG*acc_streak*acc_streak_cont,
  land_acc_cont_sq = Landfall*acc_streak*acc_streak_cont,
  
  
  earlyTG_acc = earlyTG*acc_streak,
  midTG_acc = midTG*acc_streak,
  lateTG_acc = lateTG*acc_streak,
  
  earlyTG_acc_sq = earlyTG*acc_streak*acc_streak,
  midTG_acc_sq = midTG*acc_streak*acc_streak,
  lateTG_acc_sq = lateTG*acc_streak*acc_streak,
  
)


# just using hit streak

lm1 <- feols(data = df, 
            log(total_rev) ~ anyTG + Landfall+
              anyTG_hit + land_hit + 
              temp_mean |
              fips + month + year, 
                cluster = c("fips"))

lm2 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_hit + land_hit + 
        anyTG_hit_sq + land_hit_sq + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


# just using miss streak

lm3 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_miss + land_miss + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


lm4 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_miss + land_miss + 
        anyTG_miss_sq + land_miss_sq + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


# using accuracy streak

lm5 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_acc + land_acc + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


lm6 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_acc + land_acc + 
        anyTG_acc_sq + land_acc_sq + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


# continuous accuracy

lm7 <- feols(data = df, 
             log(total_rev) ~ anyTG + Landfall+
               anyTG_acc_cont + land_acc_cont + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


lm8 <- feols(data = df, 
             log(total_rev) ~ anyTG + Landfall+
               anyTG_acc_cont + land_acc_cont + 
               anyTG_acc_cont_sq + land_acc_cont_sq + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


esttex(lm1, lm2, lm3, lm4, lm7, lm8,
       headers =  c("Hit", "Hit", "Miss", "Miss", "Accuracy", "Accuracy"),
       title = "Effect of Streaks of Forecast Outcomes", 
       fitstat = ~n+r2)


# hit plus miss


lm7 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_hit + land_hit + 
        anyTG_miss + land_miss + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


lm8 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG_hit + land_hit + 
        anyTG_hit_sq + land_hit_sq + 
        anyTG_miss + land_miss + 
        anyTG_miss_sq + land_miss_sq + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


esttex(lm5, lm6, lm7, lm8, 
      # headers =  c("Hit", "Hit", "Miss", "Miss", "Accuracy", "Accuracy"),
       title = "Effect of Streaks of Forecast Outcomes", 
       fitstat = ~n+r2)



# timing of forecasts with accuracy --------------------------------------------------------

lm9 <- feols(data = df, 
             log(total_rev) ~ earlyTG + Landfall+
               earlyTG_acc + land_acc + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


lm10 <- feols(data = df, 
             log(total_rev) ~ earlyTG + Landfall+
               earlyTG_acc + land_acc + 
               earlyTG_acc_sq + land_acc_sq + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


lm11 <- feols(data = df, 
             log(total_rev) ~ midTG + Landfall+
               midTG_acc + land_acc + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


lm12 <- feols(data = df, 
              log(total_rev) ~ midTG + Landfall+
                midTG_acc + land_acc + 
                midTG_acc_sq + land_acc_sq + 
                temp_mean |
                fips + month + year, 
              cluster = c("fips"))




lm13 <- feols(data = df, 
             log(total_rev) ~ lateTG + Landfall+
               lateTG_acc + land_acc + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"))


lm14 <- feols(data = df, 
              log(total_rev) ~ lateTG + Landfall+
                lateTG_acc + land_acc + 
                lateTG_acc_sq + land_acc_sq + 
                temp_mean |
                fips + month + year, 
              cluster = c("fips"))



esttex(lm5, lm6, lm9, lm10, lm11, lm12, lm13, lm14 ,
       # headers =  c("Hit", "Hit", "Miss", "Miss", "Accuracy", "Accuracy"),
       title = "Effect of Streaks of Forecast Outcomes by Timing", 
       fitstat = ~n+r2)
