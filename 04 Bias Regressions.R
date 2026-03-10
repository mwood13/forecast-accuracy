# recency bias regressions

library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest, car)

df <- fread("Data//scanner_with_past.csv")%>%as.data.frame()


df <- df %>% mutate(
  month =  month(ymd(df$week_end)),
  year = year(ymd(df$week_end)),
  time_dif = as.numeric((week_end - last_date)/365)
)


# any treatment regression
lm1 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall + temp_mean|
        fips + month + year, 
      cluster = c("fips"))

lm2 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
       # anyTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
        # anyTG:last_Hit_noTG + Landfall:last_Hit_noTG +
        temp_mean|
        fips + month + year, 
      cluster = c("fips")
)


lm3 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        #anyTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
        anyTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
        # anyTG:last_Hit_noTG + Landfall:last_Hit_noTG +
        temp_mean|
        fips + month + year, 
      cluster = c("fips")
)



lm4 <- feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
        anyTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
       # anyTG:last_Hit_noTG + Landfall:last_Hit_noTG +
        temp_mean|
        fips + month + year, 
       cluster = c("fips")
     )



esttex(lm1, lm2, lm3, lm4,
       #headers =  c("1", "2", "3", "4", "5", "6", "7", "8"),
       title = "Effect of Most Recent Forecast Outcome", 
       fitstat = ~n+r2)



feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG:last_earlyTG_Hit + Landfall:last_earlyTG_Hit +
        anyTG:last_earlyTG_NoHit + Landfall:last_earlyTG_NoHit +
       # anyTG:last_Hit_noEarlyTG + Landfall:last_Hit_noEarlyTG +
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG:last_midTG_Hit + Landfall:last_midTG_Hit +
        anyTG:last_midTG_NoHit + Landfall:last_midTG_NoHit +
       # anyTG:last_Hit_noMidTG + Landfall:last_Hit_noMidTG +
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


feols(data = df, 
      log(total_rev) ~ anyTG + Landfall+
        anyTG:last_lateTG_Hit + Landfall:last_lateTG_Hit +
        anyTG:last_lateTG_NoHit + Landfall:last_lateTG_NoHit +
       # anyTG:last_Hit_noLateTG + Landfall:last_Hit_noLateTG +
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))



# early assignment regressions

lm6 <- feols(data = df, 
      log(total_rev) ~ earlyTG + Landfall+
        earlyTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
        earlyTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
        #earlyTG:last_Hit_noTG + Landfall:last_Hit_noTG + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


# mid tratemnt assignment


lm7 <- feols(data = df, 
      log(total_rev) ~ midTG + Landfall+
        midTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
        midTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
       # midTG:last_Hit_noTG + Landfall:last_Hit_noTG + 
        temp_mean |
        fips + month + year, 
      cluster = c("fips"))


# late treatment assignment

lm8 <- feols(data = df, 
      log(total_rev) ~ lateTG + Landfall+
        lateTG:last_anyTG_Hit + Landfall:last_anyTG_Hit +
        lateTG:last_anyTG_NoHit + Landfall:last_anyTG_NoHit +
       # lateTG:last_Hit_noTG + Landfall:last_Hit_noTG + 
        temp_mean|
        fips + month + year, 
      cluster = c("fips"))




esttex(lm4, lm6, lm7, lm8,
       headers =  c("Any", "Early", "Middle", "Late"),
       title = "Effect of Most Recent Forecast Outcome", 
       fitstat = ~n+r2)



# run f-tests to see if the betas are the same for hit and miss ----------------

ftest <- linearHypothesis(lm4, c("anyTG:last_anyTG_Hit = anyTG:last_anyTG_NoHit", 
                        "Landfall:last_anyTG_Hit = Landfall:last_anyTG_NoHit") )


ftest_threat <- linearHypothesis(lm4, c("anyTG:last_anyTG_Hit = anyTG:last_anyTG_NoHit" 
                                        ) )


ftest_land <- linearHypothesis(lm4, c(
                                      "Landfall:last_anyTG_Hit = Landfall:last_anyTG_NoHit") )



# early middle and late tests

ftestE <- linearHypothesis(lm6, c("earlyTG:last_anyTG_Hit = earlyTG:last_anyTG_NoHit", 
                                 "Landfall:last_anyTG_Hit = Landfall:last_anyTG_NoHit") )

ftestM <- linearHypothesis(lm7, c("midTG:last_anyTG_Hit = midTG:last_anyTG_NoHit", 
                                 "Landfall:last_anyTG_Hit = Landfall:last_anyTG_NoHit") )

ftestL <- linearHypothesis(lm8, c("lateTG:last_anyTG_Hit = lateTG:last_anyTG_NoHit", 
                                 "Landfall:last_anyTG_Hit = Landfall:last_anyTG_NoHit") )
