# make some maps of different landfall categories.


library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools, sf, tmap,
       data.table, dtplyr, lubridate, plm, estimatr, fixest, 'tigris')


df <- fread("Data/scanner_with_past.csv")

df <- df %>% mutate(
  acc_streak = ifelse(hit_streak > 0, hit_streak, -1*miss_streak),
  fips = str_pad(fips, 5,'left', '0')
)


# accurate forecasts
acc_df <- df %>% group_by(fips) %>% summarise(
  TG_hit = sum(anyTG_Hit),
  mTG_hit = sum(midTG_Hit),
  
  TG_miss = sum(anyTG_NoHit),
  mTG_miss = sum(midTG_NoHit),
  
  noTG_hit = sum(Hit_noTG),
  noMTG_hit = sum(Hit_nomidTG),
  
  Any_Landfall = max(Landfall)
)


acc_df <- subset(acc_df, Any_Landfall == 1)

# 491 counties

us_counties <- tigris::counties(state = c('01', '12', '13',
                                          '22', '28','37', '45',
                                          '48'))

us_counties <- us_counties %>% mutate(
  fips = paste(STATEFP, COUNTYFP, sep= ""),
  fips = str_pad(fips, 5,'left', '0')
)

image_df <- us_counties %>% filter(fips %in% acc_df$fips)

image_df <- left_join(image_df, acc_df , by = "fips")


tm_shape(us_counties)+
  tm_polygons(fill = "white")+tm_layout(frame=FALSE)+
tm_shape(image_df)+
  tm_polygons(fill = "pink")+
  tm_title("Counties Reporting Sales", size = 4)


tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_shape(image_df)+
  tm_polygons(fill = "TG_hit",
              fill.scale = tm_scale_intervals(
                values = "mako",
                breaks = c(0, 1, 4, 7, 10),
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_title("Count of Accurate Forecasts", size = 4)


tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_shape(image_df)+
  tm_polygons(fill = "TG_miss",
              fill.scale = tm_scale_intervals(
                values = "mako",
                breaks = c(0, 12, 24, 36, 46),
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_title("Count of Forecasts without Landfall", size = 4)


tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "noTG_hit",
              fill.scale = tm_scale_intervals(
                values = "mako",
                breaks = c(0, 1, 2),
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Count of Landfalls without a Forecast", size = 4)






# weaker definitions
  
tm_shape(us_counties)+
  tm_polygons(fill = "white")+tm_layout(frame=FALSE)+
  tm_shape(image_df)+
  tm_polygons(fill = "mTG_hit", palette = "brewer.yl_gn", breaks = c(0, 1, 3, 6, 8),title = "Count")+
  tm_title("Count of Accurate Forecasts", size = 4)


tm_shape(us_counties)+
  tm_polygons(fill = "white")+tm_layout(frame=FALSE)+
  tm_shape(image_df)+
  tm_polygons(fill = "mTG_miss", palette = "brewer.yl_gn",  breaks = c(0, 1, 8, 16, 25), title = "Count")+
  tm_title("Count of Forecasts without Landfall", size = 4)

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "noMTG_hit",
              fill.scale = tm_scale_intervals(
                values = "mako",
                
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
 
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Landfalls with Late Forecasts", size = 4)




# plots of max streak, mean streaks, end of data, after year 1, and mid way. ------------

streak_df <- df %>% group_by(fips)%>% summarize(
  max_miss_streak = max(miss_streak, na.rm = T),
  max_hit_streak = max(hit_streak, na.rm=T)
)


image_df <- us_counties %>% filter(fips %in% acc_df$fips)

image_df <- left_join(image_df, streak_df, by = 'fips')


# max miss plot
tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "max_miss_streak",
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Most Forecasts in a Row with no Landfall", size = 4)


# max hit plot

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "max_hit_streak",
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Most Forecasts in a Row with Landfall", size = 4)




# end of 2008
date_df <- subset(df, week_end == "2008-12-27")
date_df <- date_df[,c(1,51)]

image_df <- us_counties %>% filter(fips %in% acc_df$fips)

image_df <- left_join(image_df, date_df, by = 'fips')

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "acc_streak",
              fill.scale = tm_scale_intervals(
                values = "brewer.rd_bu",
                midpoint=0,
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Accuracy Streak: End of 2008", size = 4)


# end of 2013
date_df <- subset(df, week_end == "2013-12-28")
date_df <- date_df[,c(1,51)]

image_df <- us_counties %>% filter(fips %in% acc_df$fips)

image_df <- left_join(image_df, date_df, by = 'fips')

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "acc_streak",
              fill.scale = tm_scale_intervals(
                values = "brewer.rd_bu",
                midpoint=0,
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_layout(frame=FALSE)+
  tm_title("Accuracy Streak: End of 2013", size = 4)


# end of 2019
date_df <- subset(df, week_end == "2019-12-28")
date_df <- date_df[,c(1,51)]

image_df <- us_counties %>% filter(fips %in% acc_df$fips)

image_df <- left_join(image_df, date_df, by = 'fips')

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "acc_streak",
              fill.scale = tm_scale_intervals(
                values = "brewer.rd_bu",
                midpoint=0,
                label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Accuracy Streak: End of 2019", size = 4)



# recency status maps ---------------------------------------

# 2013 official start of season

last_df <- subset(df, week_end == "2013-06-22")

last_df <- subset(last_df, fips %in% acc_df$fips)

last_df <- last_df %>% mutate(
  last_result = ifelse(last_anyTG_NoHit, "Miss", "Hit")
)

image_df <- us_counties %>% filter(fips %in% last_df$fips)

image_df <- left_join(image_df, last_df , by = "fips")

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "last_result",
              fill.scale = tm_scale_categorical(
                values = "mako",
                #label.style = "continuous"
              ),
              fill.legend = tm_legend(
                title = "Last Forecast Result",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Start of 2013 Hurricane Season", size = 4)


# 2013 Middle of season

last_df <- subset(df, week_end == "2013-08-31")

last_df <- subset(last_df, fips %in% acc_df$fips)

last_df <- last_df %>% mutate(
  last_result = ifelse(last_anyTG_NoHit, "Miss", "Hit")
)

image_df <- us_counties %>% filter(fips %in% last_df$fips)

image_df <- left_join(image_df, last_df , by = "fips")

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "last_result", title = "Recent Forecast Result")+
  tm_legend(position = c("right", "center"))+
  tm_layout(frame=FALSE)+
  tm_title("Middle of 2013 Hurricane Season", size = 4)


# end of 2013
last_df <- subset(df, week_end == "2013-11-30")

last_df <- subset(last_df, fips %in% acc_df$fips)

last_df <- last_df %>% mutate(
  last_result = ifelse(last_anyTG_NoHit, "Miss", "Hit")
)

image_df <- us_counties %>% filter(fips %in% last_df$fips)

image_df <- left_join(image_df, last_df , by = "fips")

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "last_result", title = "Forecast Result")+
  tm_legend(position = c("right", "center"))+
  tm_layout(frame=FALSE)+
  tm_title("End of 2013 Hurricane Season", size = 4)

