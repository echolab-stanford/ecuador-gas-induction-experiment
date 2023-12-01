
# 0. Libraries ----

library(tidyverse)
library(cowplot)
library(fixest)
library(broom)
library(lubridate)
library(stringr)
library(ggthemes)
library(ggsci)
library(arsenal)

mycontrols  <- tableby.control(
  test = FALSE,
  total = FALSE,
  # numeric.test="kwt", cat.test="chisq",
  numeric.stats = c("N", "meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  digits = 2,
  stats.labels = list(
    N = 'Observations',
    meansd = "Mean (SD)",
    medianq1q3 = 'Median (IQR)'
  )
)

# 1. Data -------

ts_2day_data_summary <- read_rds("~/Downloads/ts_2day_data_summary.rds")
ts_data <- read_rds("~/Downloads/ts_data.rds")
no2_monitor <- read_rds("~/Downloads/no2_monitor_ecuador.rds")

# 2. Process time series data into hourly summaries -------

hourly_summary <- 
  ts_data2 %>% 
  dplyr::group_by(hhid, date_time, stove) %>%
  dplyr::summarize(
    no2_ppb = mean(no2_ppb, na.rm=T)
  ) %>%
  mutate(
    no2_ppb_5min = RcppRoll::roll_mean(as.numeric(no2_ppb), 5, align="center",fill=NA),
    no2_ppb_5min_max = RcppRoll::roll_max(as.numeric(no2_ppb), 5, align="center",fill=NA),
  ) %>% 
  mutate(
    no2_ppb_5min = ifelse(is.na(no2_ppb_5min), no2_ppb, no2_ppb_5min),
    no2_ppb_5min_max = ifelse(is.na(no2_ppb_5min_max), no2_ppb, no2_ppb_5min_max),
  ) %>% 
  # mutate(date_hour = floor_date(date_time, unit="hour")) %>% 
  # dplyr::group_by(date_hour, hhid, stove) %>% 
  # dplyr::summarize(
  #   no2_ppb = mean(no2_ppb, na.rm=T)
  # ) %>% 
  mutate(hour = hour(date_time)) %>% 
  group_by(hour, stove) %>% 
  dplyr::summarize(
    n=n(),
    no2_0_5 = mean(no2_ppb_5min<5, na.rm=T),
    no2_5_10 = mean(no2_ppb_5min>=5 & no2_ppb_5min<10, na.rm=T),
    no2_10_15 = mean(no2_ppb_5min>=10 & no2_ppb_5min<15, na.rm=T),
    no2_15_20 = mean(no2_ppb_5min>=15 & no2_ppb_5min<20, na.rm=T),
    no2_20_25 = mean(no2_ppb_5min>=20 & no2_ppb_5min<25, na.rm=T),
    no2_25 = mean(no2_ppb_5min>=25, na.rm=T),
    mean = mean(no2_ppb, na.rm=T),
    median = median(no2_ppb, na.rm=T))

hourly_summary_df <- 
  hourly_summary %>% 
  dplyr::select(hour, stove,
                no2_0_5, no2_5_10, no2_10_15,
                no2_15_20, no2_20_25, no2_25) %>% 
  pivot_longer(-c(hour, stove)) %>% 
  mutate(
    range = factor(name, levels=rev(c("no2_0_5", "no2_5_10", "no2_10_15",
                                      "no2_15_20", "no2_20_25", "no2_25"))),
    `NO2 ppb` = 
      plyr::mapvalues(
        range,
        from=c("no2_0_5", "no2_5_10", "no2_10_15",
               "no2_15_20", "no2_20_25", "no2_25"),
        to=c("[0-5)","[5-10)","[10-15)",
             "[15-20)","[20-25)",">25")
      )
  )


# 3. Regressions ------

# 3.1 48-hour averages ------

# Personal NO2 ------
reg_personal_48_0 <- 
  fixest::feols(
    concentration ~ 
      stove  | 
      hhid , 
    ts_2day_data_summary)

reg_personal_48_1 <- 
  fixest::feols(
    concentration ~ 
      stove  | 
      hhid + 
      month, 
    ts_2day_data_summary)


reg_personal_48_2 <- 
  fixest::feols(
    concentration ~ 
      stove  | 
      hhid +
      dow, 
    ts_2day_data_summary)

reg_personal_48_3 <- 
  fixest::feols(
    concentration ~ 
      stove  | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_personal_48_4 <- 
  fixest::feols(
    concentration ~ 
      stove + 
      no2_mean | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_personal_48_5 <- 
  fixest::feols(
    concentration ~ 
      stove +
      day_motion | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_personal_48_6 <- 
  fixest::feols(
    concentration ~ 
      stove +
      day_motion + 
      no2_mean| 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)


fixest::etable(
  reg_personal_48_0, 
  reg_personal_48_1, 
  reg_personal_48_2, 
  reg_personal_48_3, 
  reg_personal_48_4, 
  reg_personal_48_5, 
  reg_personal_48_6, 
  tex=T, 
  fixef_sizes = T)

# Kitchen NO2 ------
reg_kitchen_48_0 <- 
  fixest::feols(
    no2_ppb ~ 
      stove  | 
      hhid , 
    ts_2day_data_summary)

reg_kitchen_48_1 <- 
  fixest::feols(
    no2_ppb ~ 
      stove  | 
      hhid + 
      month, 
    ts_2day_data_summary)


reg_kitchen_48_2 <- 
  fixest::feols(
    no2_ppb ~ 
      stove  | 
      hhid +
      dow, 
    ts_2day_data_summary)

reg_kitchen_48_3 <- 
  fixest::feols(
    no2_ppb ~ 
      stove  | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_kitchen_48_4 <- 
  fixest::feols(
    no2_ppb ~ 
      stove + 
      no2_mean | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_kitchen_48_5 <- 
  fixest::feols(
    no2_ppb ~ 
      stove +
      day_motion | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)

reg_kitchen_48_6 <- 
  fixest::feols(
    no2_ppb ~ 
      stove +
      day_motion + 
      no2_mean| 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)


fixest::etable(
  reg_kitchen_48_0, 
  reg_kitchen_48_1, 
  reg_kitchen_48_2, 
  reg_kitchen_48_3, 
  reg_kitchen_48_4, 
  reg_kitchen_48_5, 
  reg_kitchen_48_6, 
  tex=T, 
  fixef_sizes = T)

# Personal PM2.5 -------

reg_personal_pm_48_0 <- 
  fixest::feols(
    pm48 ~ 
      stove  | 
      hhid , 
    ts_2day_data_summary %>% filter(pm48<100))
reg_personal_pm_48_1 <- 
  fixest::feols(
    pm48 ~ 
      stove  | 
      hhid + 
      month, 
    ts_2day_data_summary %>% filter(pm48<100))


reg_personal_pm_48_2 <- 
  fixest::feols(
    pm48 ~ 
      stove  | 
      hhid +
      dow, 
    ts_2day_data_summary %>% filter(pm48<100))

reg_personal_pm_48_3 <- 
  fixest::feols(
    pm48 ~ 
      stove  | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary %>% filter(pm48<100))

reg_personal_pm_48_40 <- 
  fixest::feols(
    pm48 ~ 
      stove + 
      pm25_mean | 
      hhid, 
    ts_2day_data_summary %>% filter(pm48<100))

reg_personal_pm_48_41 <- 
  fixest::feols(
    pm48 ~ 
      stove + 
      pm25_mean | 
      hhid + 
      month, 
    ts_2day_data_summary %>% filter(pm48<100))

reg_personal_pm_48_42 <- 
  fixest::feols(
    pm48 ~ 
      stove + 
      pm25_mean | 
      hhid + 
      # month +
      dow, 
    ts_2day_data_summary %>% filter(pm48<100))

reg_personal_pm_48_5 <- 
  fixest::feols(
    pm48 ~ 
      stove +
      day_motion | 
      hhid , 
    ts_2day_data_summary %>% filter(pm48<100 & !is.na(day_motion)))

reg_personal_pm_48_6 <- 
  fixest::feols(
    pm48 ~ 
      stove +
      day_motion + 
      pm25_mean| 
      hhid , 
    ts_2day_data_summary %>% filter(pm48<100))


fixest::etable(
  reg_personal_pm_48_0, 
  reg_personal_pm_48_1, 
  reg_personal_pm_48_2, 
  reg_personal_pm_48_3, 
  reg_personal_pm_48_40, 
  reg_personal_pm_48_41, 
  reg_personal_pm_48_42, 
  reg_personal_pm_48_5, 
  reg_personal_pm_48_6, 
  tex=T, 
  fixef_sizes = T)


# Fidelity restriction -------
reg_personal_48_fidelity <- 
  fixest::feols(
    concentration ~ 
      stove  | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary%>% 
      filter(hhid!="37-DUAL" &
               hhid!="36-DUAL" &
               hhid!="34-DUAL" &
               hhid!="32-DUAL" &
               hhid!="31-DUAL" &
               hhid!="10-DUAL"))


reg_kitchen_48_fidelity <- 
  fixest::feols(
    no2_ppb ~ 
      stove | 
      hhid + 
      # month +
      dow +
      `Monitor ID`, 
    ts_2day_data_summary%>% 
      filter(hhid!="37-DUAL" &
               hhid!="36-DUAL" &
               hhid!="34-DUAL" &
               hhid!="32-DUAL" &
               hhid!="31-DUAL" &
               hhid!="10-DUAL"))

reg_personal_pm_48_fidelity <- 
  fixest::feols(
    pm48 ~ 
      stove 
    # + day_motion 
    | 
      hhid +
      # month +
      dow ,
    ts_2day_data_summary %>% 
      # group_by(hhid, stove, date_min) %>%
      # summarize(pm48=mean(pm48, na.rm=T)) %>%
      ungroup() %>% 
      filter(pm48<100) %>% 
      filter(hhid!="37-DUAL" &
               hhid!="36-DUAL" &
               hhid!="34-DUAL" &
               hhid!="32-DUAL" &
               hhid!="31-DUAL" &
               hhid!="10-DUAL"))

etable(reg_personal_48_fidelity, reg_kitchen_48_fidelity, reg_personal_pm_48_fidelity, tex=T, fixef_sizes = T)


# IV instrument using % fidelity approach --------

ts_2day_data_summary <- 
  ts_2day_data_summary %>% 
  mutate(total_cooking = induction_cook + lpg_cook) %>% 
  mutate(induction_percent = induction_cook / total_cooking,
         lpg_percent = lpg_cook / total_cooking) %>% 
  mutate(fidelity_percent = ifelse(stove=="LPG", lpg_percent, induction_percent)) %>% 
  mutate(fidelity_percent = ifelse(stove=="LPG" & hhid=="10-DUAL", 1, fidelity_percent))

reg_personal_48_iv <- 
  fixest::feols(
    concentration ~ 
      stove  + 
      fidelity_percent | 
      hhid + 
      month + 
      dow, 
    ts_2day_data_summary)


reg_kitchen_48_iv <- 
  fixest::feols(
    no2_ppb ~ 
      stove + 
      fidelity_percent| 
      hhid + 
      month +
      dow +
      `Monitor ID`, 
    ts_2day_data_summary)

reg_personal_pm_48_iv <- 
  fixest::feols(
    pm48 ~ 
      stove 
    + fidelity_percent
    | 
      hhid +
      # month +
      dow ,
    ts_2day_data_summary %>% 
      # group_by(hhid, stove, date_min) %>%
      # summarize(pm48=mean(pm48, na.rm=T)) %>%
      ungroup() %>% 
      filter(pm48<100))

etable(reg_personal_48_iv, 
       reg_personal_48_fidelity,
       reg_kitchen_48_iv, 
       reg_kitchen_48_fidelity, 
       reg_personal_pm_48_iv,
       reg_personal_pm_48_fidelity,
       tex=T, fixef_sizes = T)

# 3.2: Regressions that assess impacts of cooking events on air pollution ------

# Create a 'clean' data frame for LPG cooking
ts_data_lpg_reg <- 
  ts_data %>% 
  ungroup() %>% 
  distinct() %>% 
  dplyr::select(hhid, stove, date_time, 
                motion, pats_temp, pm_5min, no2_ppb_5min, 
                cook_lpg, cook_induction, lpg_stove_temp_max, V) %>% 
  distinct() %>% 
  filter(!is.na(hhid) & !is.na(stove) & !is.na(date_time)) %>% 
  mutate(cook_lpg = ifelse(as.character(cook_lpg)=="-Inf", NA, as.character(cook_lpg)),
         cook_induction = ifelse(as.character(cook_induction)=="-Inf", NA, as.character(cook_induction)))  %>%
  mutate(pm_5min = ifelse(pm_5min>500, 500, pm_5min)) %>% 
  distinct(hhid, stove, date_time, no2_ppb_5min, pm_5min, cook_lpg) %>% 
  filter(!is.na(hhid) & !is.na(date_time) & !is.na(pm_5min) & 
           !is.na(cook_lpg)) %>% 
  left_join(no2_monitor, by=c("hhid", "stove")) %>% 
  mutate(month = month(date_time),
         day = wday(date_time),
         hour = hour(date_time)) 


# Create a 'clean' data frame for induction cooking
ts_data_induction_reg <- 
  ts_data %>% 
  ungroup() %>% 
  distinct() %>% 
  dplyr::select(hhid, stove, date_time, 
                motion, pats_temp, pm_5min, no2_ppb_5min, 
                cook_lpg, cook_induction, lpg_stove_temp_max, V) %>% 
  distinct() %>% 
  filter(!is.na(hhid) & !is.na(stove) & !is.na(date_time)) %>% 
  mutate(cook_lpg = ifelse(as.character(cook_lpg)=="-Inf", NA, as.character(cook_lpg)),
         cook_induction = ifelse(as.character(cook_induction)=="-Inf", NA, as.character(cook_induction)))  %>%
  mutate(pm_5min = ifelse(pm_5min>500, 500, pm_5min)) %>%
  left_join(no2_monitor, by=c("hhid", "stove")) %>% 
  distinct(hhid, date_time, 
           cook_induction,
           pm_5min,
           no2_ppb_5min, `Monitor ID`) %>% 
  filter(!is.na(hhid) & 
           !is.na(date_time) & 
           !is.na(cook_induction)) %>%
  mutate(month = month(date_time),
         day = wday(date_time),
         hour = hour(date_time)) 


reg_pm_5min_1b <- 
  feols(pm_5min ~ 
          cook_lpg
        | 
          hhid + hour + day + month, 
        ts_data_lpg_reg)

reg_pm_5min_2b <- 
  feols(pm_5min ~ 
          cook_induction
        | 
          hhid + hour + day + month, 
        ts_data_induction_reg)

reg_no2_5min_1b <- 
  feols(no2_ppb_5min ~ 
          cook_lpg | 
          hhid + hour + day + month + `Monitor ID`, 
        ts_data_lpg_reg)

reg_no2_5min_2b <- 
  feols(no2_ppb_5min ~ 
          cook_induction | 
          hhid + hour + day + month + 
          `Monitor ID`, 
        ts_data_induction_reg)

reg_no2_5min_3b <- feols(
  no2_ppb_5min ~ 
    cook_induction + 
    cook_lpg |
    hhid + hour + day + month + `Monitor ID`, 
  ts_data %>% 
    mutate(cook_lpg = ifelse(as.character(cook_lpg)=="-Inf", NA, as.character(cook_lpg)),
           cook_induction = ifelse(as.character(cook_induction)=="-Inf", NA, as.character(cook_induction)))  %>%
    mutate(day = wday(date_time),
           hour = hour(date_time),
           month = month(date_time)) %>% 
    left_join(no2_monitor))

reg_pm_5min_3b <- feols(
  pm_5min ~ 
    cook_induction + cook_lpg
  |
    hhid + hour + day + month,
  ts_data %>% 
    mutate(cook_lpg = ifelse(as.character(cook_lpg)=="-Inf", NA, as.character(cook_lpg)),
           cook_induction = ifelse(as.character(cook_induction)=="-Inf", NA, as.character(cook_induction)))  %>%
    mutate(
      pm_5min = ifelse(pm_5min>500, 500, pm_5min),
      day = wday(date_time),
      hour = hour(date_time),
      month = month(date_time)))

reg_pm_5min_3b


etable(reg_no2_5min_1b, reg_no2_5min_2b, 
       reg_pm_5min_1b, reg_pm_5min_2b, 
       tex=T, fixef_sizes = T)  

etable(reg_no2_5min_3b, reg_pm_5min_3b, 
       tex=T, fixef_sizes = T)   



# 4. Table 1: Summary stats -------


table_summary <- 
  summary(tableby(~ 
                    no2_mean + 
                    pm25_mean + 
                    concentration + 
          no2_ppb  +
          pm48 + 
            pats_temp + 
            motion + 
            lpg_cook_min + 
          induction_cook_min,
        data=ts_2day_data_summary %>% 
          mutate(pm48 = ifelse(pm48>100, NA, pm48)) %>% 
          mutate(motion = ifelse(motion==0, NA, motion)) %>% # these are missing data
          mutate(lpg_cook_min = lpg_cook,
                 induction_cook_min = induction_cook) %>%
          mutate(lpg_cook_min = ifelse(stove=="Induction", NA, lpg_cook_min)) %>% 
          mutate(induction_cook_min = ifelse(stove=="LPG", NA, induction_cook_min)), 
        control = mycontrols))

table_summary

stove_table_summary <- 
  summary(tableby(stove~ 
                    no2_mean + 
                    pm25_mean +
                    concentration + 
                    
                    no2_ppb  +
                    pm48 + 
                    pats_temp + 
                    motion + 
                    lpg_cook_min + 
                    
                    induction_cook_min,
                  data=ts_2day_data_summary %>% 
                    mutate(pm48 = ifelse(pm48>100, NA, pm48)) %>% 
                    mutate(motion = ifelse(motion==0, NA, motion)) %>% # these are missing data
                    mutate(lpg_cook_min = lpg_cook,
                           induction_cook_min = induction_cook),
                    control = mycontrols))

stove_table_summary


# 5. Make Figure 1 -------

# Panel A. Personal NO2 exposure -----

ogawa_fig <- 
  ggplot(ts_2day_data_summary, aes(stove, concentration, color=stove, fill=stove)) + 
  # ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, 
  #                      justification = -.3, point_colour = NA,
  #                      color="black", alpha=0.7) +
  geom_hline(yintercept=13.29, linewidth=0.25, linetype="twodash") + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black", alpha=0.8) +
  scale_x_discrete(limits=c("Induction", "LPG")) + 
  annotate("text", x=2/3, y=15, label="WHO 24-hr Guideline", size=3) + 
  scale_color_aaas() + 
  scale_fill_aaas() + 
  gghalves::geom_half_point(side = "r", range_scale = 0, 
                            shape = 95, size = 10, alpha = .8) +
  theme_clean() + 
  coord_cartesian(ylim=c(0, 47)) + 
  scale_y_continuous(labels=scales::unit_format(suffix=" ppb"),
                     breaks=c(seq(0,40,10)),
                     expand=c(0,0)) + 
  ggtitle(expression(bold(paste(
    "Personal ", NO[2],
    " exposures", sep="")))) + 
  annotate("text", x=1.4, y=43, label="50% higher (P=0.001)", 
    col="black", 
    size=4, family="Helvetica") +
  geom_segment(aes(y=41, yend=41,x=1, xend=2), size=0.07, color="black") + 
  ylab("") + xlab("") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        plot.title = element_text(size=13, color="black"),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color="black", linewidth = 0.7),
        axis.line.y = element_blank(),
        plot.background = element_blank())

ogawa_fig

# Panel B: Kitchen NO2 -------

kitchen_fig <- 
  ggplot(ts_2day_data_summary, aes(stove, no2_ppb, color=stove, fill=stove)) + 
  geom_hline(yintercept=13.29, linewidth=0.25, linetype="twodash") + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black", alpha=0.8) +
  scale_x_discrete(limits=c("Induction", "LPG")) + 
  
  scale_color_aaas() + 
  scale_fill_aaas() + 
  gghalves::geom_half_point(side = "l", range_scale = 0, 
                            shape = 95, size = 10, alpha = .8) +
  theme_clean() + 
  coord_cartesian(ylim=c(0, 47)) + 
  scale_y_continuous(labels=scales::unit_format(suffix=" ppb"),
                     breaks=c(seq(0,40,10)),
                     expand=c(0,0)) + 
  annotate("text", x=1.4, y=26, label="15% higher (P=0.09)", col="black", 
    size=4, family="Helvetica") +
  geom_segment(aes(y=24, yend=24,x=1,xend=2), size=0.07, color="black") + 
  ggtitle(expression(bold(paste(
    "Kitchen area ", NO[2],
    " concentrations", sep="")))) + 
  ylab("") + xlab("") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        strip.text = element_text(size=12, color="black"),
        plot.title = element_text(size=13, color="black"),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color="black", linewidth = 0.7),
        axis.line.y = element_blank(),
        plot.background = element_blank())

# Panel C: Personal PM2.5 -------

pm_fig <- 
  ggplot(ts_2day_data_summary, aes(stove, pm48, color=stove, fill=stove)) + 
  geom_hline(yintercept=15, linewidth=0.25, linetype="twodash") + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black", alpha=0.8) +
  scale_x_discrete(limits=c("Induction", "LPG")) + 
  scale_color_aaas() + 
  scale_fill_aaas() + 
  gghalves::geom_half_point(side = "l", range_scale = 0, 
                            shape = 95, size = 10, alpha = .8) +
  theme_clean() + 
  coord_cartesian(ylim=c(0, 110)) + 
  annotate("text", x=1.4, y=105, label="44% higher (P=0.06)", size=4, family="Helvetica") +
  geom_segment(aes(y=101, yend=101,x=0.75,xend=2), size=0.07, color="black") + 
  scale_y_continuous(
    breaks=c(0, 25, 50, 75, 100),
    expand=c(0,0),
    labels=scales::unit_format(suffix=" ug/m3")) +
  ggtitle(expression(bold(paste("Personal ", PM[2.5]," exposure", sep="")))) + 
  # ylab(expression(paste(
  #   "(",
  #   mu, g, "/", m^3,
  #   ")", sep=""))) +
  xlab("") + 
  theme(legend.position = "none",
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        plot.title = element_text(size=13, color="black",face = "bold"),
        axis.title.y = element_blank(),
        axis.line.x.bottom = element_line(color="black", linewidth=0.7),
        axis.ticks = element_blank(),
        # axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.background = element_blank())

# Panel D: Hourly kitchen NO2 -------

hourly_summary_df_1 <- 
  hourly_summary_df %>% 
  filter(!is.na(hour) & !is.na(stove))

hour_lpg_ind_fig <- 
  ggplot(
    hourly_summary_df %>% filter(!is.na(hour) & !is.na(stove)) %>% 
      mutate(stove = factor(stove, levels=c("Induction", "LPG"))), 
    aes(x=hour, y=value, color=`NO2 ppb`, fill=`NO2 ppb`)
  ) + 
  geom_area(
  ) + 
  scale_fill_manual(values=MetBrewer::met.brewer("Cassatt2")) +
  scale_color_manual(values=MetBrewer::met.brewer("Cassatt2")) +
  theme_clean() + 
  scale_y_continuous(labels=scales::percent_format()) + 
  scale_x_continuous(breaks=c(seq(0, 24, 4))) +
  # ggtitle("Hourly kitchen area NO2 concentrations") + 
  ggtitle(expression(bold(paste(
    "Hourly kitchen area ", NO[2],
    " concentrations", sep="")))) + 
  ylab("") + xlab("Hour") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=12, color="black"),
        strip.text=element_text(size=12, color="black"),
        legend.background = element_blank(),
        axis.title.x = element_text(size=12, color="black"),
        # legend.box = element_blank(),
        legend.box.background = element_blank(),
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        plot.title = element_text(size=13, color="black", face="bold"),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.background = element_blank()) + 
  facet_grid(.~stove)



# Annotations and combining -------

ogawa_fig_annotate <- plot_grid(
  ogawa_fig,
  NULL,
  rel_heights = c(1, 0.1),
  nrow=2
) + 
  annotate(
    "text", x=0.55, y=0.07, label="Effect of LPG vs. induction: 9.9 ppb", size=5
  ) + 
  annotate(
    "text", x=0.55, y=0, label="95% confidence interval: [4.5, 15.2]", size=5
  )

kitchen_no2_fig_annotate <- plot_grid(
  kitchen_fig,
  NULL,
  rel_heights = c(1, 0.1),
  nrow=2
) + 
  annotate(
    "text", x=0.55, y=0.07, label="Effect of LPG vs. induction: 1.1 ppb", size=5
  ) + 
  annotate(
    "text", x=0.55, y=0, label="95% confidence interval: [0.4, 2.1]", size=5
  )

pm_fig_annotate <- plot_grid(
  pm_fig,
  NULL,
  rel_heights = c(1, 0.1),
  nrow=2
) + 
  annotate(
    "text", x=0.55, y=0.07, label="Effect of LPG vs. induction: 11.4 ug/m3", size=5
  ) + 
  annotate(
    "text", x=0.55, y=0, label="95% confidence interval: [-0.1, 22.8]", size=5
  )

combined_pollution_figs <- 
  plot_grid(
  ogawa_fig_annotate, kitchen_no2_fig_annotate, pm_fig_annotate,
  nrow=1,align="hv",
  labels = c("A", "B", "C"),
  label_size=14
)


combined_pollution_figs1 <- 
  plot_grid(
    combined_pollution_figs,
    NULL,
    hour_lpg_ind_fig,
    align="hv",
    nrow = 3,
    labels = c("","", "D"),
    label_size = 14,
    rel_heights = c(1, 0.1, 1)
  )


# cowplot::ggsave2(
#   "~/Desktop/combined_pollution_figs1.pdf",
#   plot = combined_pollution_figs1,
#   dpi = 300,
#   height = 225,
#   width = 300,
#   unit = "mm"
# )


