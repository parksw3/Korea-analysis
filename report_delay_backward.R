library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(brms)
library(rstan)

covid_line <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA") %>%
  mutate(
    age=as.numeric(age),
    date_onset=as.Date(date_onset)
  )

casenum <- covid_line$case[!is.na(covid_line$case)]

covid_seoul <- read_xlsx("data/COVID19-Korea-Regional-2020-03-16.xlsx", sheet=1, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_chungnam <- read_xlsx("data/COVID19-Korea-Regional-2020-03-16.xlsx", sheet=2, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_busan <- read_xlsx("data/COVID19-Korea-Regional-2020-03-16.xlsx", sheet=3, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_gyeongnam <- read_xlsx("data/COVID19-Korea-Regional-2020-03-16.xlsx", sheet=4, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_all <- rbind(covid_line, covid_seoul, covid_chungnam, covid_busan, covid_gyeongnam)

covid_delay <- covid_all %>%
  filter(!is.na(date_onset), !is.na(date_confirm)) %>%
  mutate(
    delay=yday(date_confirm)-yday(date_onset),
    day=yday(date_confirm),
    day=day-min(day)
  ) %>%
  filter(yday(date_confirm) <= yday(as.Date("2020-03-16")))

report_delay_backward <- brm(delay~s(day),
                             data=covid_delay,
                             family=negbinomial,
                             prior=prior(normal(0, 2), class=b) +
                               prior(normal(0, 2), class=Intercept),
                             seed=102, 
                             thin=5,
                             control=list(adapt_delta=0.9))

save("report_delay_backward", file="report_delay_backward.rda")
