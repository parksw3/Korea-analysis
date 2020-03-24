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
    day=yday(date_onset),
    day=day-min(day),
    absmax=yday(as.Date("2020-03-16"))-yday(date_onset)
  ) %>%
  filter(yday(date_confirm) <= yday(as.Date("2020-03-16")))

standata <- make_standata(delay~s(day),
              data=covid_delay,
              family=negbinomial)

standata$absmax <- covid_delay$absmax

smodel <- stan_model(file="report_delay_censor.stan")

report_delay_censor <- sampling(smodel, data=standata,
                                seed=101, chain=4,
                                iter=2000,
                                control=list(adapt_delta=0.99))

save("standata", "report_delay_censor", file="report_delay_censor.rda")
