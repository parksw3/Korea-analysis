library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(brms)
library(rstan)

load("test_weight.rda")
load("report_delay_backward.rda")

covid_line <- read_xlsx("data/COVID19-Korea-2020-03-13.xlsx", na="NA") %>%
  mutate(
    age=as.numeric(age),
    date_onset=as.Date(date_onset)
  )

casenum <- covid_line$case[!is.na(covid_line$case)]

covid_seoul <- read_xlsx("data/COVID19-Korea-Regional-2020-03-13.xlsx", sheet=1, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_chungnam <- read_xlsx("data/COVID19-Korea-Regional-2020-03-13.xlsx", sheet=2, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_busan <- read_xlsx("data/COVID19-Korea-Regional-2020-03-13.xlsx", sheet=3, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  filter(
    !(case %in% casenum)
  )

covid_gyeongnam <- read_xlsx("data/COVID19-Korea-Regional-2020-03-13.xlsx", sheet=4, na="NA") %>%
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
  filter(yday(date_confirm) <= yday(as.Date("2020-03-11")))

covid_delay_fake <- data.frame(
  day=covid_delay$day[covid_delay$date_confirm==as.Date("2020-01-20")]:56,
  delay=1
)

ex <- extract_draws(report_delay_backward, newdata=covid_delay_fake)

Xs <- ex$dpars$mu$sm$fe$Xs

Zs <- ex$dpars$mu$sm$re$sday$Zs[[1]]

ee <- rstan::extract(report_delay_backward$fit)

estmean <- sapply(1:length(ee$b_Intercept), function(i) {
  as.numeric(exp(ee$b_Intercept[i] + Xs * ee$bs[i] + Zs %*% ee$s_1_1[i,]))
})

geo <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
  mutate(
    date_report=as.Date(date_report)
  ) 

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

gyeongbuk <- geo %>% group_by(date_report) %>%
  summarize(
    cases=sum(`Gyeongsangbuk-do`, na.rm=TRUE)
  ) %>%
  merge(test_weight) %>%
  mutate(
    cases2=cases*weight,
    cases3=round(cases2*weight2)
  )

cases <- filter(gyeongbuk, date_report <= as.Date("2020-03-16"))$cases3

reconstruct_time_series_gyeongbuk <- vector('list', length(ee$b_Intercept))

set.seed(101)
for (i in 1:length(ee$b_Intercept)) {
  if (i %% 1000 == 0) print(i)
  ## 
  ## pgamma(7.9, 145, 145/6.5) - pgamma(5.6, 145, 145/6.5) approx 0.95
  meaninc <- rgamma(1, 145, 145/6.5)
  ## pgamma(4.2, 25, 25/2.6) - pgamma(1.8, 25, 25/2.6)
  sdinc <- rgamma(1, 25, 25/2.6)
  
  shape <- meaninc^2/sdinc^2
  
  meandelay <- estmean[,i]
  shape <- ee$shape[i]
  
  reconstruct_time_series_gyeongbuk[[i]] <- sapply(1:length(cases), function(x) {
    data_frame(
      confirm=as.Date("2020-01-20")+x-1,
      onset=confirm-rnbinom(round(cases[x]), mu=meandelay[x], size=shape),
      infection=onset-floor(rgamma(round(cases[x]), shape=shape, rate=shape/meaninc))
    )
  }, simplify = FALSE) %>%
    bind_rows
}

save("reconstruct_time_series_gyeongbuk", file="reconstruct_time_series_gyeongbuk.rda")
