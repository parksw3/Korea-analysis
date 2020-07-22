library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(brms)
library(rstan)

load("../analysis_test/test_weight.rda")
load("../analysis_delay/report_delay_backward.rda")

covid_delay_fake <- data.frame(
  day=0:56,
  delay=1
)

ex <- extract_draws(report_delay_backward, newdata=covid_delay_fake)

Xs <- ex$dpars$mu$sm$fe$Xs

Zs <- ex$dpars$mu$sm$re$sday$Zs[[1]]

ee <- rstan::extract(report_delay_backward$fit)

estmean <- sapply(1:length(ee$b_Intercept), function(i) {
  as.numeric(exp(ee$b_Intercept[i] + Xs * ee$bs[i] + Zs %*% ee$s_1_1[i,]))
})

geo <- read_xlsx("../data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
  mutate(
    date_report=as.Date(date_report)
  )

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

seoul <- geo %>% group_by(date_report) %>%
  summarize(
    cases=sum(Seoul, na.rm=TRUE)
  )

cases <- filter(seoul, date_report <= as.Date("2020-03-16"))$cases

reconstruct_time_series_seoul_raw <- vector('list', length(ee$b_Intercept))

set.seed(101)
for (i in 1:length(ee$b_Intercept)) {
  if (i %% 1000 == 0) print(i)
  ## 
  ## pgamma(7.9, 145, 145/6.5) - pgamma(5.6, 145, 145/6.5) approx 0.95
  meaninc <- rgamma(1, 145, 145/6.5)
  ## pgamma(4.2, 25, 25/2.6) - pgamma(1.8, 25, 25/2.6)
  sdinc <- rgamma(1, 25, 25/2.6)
  
  shapeinc <- meaninc^2/sdinc^2
  
  meandelay <- estmean[,i]
  shape <- ee$shape[i]
  
  reconstruct_time_series_seoul_raw[[i]] <- sapply(1:length(cases), function(x) {
    data_frame(
      confirm=as.Date("2020-01-20")+x-1,
      onset=confirm-rnbinom(cases[x], mu=meandelay[x], size=shape),
      infection=onset-floor(rgamma(cases[x], shape=shapeinc, rate=shapeinc/meaninc))
    )
  }, simplify = FALSE) %>%
    bind_rows
}

save("reconstruct_time_series_seoul_raw", file="reconstruct_time_series_seoul_raw.rda")
