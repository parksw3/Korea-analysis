library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(brms)
library(rstan)

load("../analysis_delay/report_delay_backward.rda")

covid_seoul <- read_xlsx("../data/COVID19-Seoul.xlsx", sheet=1, na="NA")

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

reconstruct_time_series_seoul_linelist <- vector('list', length(ee$b_Intercept))

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
  
  tmp <- data_frame(
    confirm=as.Date(covid_seoul$date_confirm),
    onset=as.Date(covid_seoul$date_onset)
  )
  
  tmp$onset[is.na(tmp$onset)] <- tmp$confirm[is.na(tmp$onset)]-rnbinom(
    sum(is.na(tmp$onset)), 
    mu=meandelay[as.numeric(as.Date(covid_seoul$date_confirm[is.na(tmp$onset)])-as.Date("2020-01-20"))], 
    size=shape)
  
  tmp$infection <- tmp$onset-floor(rgamma(nrow(tmp), shape=shapeinc, rate=shapeinc/meaninc))
  
  tmp$import <- !is.na(covid_seoul$import_source)
  
  reconstruct_time_series_seoul_linelist[[i]] <- tmp
}

save("reconstruct_time_series_seoul_linelist", file="reconstruct_time_series_seoul_linelist.rda")
