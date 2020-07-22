library(dplyr)
load("../analysis_reconstruct/reconstruct_time_series_gyeonggi_raw.rda")
load("../analysis_delay/degree_censor.rda")

reconstruct_censor_raw <- R_t_gyeonggi_censor_raw <- vector('list', length(reconstruct_time_series_gyeonggi_raw))

set.seed(101)
for (i in 1:length(reconstruct_time_series_gyeonggi_raw)) {
  rr <- reconstruct_time_series_gyeonggi_raw[[i]]
  
  # pgamma(7, 62, 62/5) - pgamma(4, 62, 62/5) approx .95
  
  meangen <- rgamma(1, 62, 62/5)
  size <- rgamma(1, 20, 20/5)
  
  gen <- dnbinom(1:14, mu=meangen, size=size)
  gen <- gen/sum(gen)
  
  recondata <- rr %>%
    group_by(infection) %>%
    summarize(
      case=length(infection)
    ) %>%
    rename(
      date=infection
    ) %>%
    filter(date >= as.Date("2020-01-19"), date <= as.Date("2020-03-10"))
  
  dd <- data.frame(
    date=seq.Date(as.Date("2020-01-19"), as.Date("2020-03-10"), 1),
    case=0
  )
  
  dd$case[match(recondata$date, dd$date)] <- recondata$case
  dd$case <- dd$case/(1-degree_censor$median)
  
  reconstruct_censor_raw[[i]] <- dd
  
  IRt <- tail(dd$case, -14)/sapply(1:(length(dd$case)-14), function(x) sum(dd$case[x:(x+13)]*rev(gen)))
  
  Rt <- sapply(1:(length(IRt)-13), function(x) sum(IRt[x:(x+13)]*gen))
  
  R_t_gyeonggi_censor_raw[[i]] <- data.frame(
    date=seq.Date(as.Date("2020-01-19")+14, as.Date("2020-03-10"), 1),
    IRt=IRt,
    Rt=c(Rt, rep(NA, 13))
  )
}

save("R_t_gyeonggi_censor_raw", "reconstruct_censor_raw", file="R_t_gyeonggi_censor_raw.rda")
