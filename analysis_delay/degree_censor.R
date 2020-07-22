library(dplyr)
library(rstan)

load("../analysis_delay/report_delay_censor.rda")
load("../figure_supp/pred_report_delay.rda")

ee <- rstan::extract(report_delay_censor)

shape <- ee$shape

nsim <- 1000

degree_censor <- vector('list', length(shape))

set.seed(101)
for (i in 1:length(shape)) {
  if (i %% 100 == 0) print(i)
  
  meaninc <- rgamma(1, 145, 145/6.5)
  ## pgamma(4.2, 25, 25/2.6) - pgamma(1.8, 25, 25/2.6)
  sdinc <- rgamma(1, 25, 25/2.6)
  
  shapeinc <- meaninc^2/sdinc^2
  
  meandelay <- estmean[,i]
  shapedelay <- shape[i]
  
  dd <- data.frame(
    date=seq.Date(from=as.Date("2020-01-10"), to=as.Date("2020-03-16"), by=1),
    delay=meandelay
  ) %>%
    filter(date >= as.Date("2020-01-19"))
  
  censor <- sapply(1:nrow(dd), function(x) {
    inc <- floor(rgamma(nsim, shape=shapeinc, rate=shapeinc/meaninc))
    
    symp <- as.Date("2020-01-19")-1+inc+x
    
    mu <- dd$delay[match(symp, dd$date)]
    mu[is.na(mu)] <- tail(meandelay, 1)
    
    report <- symp + rnbinom(length(symp), mu=mu, size=shapedelay)
    
    mean(report >= as.Date("2020-03-16"))
  })
  
  degree_censor[[i]] <- data.frame(
    date=seq.Date(from=as.Date("2020-01-19"), to=as.Date("2020-03-16"), by=1),
    censor=censor
  )
}

degree_censor <- degree_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(censor),
    lwr=quantile(censor, 0.025),
    upr=quantile(censor, 0.975)
  ) %>%
  filter(date <= as.Date("2020-03-10"))

save("degree_censor", file="degree_censor.rda")
