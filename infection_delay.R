library(rstan)
load("report_delay_censor.rda")
load("pred_report_delay.rda")

# pgamma(7.7, 175, 175/6.4) - pgamma(5.6, 175, 175/6.4)
# pgamma(3.7, 35, 35/2.3) - pgamma(1.7, 35, 35/2.3)

ee <- extract(report_delay_censor)

for (i in 1:8000) {
  mu_inc <- rgamma(1, 175, 175/6.4)
  sd_inc <- rgamma(1, 35, 35/2.3)
  
  
}

plot(estmean[,1])

ee$shape



dnbinom()
