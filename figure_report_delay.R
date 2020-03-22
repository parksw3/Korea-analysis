library(brms)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(mgcv)
library(rstan)
library(gridExtra)
library(ggplot2); theme_set(theme_bw())
load("report_delay_censor.rda")
load("report_delay_backward.rda")

covid_test <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=2)

covid_test$negative[7] <- (covid_test$negative[6]+covid_test$negative[8])/2
covid_test$negative[13] <- (covid_test$negative[12]+covid_test$negative[14])/2

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
  filter(!is.na(date_onset)) %>%
  mutate(
    delay=yday(date_confirm)-yday(date_onset),
    day=yday(date_onset),
    day=day-min(day),
    absmax=yday(as.Date("2020-03-11"))-yday(date_onset)
  ) %>%
  filter(yday(date_confirm) <= yday(as.Date("2020-03-11")))

covid_delay2 <- covid_delay %>%
  group_by(date_onset) %>%
  summarize(
    mean=mean(delay),
    max=max(delay),
    min=min(delay),
    size=length(delay),
    absmax=yday(as.Date("2020-03-11"))-yday(unique(date_onset))
  ) 

## need to hack this to predict ugh
mm <- brm(delay~s(day),
          data=covid_delay,
          family=negbinomial,
          chains=1, iter=2)

covid_delay_fake <- data.frame(
  day=0:60,
  delay=1
)

ex <- extract_draws(mm, newdata=covid_delay_fake)

Xs <- ex$dpars$mu$sm$fe$Xs

Zs <- ex$dpars$mu$sm$re$sday$Zs[[1]]

ee <- rstan::extract(report_delay_censor)

# exp(Intercept + rep_vector(0, N) + Xs * bs + Zs_1_1 * s_1_1);

estmean <- sapply(1:length(ee$Intercept), function(i) {
  as.numeric(exp(ee$Intercept[i] + Xs * ee$bs[i] + Zs %*% ee$s_1_1[i,]))
})

estdata <- data.frame(
  date=seq.Date(from=as.Date("2020-01-10"), to=as.Date("2020-03-10"), by=1),
  median=apply(estmean, 1, median),
  lwr=apply(estmean, 1, quantile, 0.025),
  upr=apply(estmean, 1, quantile, 0.975)
)

covid_delay3 <- covid_all %>%
  filter(!is.na(date_onset), !is.na(date_confirm)) %>%
  mutate(
    date_confirm=as.Date(date_confirm),
    delay=yday(date_confirm)-yday(date_onset),
    day=yday(date_confirm),
    day=day-min(day)
  ) %>%
  filter(yday(date_confirm) <= yday(as.Date("2020-03-11")))

covid_delay_fake2 <- data.frame(
  day=covid_delay3$day[covid_delay3$date_confirm==as.Date("2020-01-20")]:56,
  delay=1
)

ex2 <- extract_draws(report_delay_backward, newdata=covid_delay_fake2)

Xs2 <- ex2$dpars$mu$sm$fe$Xs

Zs2 <- ex2$dpars$mu$sm$re$sday$Zs[[1]]

ee2 <- rstan::extract(report_delay_backward$fit)

estmean2 <- sapply(1:length(ee2$b_Intercept), function(i) {
  as.numeric(exp(ee2$b_Intercept[i] + Xs2 * ee2$bs[i] + Zs2 %*% ee2$s_1_1[i,]))
})

estdata2 <- data.frame(
  date=seq.Date(from=as.Date("2020-01-20"), to=as.Date("2020-03-16"), by=1),
  median=apply(estmean2, 1, median),
  lwr=apply(estmean2, 1, quantile, 0.025),
  upr=apply(estmean2, 1, quantile, 0.975)
)

covid_test2 <- covid_test %>%
  mutate(
    time_report=hour(time_report)
  ) %>%
  filter(is.na(time_report) | time_report != 16) %>%
  mutate(
    total=positive+negative
  ) %>%
  mutate(
    diff=diff(c(0, total)),
    diff2=diff(c(0, positive)),
    prop=diff2/diff
  )

g1 <- ggplot(covid_test2) +
  geom_bar(aes(as.Date(date_report), diff), stat="identity", alpha=0.5) +
  geom_vline(xintercept=as.Date("2020-01-28"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-07"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-20"), lty=2) +
  geom_vline(xintercept=as.Date("2020-03-02"), lty=2) +
  scale_y_continuous("Number of tests completed", limit=c(0, 21000), expand=c(0, 0)) +
  scale_x_date("Date", expand=c(0, 0), limits=c(as.Date("2020-01-20"), as.Date("2020-03-16"))+c(0, 0.5)) +
  coord_cartesian(clip = 'off') +
  ggtitle("A. Number of tests completed") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

g2 <- ggplot(covid_test2) +
  geom_bar(aes(as.Date(date_report), prop), stat="identity", alpha=0.5) +
  geom_vline(xintercept=as.Date("2020-01-28"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-07"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-20"), lty=2) +
  geom_vline(xintercept=as.Date("2020-03-02"), lty=2) +
  scale_y_continuous("Proportion of positive cases", limit=c(0, 0.3), expand=c(0, 0)) +
  scale_x_date("Date", expand=c(0, 0), limits=c(as.Date("2020-01-20"), as.Date("2020-03-16"))+c(0, 0.5)) +
  coord_cartesian(clip = 'off') +
  ggtitle("B. Proportion of positive cases") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

g3 <- ggplot(covid_delay3) +
  geom_boxplot(aes(date_confirm, delay, group=date_confirm)) +
  geom_ribbon(data=estdata2, aes(date, ymin=lwr, ymax=upr), alpha=0.4) +
  geom_line(data=estdata2, aes(date, median)) +
  geom_vline(xintercept=as.Date("2020-01-28"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-07"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-20"), lty=2) +
  geom_vline(xintercept=as.Date("2020-03-02"), lty=2) +
  #geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  # annotate("text", x=as.Date("2020-01-20"), y=23.5, label="Imported cases introduced", angle=90) +
  # geom_vline(xintercept=as.Date("2020-01-19"), col="red", lwd=2) +
  scale_size_area("Number of samples", guide=FALSE, max_size=2) +
  scale_y_continuous("Confirmation to symptom onset (days)", limit=c(0, 21), expand=c(0, 0)) +
  scale_x_date("Date of symptom onset", expand=c(0, 0), limits=c(as.Date("2020-01-20"), as.Date("2020-03-16"))+c(0, 0.5)) +
  coord_cartesian(clip = 'off') +
  ggtitle("C. Backward confirmation-to-onset delay") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

g4 <- ggplot(covid_delay) +
  geom_boxplot(aes(date_onset, delay, group=date_onset)) +
  geom_ribbon(data=estdata, aes(date, ymin=lwr, ymax=upr), alpha=0.4) +
  geom_line(data=estdata, aes(date, median)) +
  annotate("rect", xmin=as.Date("2020-01-10"), xmax=as.Date("2020-01-20"), ymin=-Inf, ymax=Inf, fill="orange", alpha=0.3) +
  geom_vline(xintercept=as.Date("2020-01-28"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-07"), lty=2) +
  geom_vline(xintercept=as.Date("2020-02-20"), lty=2) +
  geom_vline(xintercept=as.Date("2020-03-02"), lty=2) +
  # annotate("text", x=as.Date("2020-01-20"), y=23.5, label="Imported cases introduced", angle=90) +
  # geom_vline(xintercept=as.Date("2020-01-19"), col="red", lwd=2) +
  scale_size_area("Number of samples", guide=FALSE, max_size=2) +
  scale_y_continuous("Symptom onset to confirmation (days)", limit=c(0, 21), expand=c(0, 0)) +
  scale_x_date("Date of symptom onset", expand=c(0, 0), limits=c(as.Date("2020-01-20"), as.Date("2020-03-16"))+c(0, 0.5)) +
  coord_cartesian(clip = 'off') +
  ggtitle("D. Forward onset-to-confirmation delay") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

gtot <- arrangeGrob(g1, g2, g3, g4, nrow=2)

ggsave("figure_report_delay.pdf", gtot, width=10, height=6)

save("estmean", file="pred_report_delay.rda")
