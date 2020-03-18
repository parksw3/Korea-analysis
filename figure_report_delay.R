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

covid_test <- read_xlsx("data/COVID19-Korea-2020-03-11.xlsx", na="NA", sheet=2) 

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
  date=seq.Date(from=as.Date("2020-01-10"), to=as.Date("2020-03-11"), by=1),
  median=apply(estmean, 1, median),
  lwr=apply(estmean, 1, quantile, 0.025),
  upr=apply(estmean, 1, quantile, 0.975)
) %>%
  group_by(date) %>%
  summarize(
    median=median(median),
    lwr=median(lwr),
    upr=median(upr)
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
    diff2=diff(c(0, positive))
  )

g1 <- ggplot(covid_delay2) +
  geom_bar(data=covid_test2, aes(as.Date(date_report)-0.5, diff/1000), stat="identity", fill="red",alpha=0.3) +
  geom_ribbon(data=estdata, aes(date, ymin=lwr, ymax=upr), alpha=0.4) +
  geom_line(data=estdata, aes(date, median)) +
  geom_errorbar(aes(date_onset, ymin=min, ymax=max), width=0) +
  geom_point(aes(date_onset, mean, size=size)) +
  geom_line(aes(date_onset, absmax), lty=2) +
  annotate("rect", xmin=as.Date("2020-01-10"), xmax=as.Date("2020-01-19"), ymin=-Inf, ymax=Inf, fill="orange", alpha=0.3) +
  # annotate("text", x=as.Date("2020-01-20"), y=23.5, label="Imported cases introduced", angle=90) +
  # geom_vline(xintercept=as.Date("2020-01-19"), col="red", lwd=2) +
  scale_size_area("Number of samples", guide=FALSE, max_size=2) +
  scale_y_continuous("Symptom onset to confirmation (days)", limit=c(0, 22), expand=c(0, 0),
                     sec.axis = sec_axis(~ . * 1000, name = "Daily number of tests")) +
  scale_x_date("Date of symptom onset", expand=c(0, 0), limits=c(as.Date("2020-01-19"), as.Date("2020-03-10"))) +
  coord_cartesian(clip = 'off') +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83),
    axis.line.y.right = element_line(color = "red"),
    axis.title.y.right = element_text(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.ticks.y.right = element_line(color = "red")
  )

g2 <- ggplot(covid_delay2) +
  geom_bar(data=covid_test2, aes(as.Date(date_report)-0.5, diff2/50), stat="identity", fill="blue",alpha=0.3) +
  geom_ribbon(data=estdata, aes(date, ymin=lwr, ymax=upr), alpha=0.4) +
  geom_line(data=estdata, aes(date, median)) +
  geom_errorbar(aes(date_onset, ymin=min, ymax=max), width=0) +
  geom_point(aes(date_onset, mean, size=size)) +
  geom_line(aes(date_onset, absmax), lty=2) +
  annotate("rect", xmin=as.Date("2020-01-10"), xmax=as.Date("2020-01-19"), ymin=-Inf, ymax=Inf, fill="orange", alpha=0.3) +
  # annotate("text", x=as.Date("2020-01-20"), y=23.5, label="Imported cases introduced", angle=90) +
  # geom_vline(xintercept=as.Date("2020-01-19"), col="red", lwd=2) +
  scale_size_area("Number of samples", guide=FALSE, max_size=2) +
  scale_y_continuous("Symptom onset to confirmation (days)", limit=c(0, 22), expand=c(0, 0),
                     sec.axis = sec_axis(~ . * 50, name = "Daily number of cases")) +
  scale_x_date("Date of symptom onset", expand=c(0, 0), limits=c(as.Date("2020-01-19"), as.Date("2020-03-10"))) +
  coord_cartesian(clip = 'off') +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83),
    axis.line.y.right = element_line(color = "blue"),
    axis.title.y.right = element_text(color="blue"),
    axis.text.y.right = element_text(color="blue"),
    axis.ticks.y.right = element_line(color = "blue")
  )

gtot <- arrangeGrob(g1, g2, nrow=2)

ggsave("figure_report_delay.pdf", gtot, width=6, height=8)

save("estmean", file="pred_report_delay.rda")
