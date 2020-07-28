library(readxl)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
source("../R/wquant.R")

load("../analysis_R_t/R_t_seoul_censor_detect.rda")
load("../analysis_R_t/R_t_seoul_censor_raw.rda")

R0prior <- function(x) dgamma(x, shape=(2.6/2)^2, rate=(2.6/2)^2/2.6)

rt_censor_detect <- R_t_seoul_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975)
  ) %>%
  mutate(
    group="adjusted for testing criteria"
  )

rt_censor_raw <- R_t_seoul_censor_raw %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975)
  ) %>%
  mutate(
    group="raw"
  )

rt_all <- bind_rows(rt_censor_detect, rt_censor_raw) %>%
  mutate(
    group=factor(group, level=c("raw",
                                "adjusted for testing criteria"))
  )

geo <- read_xlsx("../data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3)

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

reconstruct_case_detect <- reconstruct_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  ) %>%
  mutate(
    group="adjusted for testing criteria"
  )

reconstruct_case_raw <- reconstruct_censor_raw %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  ) %>%
  mutate(
    group="raw"
  )

reconstruct_case_all <- bind_rows(reconstruct_case_detect, reconstruct_case_raw) %>%
  mutate(
    group=factor(group, level=c("raw",
                                "adjusted for testing criteria"))
  )

g1 <- ggplot(geo) +
  geom_bar(aes(as.Date(date_report)-0.5, Seoul), stat="identity", alpha=0.5) +
  geom_ribbon(data=reconstruct_case_all, aes(date, ymin=lwr, ymax=upr, col=group, fill=group, lty=group), alpha=0.3) +
  geom_line(data=reconstruct_case_all, aes(date, median, col=group, lty=group)) +
  geom_vline(xintercept=as.Date("2020-03-10"), lty=2) +
  scale_color_manual(values=c(2, 1)) +
  scale_fill_manual(values=c(2, 1)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-14"))) +
  scale_y_continuous("Reconstructed incidence", limits=c(0, 110), expand=c(0, 0),
                     sec.axis = sec_axis(~ ., name = "Daily number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.31, 0.8),
    legend.title = element_blank(),
    plot.margin = margin(5, 5, 5, 20, unit="pt")
  )

g2 <- ggplot(rt_all) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr, col=group, fill=group), alpha=0.3) +
  geom_line(aes(date, median, col=group, lty=group)) +
  geom_hline(yintercept=1, lty=2) + 
  geom_vline(xintercept=as.Date("2020-03-10"), lty=2) +
  scale_color_manual(values=c(2, 1)) +
  scale_fill_manual(values=c(2, 1)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-14"))) +
  scale_y_continuous("Time-dependent reproduction number", limits=c(0, 8), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    plot.margin = margin(5, 38, 5, 30, unit="pt")
  )

ggsave("EID-20-1099-AppendixFigure6A.jpg", g1 + theme(plot.title = element_blank()), width=6,height=3, dpi=600)
ggsave("EID-20-1099-AppendixFigure6B.jpg", g2 + theme(plot.title = element_blank()), width=6,height=3, dpi=600)
