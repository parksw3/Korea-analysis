library(readxl)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
source("../R/wquant.R")

load("../analysis_R_t/R_t_seoul_censor_detect.rda")
load("../analysis_R_t/R_t_seoul_censor_linelist.rda")

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
    group="main analysis"
  )

rt_censor_linelist <- R_t_seoul_censor_linelist %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975)
  ) %>%
  mutate(
    group="analysis based on public line list"
  )

rt_all <- bind_rows(rt_censor_detect, rt_censor_linelist) %>%
  mutate(
    group=factor(group, level=c("main analysis",
                                "analysis based on public line list"))
  )

covid_seoul <- read_xlsx("../data/COVID19-Seoul.xlsx", sheet=1, na="NA") %>%
  mutate(
    date_onset=as.Date(date_onset)
  ) %>%
  mutate(
    import=!is.na(import_source),
    import=ifelse(import, "imported", "local")
  )

reconstruct_case_detect <- reconstruct_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  ) %>%
  mutate(
    group="main analysis"
  )

reconstruct_case_linelist <- reconstruct_censor_linelist %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  ) %>%
  mutate(
    group="analysis based on public line list"
  )

reconstruct_case_all <- bind_rows(reconstruct_case_detect, reconstruct_case_linelist) %>%
  mutate(
    group=factor(group, level=c("main analysis",
                                "analysis based on public line list"))
  )

g1 <- ggplot(covid_seoul) +
  geom_bar(aes(as.Date(date_confirm)-0.5, fill=import), alpha=0.5) +
  geom_ribbon(data=reconstruct_case_linelist, aes(date, ymin=lwr, ymax=upr), alpha=0.3, fill=2, col=2) +
  geom_line(data=reconstruct_case_linelist, aes(date, median), col=2) +
  scale_color_manual(values=c(2, 1)) +
  scale_fill_manual(values=c(3, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-14"))) +
  scale_y_continuous("Reconstructed incidence", limits=c(0, 50), expand=c(0, 0),
                     sec.axis = sec_axis(~ ., name = "Daily number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.2, 0.95),
    legend.title = element_blank()
  )

g2 <- ggplot(rt_all) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr, col=group, fill=group), alpha=0.3) +
  geom_line(aes(date, median, col=group, lty=group)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_color_manual(values=c(1, 2)) +
  scale_fill_manual(values=c(1, 2)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-14"))) +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, 8), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.title = element_blank(),
    legend.position = c(0.33, 0.95)
  )

ggsave("EID-20-1099-AppendixFigure9A.jpg", g1 + theme(plot.title = element_blank()), width=4,height=4, dpi=600)
ggsave("EID-20-1099-AppendixFigure9B.jpg", g2 + theme(plot.title = element_blank()), width=4,height=4, dpi=600)
