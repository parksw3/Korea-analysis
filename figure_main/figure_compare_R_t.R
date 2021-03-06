library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
library(gridExtra)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
source("../R/wquant.R")

load("../analysis_R_t/R_t_daegu_censor_detect.rda")

reconstruct_daegu <- reconstruct_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  )

load("../analysis_R_t/R_t_seoul_censor_detect.rda")

reconstruct_seoul <- reconstruct_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case),
    lwr=quantile(case, 0.025),
    upr=quantile(case, 0.975)
  )

load("../data_processed/traffic_daegu.rda")
load("../data_processed/traffic_seoul.rda")

R0prior <- function(x) dgamma(x, shape=(2.6/2)^2, rate=(2.6/2)^2/2.6)

geo <- read_xlsx("../data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
  mutate(
    date_report=as.Date(date_report)
  ) 

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

daegu <- geo %>% 
  group_by(date_report) %>%
  summarize(
    cases=sum(Daegu, na.rm=TRUE)
  )

seoul <- geo %>% 
  group_by(date_report) %>%
  summarize(
    cases=sum(Seoul, na.rm=TRUE)
  )

traffic_daegu1 <- dplyr::select(ungroup(filter(traffic_daegu,year==2017)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-01-20")+3, date <= as.Date("2017-03-17")+3
  )

traffic_daegu2 <- dplyr::select(ungroup(filter(traffic_daegu,year==2018)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-01-20")+2, date <= as.Date("2018-03-17")+2
  )

traffic_daegu3 <- dplyr::select(ungroup(filter(traffic_daegu,year==2019)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-01-20")+1, date <= as.Date("2019-03-17")+1
  )

traffic_daegu4 <- dplyr::select(ungroup(filter(traffic_daegu,year==2020)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-01-20"), date <= as.Date("2020-03-16")
  )

rt_daegu <- R_t_daegu_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975),
    location="Daegu"
  )

daegu_traffic <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-16"), 1),
  traffic=traffic_daegu4$total/((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3)
)

traffic_seoul1 <- ungroup(filter(traffic_seoul,year==2017)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-01-20")+3, date <= as.Date("2017-03-17")+3
  )

traffic_seoul2 <- ungroup(filter(traffic_seoul,year==2018)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-01-20")+2, date <= as.Date("2018-03-17")+2
  )

traffic_seoul3 <- ungroup(filter(traffic_seoul,year==2019)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-01-20")+1, date <= as.Date("2019-03-17")+1
  )

traffic_seoul4 <- ungroup(filter(traffic_seoul,year==2020)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-01-20"), date <= as.Date("2020-03-16")
  )

rt_seoul <- R_t_seoul_censor_detect %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975),
    location="Seoul"
  )

seoul_traffic <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-16"), 1),
  traffic=traffic_seoul4$total/((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3)
)

daegu_merge <- merge(rt_daegu, daegu_traffic)
cor.test(daegu_merge$median, daegu_merge$traffic)

seoul_merge <- merge(rt_seoul, seoul_traffic)
cor.test(seoul_merge$median, seoul_merge$traffic)

g1 <- ggplot(reconstruct_daegu) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Daegu", hjust=0, vjust=1) +
  geom_bar(data=daegu, aes(date_report, cases), stat="identity", alpha=0.3) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median), lwd=1) +
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_color_manual(values=c(1, 2, 4)) +
  scale_fill_manual(values=c(1, 2, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,1)) +
  scale_y_continuous("Reconstructed incidence", expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()
  )

g2 <- ggplot(reconstruct_seoul) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Seoul", hjust=0, vjust=1) +
  geom_bar(data=seoul, aes(date_report, cases), stat="identity", alpha=0.3) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median), lwd=1) +
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_color_manual(values=c(1, 2, 4)) +
  scale_fill_manual(values=c(1, 2, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,1)) +
  scale_y_continuous("Reconstructed incidence", expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()
  )

g3 <- ggplot(rt_daegu) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Daegu", hjust=0, vjust=1) +
  geom_hline(yintercept=6, lty=2, col=2) + 
  geom_line(data=daegu_traffic, aes(date, traffic*6), col=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median), lwd=1) +
  geom_hline(yintercept=1, lty=2) + 
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_color_manual(values=c(1, 2, 4)) +
  scale_fill_manual(values=c(1, 2, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,0.5)) +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, 8), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*1/6, name = "(Daily traffic, 2020)/(Mean daily traffic, 2017 - 2019)")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.title.y.right = element_text(color="red"),
    axis.text.y.right = element_text(color="red")
  )

g4 <- ggplot(rt_seoul) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Seoul", hjust=0, vjust=1) +
  geom_hline(yintercept=6, lty=2, col=2) + 
  geom_line(data=seoul_traffic, aes(date, traffic*6), col=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median), lwd=1) +
  geom_hline(yintercept=1, lty=2) + 
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,0.5)) +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, 8), expand=c(0, 0),
                     sec.axis = sec_axis(~ ./6, name = "(Daily traffic, 2020)/(Mean daily traffic, 2017 - 2019)")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.title.y.right = element_text(color="red"),
    axis.text.y.right = element_text(color="red")
  )

ggsave("EID-20-1099-figure2A.jpg", g1 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)
ggsave("EID-20-1099-figure2B.jpg", g2 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)
ggsave("EID-20-1099-figure2C.jpg", g3 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)
ggsave("EID-20-1099-figure2D.jpg", g4 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)

g5 <- ggplot(bind_rows(mutate(daegu_merge, region="Daegu"), mutate(seoul_merge, region="Seoul"))) +
  geom_point(aes(traffic, median, shape=region, col=region), size=3) +
  scale_x_continuous("(Daily traffic, 2020)/(Mean daily traffic, 2017 - 2019)") +
  scale_y_continuous("Instantaneous reproduction number") +
  scale_shape_manual(values=c(21, 22)) +
  scale_color_manual(values=c("blue", "black")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.13, 0.9),
    legend.title = element_blank()
  )

ggsave("EID-20-1099-AppendixFigure2.jpg", g5, width=4, height=4, dpi=600)
