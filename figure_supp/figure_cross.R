library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
library(gridExtra)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
source("../R/wquant.R")

R0prior <- function(x) dgamma(x, shape=(2.6/2)^2, rate=(2.6/2)^2/2.6)

load("../analysis_R_t/R_t_daegu_censor_detect.rda")
load("../analysis_R_t/R_t_seoul_censor_detect.rda")
load("../data_processed/traffic_daegu.rda")
load("../data_processed/traffic_seoul.rda")

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



c1 <- ccf(
  merge(daegu_traffic, rt_daegu)$traffic,
  merge(daegu_traffic, rt_daegu)$median
)

c2 <- ccf(
  merge(seoul_traffic, rt_seoul)$traffic,
  merge(seoul_traffic, rt_seoul)$median
)

c1data <- data.frame(
  lag=c(c1$lag),
  ccf=c(c1$acf)
)

c2data <- data.frame(
  lag=c(c2$lag),
  ccf=c(c2$acf)
)

g1 <- ggplot(c1data) +
  annotate("text", x=-Inf, y=Inf, label="Daegu", hjust=0, vjust=1) +
  geom_point(aes(lag, ccf), stat="identity", fill=NA, col=1) +
  geom_segment(aes(x=lag, xend=lag, y=0, yend=ccf), stat="identity", fill=NA, col=1) +
  geom_hline(yintercept=0) +
  xlab("Lag (days)") +
  scale_y_continuous("Cross correlation", limits=c(-1, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

g2 <- ggplot(c2data) +
  annotate("text", x=-Inf, y=Inf, label="Seoul", hjust=0, vjust=1) +
  geom_point(aes(lag, ccf), stat="identity", fill=NA, col=1) +
  geom_segment(aes(x=lag, xend=lag, y=0, yend=ccf), stat="identity", fill=NA, col=1) +
  geom_hline(yintercept=0) +
  xlab("Lag (days)") +
  scale_y_continuous("Cross correlation", limits=c(-1, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.87, 0.83)
  )

ggsave("EID-20-1099-AppendixFigure3A.jpg", g1 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)
ggsave("EID-20-1099-AppendixFigure3B.jpg", g2 + theme(plot.title = element_blank()), width=4, height=4, dpi=600)
