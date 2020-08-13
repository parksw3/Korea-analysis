library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
source("../R/wquant.R")

load("../analysis_R_t/R_t_gyeongbuk_censor_detect.rda")
load("../analysis_R_t/R_t_daegu_censor_detect.rda")
load("../analysis_R_t/R_t_seoul_censor_detect.rda")
load("../analysis_R_t/R_t_gyeonggi_censor_detect.rda")

R0prior <- function(x) dgamma(x, shape=(2.6/2)^2, rate=(2.6/2)^2/2.6)

R_t_all <- bind_rows(
  R_t_daegu_censor_detect %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Daegu"
    ),
  R_t_seoul_censor_detect %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Seoul"
    ),
  R_t_gyeongbuk_censor_detect %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Gyeongsangbuk-do"
    ),
  R_t_gyeonggi_censor_detect %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Gyeonggi-do"
    )
) %>%
  group_by(region, date) %>%
  mutate(weight=R0prior(IRt)) %>%
  summarize(
    median=wquant(IRt, weights=weight,0.5),
    lwr=wquant(IRt, weights=weight,0.025),
    upr=wquant(IRt, weights=weight,0.975)
  ) %>%
  ungroup %>%
  mutate(
    region=factor(region, levels=c("Daegu", "Seoul", "Gyeongsangbuk-do", "Gyeonggi-do"))
  )

geo <- read_xlsx("../data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
  mutate(
    date_report=as.Date(date_report)
  ) 

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

geo_all <- geo %>% group_by(date_report) %>%
  summarize(
    Daegu=sum(Daegu, na.rm=TRUE),
    Seoul=sum(Seoul, na.rm=TRUE),
    `Gyeongsangbuk-do`=sum(`Gyeongsangbuk-do`, na.rm=TRUE),
    `Gyeonggi-do`=sum(`Gyeonggi-do`, na.rm=TRUE)
  ) %>%
  gather(key, value,- date_report) %>%
  rename(
    region=key
  ) %>%
  mutate(
    region=factor(region, levels=c("Daegu", "Seoul", "Gyeongsangbuk-do", "Gyeonggi-do"))
  )
 
g1 <- ggplot(filter(R_t_all, region=="Daegu")) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Daegu", hjust=0, vjust=1) +
  geom_bar(data=filter(geo_all, region=="Daegu"), aes(date_report, value/150), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*150, name = "Number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g2 <- ggplot(filter(R_t_all, region=="Gyeongsangbuk-do")) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Gyeongsangbuk-do", hjust=0, vjust=1) +
  geom_bar(data=filter(geo_all, region=="Gyeongsangbuk-do"), aes(date_report, value/30), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*30, name = "Number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g3 <- ggplot(filter(R_t_all, region=="Seoul")) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Seoul", hjust=0, vjust=1) +
  geom_bar(data=filter(geo_all, region=="Seoul"), aes(date_report, value/8), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*8, name = "Number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g4 <- ggplot(filter(R_t_all, region=="Gyeonggi-do")) +
  annotate("text", x=as.Date("2020-01-21"), y=Inf, label="Gyeonggi-do", hjust=0, vjust=1) +
  geom_bar(data=filter(geo_all, region=="Gyeonggi-do"), aes(date_report, value/3), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.15) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Instantaneous reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*3, name = "Number of reported cases")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

ggsave("EID-20-1099-AppendixFigure4A.jpg", g1, width=4, height=4, dpi=600)
ggsave("EID-20-1099-AppendixFigure4B.jpg", g3, width=4, height=4, dpi=600)
ggsave("EID-20-1099-AppendixFigure4C.jpg", g2, width=4, height=4, dpi=600)
ggsave("EID-20-1099-AppendixFigure4D.jpg", g4, width=4, height=4, dpi=600)
