library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
source("wquant.R")

load("R_t_gyeongbuk_censor_detect.rda")
load("R_t_daegu_censor_detect.rda")
load("R_t_seoul_censor_detect.rda")
load("R_t_gyeonggi_censor_detect.rda")

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

geo <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
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
  geom_bar(data=filter(geo_all, region=="Daegu"), aes(date_report, value/150), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Time-dependent reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*150, name = "Number of reported cases")) +
  facet_wrap(~region, scale="free") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g2 <- ggplot(filter(R_t_all, region=="Gyeongsangbuk-do")) +
  geom_bar(data=filter(geo_all, region=="Gyeongsangbuk-do"), aes(date_report, value/30), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Time-dependent reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*30, name = "Number of reported cases")) +
  facet_wrap(~region, scale="free") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g3 <- ggplot(filter(R_t_all, region=="Seoul")) +
  geom_bar(data=filter(geo_all, region=="Seoul"), aes(date_report, value/8), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Time-dependent reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*8, name = "Number of reported cases")) +
  facet_wrap(~region, scale="free") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

g4 <- ggplot(filter(R_t_all, region=="Gyeonggi-do")) +
  geom_bar(data=filter(geo_all, region=="Gyeonggi-do"), aes(date_report, value/3), stat="identity", alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Time-dependent reproduction number", limits=c(0, NA), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*3, name = "Number of reported cases")) +
  facet_wrap(~region, scale="free") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

gtot <- arrangeGrob(g1, g3, g2, g4)

ggsave("figure_R_t_all.pdf", gtot, width=8, height=6)
