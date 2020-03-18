library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)

load("test_weight.rda")

geo <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
  mutate(
    date_report=as.Date(date_report)
  ) 

geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] <- geo$date_report[geo$time_report==16 & !is.na(geo$time_report)] + 1

daegu <- geo %>% 
  group_by(date_report) %>%
  summarize(
    cases=sum(Daegu, na.rm=TRUE)
  ) %>%
  merge(test_weight) %>%
  mutate(
    cases2=cases*weight,
    cases3=round(cases2*weight2),
    cases2=round(cases2)
  ) %>%
  gather(
    key, value,-date_report, -meanp, -test, -weight, -weight2, -group
  ) %>%
  mutate(
    date_report=as.Date(date_report),
    key=factor(key, levels=c("cases", "cases2", "cases3"),
               labels=c("raw", "adjusted for detection rate", "adjusted for detection rate + number of tests"))
  )

seoul <- geo %>% 
  group_by(date_report) %>%
  summarize(
    cases=sum(Seoul, na.rm=TRUE)
  ) %>%
  merge(test_weight) %>%
  mutate(
    cases2=cases*weight,
    cases3=round(cases2*weight2),
    cases2=round(cases2)
  ) %>%
  gather(
    key, value,-date_report, -meanp, -test, -weight, -weight2, -group
  ) %>%
  mutate(
    date_report=as.Date(date_report),
    key=factor(key, levels=c("cases", "cases2", "cases3"),
               labels=c("raw", "adjusted for detection rate", "adjusted for detection rate + number of tests"))
  )

g1 <- ggplot(daegu) +
  geom_point(aes(date_report, value, col=key, shape=key)) +
  geom_line(aes(date_report, value, col=key, lty=key)) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Scaled number of reported cases", expand=c(0, 0)) +
  scale_color_manual(values=c(1, 2, 4)) +
  ggtitle("A. Daegu") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.2, 0.8),
    legend.title = element_blank(),
    axis.line.y.right = element_line(color = "blue"),
    axis.title.y.right = element_text(color="blue"),
    axis.text.y.right = element_text(color="blue"),
    axis.ticks.y.right = element_line(color = "blue"),
    plot.margin = margin(5, 5, 5, 5)
  )

g2 <- ggplot(seoul) +
  geom_point(aes(date_report, value, col=key, shape=key)) +
  geom_line(aes(date_report, value, col=key, lty=key)) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Scaled number of reported cases", expand=c(0, 0)) +
  scale_color_manual(values=c(1, 2, 4)) +
  ggtitle("B. Seoul") +  
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color = "blue"),
    axis.title.y.right = element_text(color="blue"),
    axis.text.y.right = element_text(color="blue"),
    axis.ticks.y.right = element_line(color = "blue"),
    plot.margin = margin(5, 5, 5, 15)
  )

gtot <- arrangeGrob(g1, g2, nrow=2)

ggsave("figure_adjusted_timeseries.pdf", gtot, width=8, height=6)

