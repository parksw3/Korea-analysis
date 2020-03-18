library(ggplot2); theme_set(theme_bw())
library(gridExtra)

geo <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=3) %>%
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

load("traffic_daegu.rda")
load("traffic_seoul.rda")

traffic_daegu1 <- select(ungroup(filter(traffic_daegu,year==2017)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-01-20")+3, date <= as.Date("2017-03-01")+3
  )

traffic_daegu2 <- select(ungroup(filter(traffic_daegu,year==2018)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-01-20")+2, date <= as.Date("2018-03-01")+2
  )

traffic_daegu3 <- select(ungroup(filter(traffic_daegu,year==2019)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-01-20")+1, date <= as.Date("2019-03-01")+1
  )

traffic_daegu4 <- select(ungroup(filter(traffic_daegu,year==2020)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-01-20"), date <= as.Date("2020-02-29")
  )

traffic_daegu_all <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-02-29"), 1),
  traffic=c(traffic_daegu1$total,traffic_daegu2$total,traffic_daegu3$total,traffic_daegu4$total),
  mean=((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=41))
)

traffic_seoul1 <- ungroup(filter(traffic_seoul,year==2017)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-01-20")+3, date <= as.Date("2017-03-11")+3
  )

traffic_seoul2 <- ungroup(filter(traffic_seoul,year==2018)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-01-20")+2, date <= as.Date("2018-03-11")+2
  )

traffic_seoul3 <- ungroup(filter(traffic_seoul,year==2019)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-01-20")+1, date <= as.Date("2019-03-11")+1
  )

traffic_seoul4 <- ungroup(filter(traffic_seoul,year==2020)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-01-20"), date <= as.Date("2020-03-10")
  )

traffic_seoul_all <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-10"), 1),
  traffic=c(traffic_seoul1$total,traffic_seoul2$total,traffic_seoul3$total,traffic_seoul4$total),
  mean=((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=51))
)

g1 <- ggplot(traffic_daegu_all) +
  geom_bar(data=daegu, aes(date_report, cases), stat="identity", alpha=0.5) + 
  geom_line(aes(date, traffic/3e2, group=year, col=year, lwd=factor(year))) +
  geom_line(aes(date, mean/3e2), lwd=1) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Daily number of reported cases",
                     sec.axis = sec_axis(~ .*3e2, name = "Daily traffic volume, 2020"), expand=c(0, 0)) +
  scale_color_manual(values=c("gray", "gray", "gray", "red")) +
  scale_size_manual(values=c(0.7, 0.7, 0.7, 1)) +
  ggtitle("A. Daegu") +
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

g2 <- ggplot(traffic_seoul_all) +
  geom_bar(data=seoul, aes(date_report, cases), stat="identity", alpha=0.5) + 
  geom_line(aes(date, traffic/1e5, group=year, col=year, lwd=factor(year))) +
  geom_line(aes(date, mean/1e5), lwd=1) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Daily number of reported cases",
                     sec.axis = sec_axis(~ .*1e5, name = "Daily traffic volume, 2020", breaks=c(0, 2e6, 4e6, 6e6, 8e6)), expand=c(0, 0)) +
  scale_color_manual(values=c("gray", "gray", "gray", "red")) +
  scale_size_manual(values=c(0.7, 0.7, 0.7, 1)) +
  ggtitle("B. Seoul") +
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

gtot <- arrangeGrob(g1, g2, nrow=1, widths=c(1.1, 1))

ggsave("figure_compare_report.pdf", gtot, width=8, height=4)
