library(ggplot2); theme_set(theme_bw())
library(gridExtra)

load("traffic_daegu.rda")
load("traffic_seoul.rda")

traffic_daegu1 <- select(ungroup(filter(traffic_daegu,year==2017)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-02-02")+3, date <= as.Date("2017-03-01")+3
  )

traffic_daegu2 <- select(ungroup(filter(traffic_daegu,year==2018)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-02-02")+2, date <= as.Date("2018-03-01")+2
  )

traffic_daegu3 <- select(ungroup(filter(traffic_daegu,year==2019)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-02-02")+1, date <= as.Date("2019-03-01")+1
  )

traffic_daegu4 <- select(ungroup(filter(traffic_daegu,year==2020)), -월, -일) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-02-02"), date <= as.Date("2020-02-29")
  )

traffic_daegu_all <- data.frame(
  date=seq.Date(as.Date("2020-02-02"), as.Date("2020-02-29"), 1),
  traffic=c(traffic_daegu1$total,traffic_daegu2$total,traffic_daegu3$total,traffic_daegu4$total),
  mean=((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=28))
)

g1 <- ggplot(traffic_daegu_all) +
  geom_line(aes(date, traffic, group=year, col=year)) +
  geom_line(aes(date, mean)) +
  scale_x_date("Date") +
  scale_y_continuous("Daily metro traffic") +
  scale_color_manual(values=c("gray", "gray", "gray", "red")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()
  )

traffic_seoul1 <- ungroup(filter(traffic_seoul,year==2017)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2017-02-02")+3, date <= as.Date("2017-03-11")+3
  )

traffic_seoul2 <- ungroup(filter(traffic_seoul,year==2018)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2018-02-02")+2, date <= as.Date("2018-03-11")+2
  )

traffic_seoul3 <- ungroup(filter(traffic_seoul,year==2019)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2019-02-02")+1, date <= as.Date("2019-03-11")+1
  )

traffic_seoul4 <- ungroup(filter(traffic_seoul,year==2020)) %>%
  mutate(
    wday=wday(date)
  ) %>%
  filter(
    date >= as.Date("2020-02-02"), date <= as.Date("2020-03-10")
  )

traffic_seoul_all <- data.frame(
  date=seq.Date(as.Date("2020-02-02"), as.Date("2020-03-10"), 1),
  traffic=c(traffic_seoul1$total,traffic_seoul2$total,traffic_seoul3$total,traffic_seoul4$total),
  mean=((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=38))
)

g2 <- g1 %+% traffic_seoul_all

gtot <- arrangeGrob(g1 + ggtitle("Daegu"), g2  + ggtitle("Seoul"), nrow=1)

ggsave("figure_traffic.pdf", gtot, width=8, height=5)
