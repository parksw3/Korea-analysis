library(ggplot2); theme_set(theme_bw())
library(gridExtra)

load("R_t_daegu_censor.rda")
load("R_t_seoul_censor.rda")
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

rt_daegu <- R_t_daegu_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE),
    location="Daegu"
  )

daegu_traffic <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-02-29"), 1),
  traffic=traffic_daegu4$total/((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3)
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

rt_seoul <- R_t_seoul_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE),
    location="Seoul"
  )

seoul_traffic <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-10"), 1),
  traffic=traffic_seoul4$total/((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3)
)

g1 <- ggplot(rt_daegu) +
  geom_bar(data=daegu, aes(date_report, cases/max(daegu$cases)), stat="identity", alpha=0.5) + 
  geom_hline(yintercept=6, lty=2, col=2) + 
  geom_line(data=daegu_traffic, aes(date, traffic*6), col=2, lwd=1) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_color_manual(values=c(1, 2, 4)) +
  scale_fill_manual(values=c(1, 2, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,0.5)) +
  scale_y_continuous("Effective reproduction number", limits=c(0, 8.5), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*1/6, name = "(Daily traffic, 2020)/(Mean daily traffic, 2017 - 2019)")) +
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

g2 <- ggplot(rt_seoul) +
  geom_bar(data=seoul, aes(date_report, cases/12), stat="identity", alpha=0.5) + 
  geom_hline(yintercept=6, lty=2, col=2) + 
  geom_line(data=seoul_traffic, aes(date, traffic*6), col=2, lwd=1) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-20", "2020-03-16"))+c(0,0.5)) +
  ggtitle("B. Seoul") +
  scale_y_continuous("Effective reproduction number", limits=c(0, 8.5), expand=c(0, 0),
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

gtot <- arrangeGrob(g1, g2, nrow=1, widths=c(1.1, 1))

ggsave("figure_compare_R_t.pdf", gtot, width=8, height=4.5)
