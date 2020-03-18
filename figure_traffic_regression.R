library(ggplot2); theme_set(theme_bw())

load("R_t_daegu_censor.rda")
load("R_t_seoul_censor.rda")
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

rt_daegu <- R_t_daegu_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE),
    location="Daegu"
  ) %>%
  filter(date <= as.Date("2020-02-29")) %>%
  mutate(
    traffic=traffic_daegu4$total/((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3)
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

rt_seoul <- R_t_seoul_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE),
    location="Seoul"
  ) %>%
  mutate(
    traffic=traffic_seoul4$total/((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3)
  )

rt_all <- bind_rows(rt_daegu, rt_seoul) %>%
  mutate(width=(upr-lwr)/median)

g1 <- ggplot(rt_all) +
  geom_point(aes(traffic, median, col=location, size=1/width, shape=location)) +
  # geom_errorbar(aes(traffic, ymin=lwr, ymax=upr, col=location)) +
  geom_smooth(aes(traffic, median, col=location, fill=location, lty=location, weight=1/width), method="lm", fullrange=TRUE) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("(Daily traffic, 2020)/(Mean daily traffic, 2017 - 2019)", limits=c(0, 1.3), expand=c(0, 0)) +
  scale_y_continuous("Effective reproduction number", limits=c(0, 3.5), expand=c(0, 0)) +
  scale_shape_manual(values=c(1, 2)) +
  scale_color_manual(values=c(1, 2)) +
  scale_fill_manual(values=c(1, 2)) +
  scale_size_area(guide=FALSE) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.1, 0.9),
    legend.title = element_blank()
  )

ggsave("figure_traffic_regression.pdf", g1, width=6, height=4)
