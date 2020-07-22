library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
library(gridExtra)
library(lubridate)
library(raster)

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

load("../data_processed/traffic_daegu.rda")
load("../data_processed/traffic_seoul.rda")

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

traffic_daegu_all <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-16"), 1),
  traffic=c(traffic_daegu1$total,traffic_daegu2$total,traffic_daegu3$total,traffic_daegu4$total),
  mean=((traffic_daegu1$total + traffic_daegu2$total + traffic_daegu3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=57))
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

traffic_seoul_all <- data.frame(
  date=seq.Date(as.Date("2020-01-20"), as.Date("2020-03-16"), 1),
  traffic=c(traffic_seoul1$total,traffic_seoul2$total,traffic_seoul3$total,traffic_seoul4$total),
  mean=((traffic_seoul1$total + traffic_seoul2$total + traffic_seoul3$total)/3),
  year=factor(rep(c(2017, 2018, 2019, 2020), each=57))
)

data_range <- seq.Date(as.Date("2020-01-20"), as.Date("2020-03-16"), 1)

weekend_data <- data.frame(
  begin=data_range[wday(data_range)==7],
  end=data_range[wday(data_range)==1]
)

g1 <- ggplot(filter(traffic_daegu_all, year==2020)) +
  annotate("rect", xmin=weekend_data$begin, xmax=weekend_data$end, ymin=-Inf, ymax=Inf, alpha=0.2) +
  geom_vline(xintercept=weekend_data$end, col="gray") +
  annotate("text", x=as.Date("2020-01-20"), y=Inf, label="Daegu", hjust=0, vjust=1) +
  geom_bar(data=daegu, aes(date_report, cases), stat="identity", fill="gray40") + 
  geom_line(aes(date, mean/3e2)) +
  geom_line(aes(date, traffic/3e2, group=year, col=year)) +
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Daily number of reported cases",
                     sec.axis = sec_axis(~ .*3e2, name = "Daily traffic volume, 2020"), expand=c(0, 0)) +
  scale_color_manual(values=c("red")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.title.y.right = element_text(color="red"),
    axis.text.y.right = element_text(color="red")
  )

g2 <- ggplot(filter(traffic_seoul_all, year==2020)) +
  annotate("rect", xmin=weekend_data$begin, xmax=weekend_data$end, ymin=-Inf, ymax=Inf, alpha=0.2) +
  geom_vline(xintercept=weekend_data$end, col="gray") +
  annotate("text", x=as.Date("2020-01-20"), y=Inf, label="Seoul", hjust=0, vjust=1) +
  geom_bar(data=seoul, aes(date_report, cases), stat="identity", fill="gray40") + 
  geom_line(aes(date, mean/1e5)) +
  geom_line(aes(date, traffic/1e5, group=year, col=year)) +
  geom_vline(xintercept=as.Date("2020-02-18"), lty=2) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Daily number of reported cases",
                     sec.axis = sec_axis(~ .*1e5, name = "Daily traffic volume, 2020", breaks=c(0, 2e6, 4e6, 6e6, 8e6)), expand=c(0, 0)) +
  scale_color_manual(values=c("red")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.title.y.right = element_text(color="red"),
    axis.text.y.right = element_text(color="red")
  )

ggsave("EID-20-1099-figure1A.jpg", g1, width=6, height=4, dpi=600)
ggsave("EID-20-1099-figure1B.jpg", g2, width=6, height=4, dpi=600)
