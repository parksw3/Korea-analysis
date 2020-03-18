library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2); theme_set(theme_bw())

summfun <- function(x) {
  x %>%
    filter(승하=="승차") %>%
    group_by(월, 일) %>%
    summarize(
      total=sum(일계)
    )
}

summfun2 <- function(x) {
  x %>%
    filter(승하차=="승차") %>%
    group_by(월, 일) %>%
    summarize(
      total=sum(일계)
    )
}

dd2017 <- read.csv("daegu_traffic/daegu_metro_daily_onoff_20171231.csv", stringsAsFactors=FALSE,
                   fileEncoding="EUC-KR") %>% 
  summfun %>%
  mutate(year=2017)

dd2018 <- read.csv("daegu_traffic/daegu_metro_daily_onoff_20181231.csv", stringsAsFactors=FALSE,
                   fileEncoding="EUC-KR") %>% 
  summfun2 %>%
  mutate(year=2018)

dd2019 <- read.csv("daegu_traffic/daegu_metro_daily_onoff_20191231.csv", stringsAsFactors=FALSE,
                   fileEncoding="EUC-KR") %>% 
  summfun %>%
  mutate(year=2019)

dd2020 <- read.csv("daegu_traffic/daegu_metro_daily_onoff_20200229.csv", stringsAsFactors=FALSE,
               fileEncoding="EUC-KR") %>% 
  summfun %>%
  mutate(year=2020)

traffic_daegu <- ddall <- bind_rows(dd2017, dd2018, dd2019, dd2020) %>%
  mutate(
    date=as.Date(paste0(year, "-", 월, "-", 일)),
    day=yday(date),
    year=factor(year),
    location="Daegu"
  )

save("traffic_daegu", file="traffic_daegu.rda")
