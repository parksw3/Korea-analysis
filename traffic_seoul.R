library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2); theme_set(theme_bw())
library(readxl)

whichfilter <- c("1호선", "2호선", "3호선", "4호선", "5호선", "6호선", "7호선", "8호선", "9호선",
                 "9호선2~3단계", "우이신설선", "9호선2단계")

ss2020 <- read.csv("seoul_subway/서울시 지하철호선별 역별 승하차 인원 정보.csv", fileEncoding="EUC-KR") 

ss2020a <- ss2020 %>%
  filter(호선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    사용일자=gsub("2020-03", "2020-03-", 사용일자),
    date=as.Date(gsub("2020-02", "2020-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2020b <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_202002.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-02", "2020-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  ) %>%
  filter(date=="2020-02-01")

ss2020c <- read_xlsx("seoul_subway/CARD_SUBWAY_MONTH_202001.xlsx", skip=1) %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-01", "2020-01-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2019a <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201901.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2019", "2019-", 사용일자),
    date=as.Date(gsub("2019-01", "2019-01-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2019
  )

ss2019b <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201902.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2019", "2019-", 사용일자),
    date=as.Date(gsub("2019-02", "2019-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2019
  )

ss2019c <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201903.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2019", "2019-", 사용일자),
    date=as.Date(gsub("2019-03", "2019-03-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2019
  )

ss2018a <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201801.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2018", "2018-", 사용일자),
    date=as.Date(gsub("2018-01", "2018-01-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2018
  )

ss2018b <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201802.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2018", "2018-", 사용일자),
    date=as.Date(gsub("2018-02", "2018-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2018
  )

ss2018c <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201803.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2018", "2018-", 사용일자),
    date=as.Date(gsub("2018-03", "2018-03-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2018
  )

ss2017a <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201701.csv", stringsAsFactors=FALSE, fileEncoding="EUC-KR") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2017", "2017-", 사용일자),
    date=as.Date(gsub("2017-01", "2017-01-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2017
  )

ss2017b <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201702.csv", stringsAsFactors=FALSE, fileEncoding="EUC-KR") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2017", "2017-", 사용일자),
    date=as.Date(gsub("2017-02", "2017-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2017
  )

ss2017c <- read.csv("seoul_subway/CARD_SUBWAY_MONTH_201703.csv", stringsAsFactors=FALSE, fileEncoding="utf8")  %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2017", "2017-", 사용일자),
    date=as.Date(gsub("2017-03", "2017-03-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2017
  )

traffic_seoul <- ssall <- rbind(ss2020a, ss2020b, ss2020c,
               ss2019a, ss2019b, ss2019c,
               ss2018a, ss2018b, ss2018c,
               ss2017a, ss2017b, ss2017c) %>%
  arrange(date) %>%
  mutate(
    year=factor(year),
    location="Seoul"
  )

save("traffic_seoul", file="traffic_seoul.rda")
