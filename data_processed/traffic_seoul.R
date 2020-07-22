library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2); theme_set(theme_bw())
library(readxl)

whichfilter <- c("1호선", "2호선", "3호선", "4호선", "5호선", "6호선", "7호선", "8호선", "9호선",
                 "9호선2~3단계", "9호선2단계")

ss2020_5 <- read.csv("../data_traffic_seoul/서울시 지하철호선별 역별 승하차 인원 정보.csv", stringsAsFactors=FALSE, fileEncoding="EUC-KR") %>%
  filter(호선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-05", "2020-05-", 사용일자))
  ) %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2020_4 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_202004.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-04", "2020-04-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2020_3 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_202003.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-03", "2020-03-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2020_2 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_202002.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2020", "2020-", 사용일자),
    date=as.Date(gsub("2020-02", "2020-02-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2020
  )

ss2020_1 <- read_xlsx("../data_traffic_seoul/CARD_SUBWAY_MONTH_202001.xlsx", skip=1) %>%
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

ss2019_5 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201905.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2019", "2019-", 사용일자),
    date=as.Date(gsub("2019-05", "2019-05-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2019
  )

ss2019_4 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201904.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2019", "2019-", 사용일자),
    date=as.Date(gsub("2019-04", "2019-04-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2019
  )

ss2019_3 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201903.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2019_2 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201902.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2019_1 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201901.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2018_5 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201805.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2018", "2018-", 사용일자),
    date=as.Date(gsub("2018-05", "2018-05-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2018
  )

ss2018_4 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201804.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2018", "2018-", 사용일자),
    date=as.Date(gsub("2018-04", "2018-04-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2018
  )

ss2018_3 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201803.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2018_2 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201802.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2018_1 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201801.csv", stringsAsFactors=FALSE, fileEncoding="utf8") %>%
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

ss2017_5 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201705.csv", stringsAsFactors=FALSE, fileEncoding="utf8")  %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2017", "2017-", 사용일자),
    date=as.Date(gsub("2017-05", "2017-05-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2017
  )

ss2017_4 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201704.csv", stringsAsFactors=FALSE, fileEncoding="utf8")  %>%
  filter(노선명 %in% whichfilter) %>%
  mutate(
    사용일자=gsub("2017", "2017-", 사용일자),
    date=as.Date(gsub("2017-04", "2017-04-", 사용일자))
  ) %>%
  group_by(date) %>%
  summarize(
    total=sum(승차총승객수),
    year=2017
  )

ss2017_3 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201703.csv", stringsAsFactors=FALSE, fileEncoding="utf8")  %>%
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

ss2017_2 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201702.csv", stringsAsFactors=FALSE, fileEncoding="EUC-KR") %>%
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

ss2017_1 <- read.csv("../data_traffic_seoul/CARD_SUBWAY_MONTH_201701.csv", stringsAsFactors=FALSE, fileEncoding="EUC-KR") %>%
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

traffic_seoul <- ssall <- rbind(
  ss2020_1, ss2020_2, ss2020_3, ss2020_4, ss2020_5,
  ss2019_1, ss2019_2, ss2019_3, ss2019_4, ss2019_5,
  ss2018_1, ss2018_2, ss2018_3, ss2018_4, ss2018_5,
  ss2017_1, ss2017_2, ss2017_3, ss2017_4, ss2017_5) %>%
  arrange(date) %>%
  mutate(
    year=factor(year),
    location="Seoul"
  )

save("traffic_seoul", file="traffic_seoul.rda")
