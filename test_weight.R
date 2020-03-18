library(readxl)

tt <- read_xlsx("data/COVID19-Korea-2020-03-16.xlsx", na="NA", sheet=2)

tt$negative[7] <- (tt$negative[6]+tt$negative[8])/2
tt$negative[13] <- (tt$negative[12]+tt$negative[14])/2

test <- tt %>%
  mutate(
    date_report=as.Date(date_report),
    time_report=hour(time_report)
  ) %>%
  filter(
    is.na(time_report) | time_report != 16
  ) %>%
  mutate(
    test=positive+negative,
    test=diff(c(0, test)),
    positive=diff(c(0, positive))
  ) %>%
  filter(
    date_report >= as.Date("2020-01-20")
  ) %>%
  mutate(
    group=1,
    group=ifelse(date_report >= as.Date("2020-01-28"), 2, group),
    group=ifelse(date_report >= as.Date("2020-02-07"), 3, group),
    group=ifelse(date_report >= as.Date("2020-02-20"), 4, group),
    group=ifelse(date_report >= as.Date("2020-03-02"), 5, group),
    group=factor(group)
  )

test_weight <- test %>%
  group_by(group) %>%
  summarize(
    meanp=sum(positive)/sum(test)
  ) %>%
  mutate(
    weight=meanp/mean(meanp)
  ) %>%
  merge(select(test, date_report, group, test)) %>%
  group_by(group) %>%
  mutate(
    weight2=mean(test)/test
  )

save("test_weight", file="test_weight.rda")
