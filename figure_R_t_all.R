library(dplyr)
library(ggplot2); theme_set(theme_bw())

load("R_t_chungnam_censor.rda")
load("R_t_gyeongbuk_censor.rda")
load("R_t_daegu_censor.rda")
load("R_t_seoul_censor.rda")
load("R_t_gyeonggi_censor.rda")

R_t_all <- bind_rows(
  R_t_daegu_censor %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Daegu"
    ),
  R_t_seoul_censor %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Seoul"
    ),
  R_t_gyeongbuk_censor %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Gyeongsangbuk-do"
    ),
  R_t_gyeonggi_censor %>%
    bind_rows(.id="sim") %>%
    mutate(
      region="Gyeonggi-do"
    )
) %>%
  group_by(region, date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE)
  ) %>%
  ungroup %>%
  mutate(
    region=factor(region, levels=c("Daegu", "Seoul", "Gyeongsangbuk-do", "Gyeonggi-do"))
  )

g1 <- ggplot(R_t_all) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  scale_x_date("Date") +
  scale_y_continuous("Effective reproduction number", limits=c(0, NA), expand=c(0, 0)) +
  facet_wrap(~region, scale="free")+
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

ggsave("figure_R_t_all.pdf", g1, width=8, height=6)
