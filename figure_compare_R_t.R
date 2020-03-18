library(ggplot2); theme_set(theme_bw())
library(gridExtra)

load("R_t_daegu_censor.rda")

rt_daegu <- R_t_daegu_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE)
  )

case_daegu <- reconstruct_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case, na.rm=TRUE),
    lwr=quantile(case, 0.025, na.rm=TRUE),
    upr=quantile(case, 0.975, na.rm=TRUE)
  )

load("R_t_seoul_censor.rda")

rt_seoul <- R_t_seoul_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(IRt, na.rm=TRUE),
    lwr=quantile(IRt, 0.025, na.rm=TRUE),
    upr=quantile(IRt, 0.975, na.rm=TRUE)
  )

case_seoul <- reconstruct_censor %>%
  bind_rows(.id="sim") %>%
  group_by(date) %>%
  summarize(
    median=median(case, na.rm=TRUE),
    lwr=quantile(case, 0.025, na.rm=TRUE),
    upr=quantile(case, 0.975, na.rm=TRUE)
  )

g1 <- ggplot(rt_daegu) +
  geom_point(data=case_daegu, aes(date, median/100), col=2) +
  geom_errorbar(data=case_daegu, aes(date, ymin=lwr/100, ymax=upr/100), width=0, col=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_color_manual(values=c(1, 2, 4)) +
  scale_fill_manual(values=c(1, 2, 4)) +
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-10"))) +
  scale_y_continuous("Effective reproduction number", limits=c(0, 8.5), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*100, name = "Reconstructed incidence")) +
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
  geom_point(data=case_seoul, aes(date, median/5), col=2) +
  geom_errorbar(data=case_seoul, aes(date, ymin=lwr/5, ymax=upr/5), width=0, col=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.3) +
  geom_line(aes(date, median)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_x_date("Date", expand=c(0, 0), limits=as.Date(c("2020-01-19", "2020-03-10"))) +
  scale_y_continuous("Effective reproduction number", limits=c(0, 8.5), expand=c(0, 0),
                     sec.axis = sec_axis(~ .*5, name = "Reconstructed incidence")) +
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

ggsave("figure_compare_R_t.pdf", gtot, width=8, height=4)
