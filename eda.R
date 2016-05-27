sessions <- readr::read_rds("data/refined_sessions.rds")

library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2) # devtools::install_github("hadley/ggplot2")
# library(mgcv) # for geom_smooth(method = "gam")

# Let's check to see that sampling isn't biased
top_langs <- sessions %>%
  group_by(`Primary language`) %>%
  summarize(n = n()) %>%
  top_n(10, n) %>%
  arrange(desc(n)) %>%
  { .$`Primary language` }
proportions_overall <- sessions %>%
  filter(`Primary language` %in% top_langs) %>%
  group_by(group, `Primary language`) %>%
  summarize(n = n()) %>%
  mutate(section = "all events (clickthroughs + abandonments)")
proportions_clickthroughed <- sessions %>%
  filter(`Primary language` %in% top_langs & clickthrough) %>%
  group_by(group, `Primary language`) %>%
  summarize(n = n()) %>%
  mutate(section = "all sections (clickthroughs)")
p <- proportions_overall %>%
  rbind(proportions_clickthroughed) %>%
  ggplot(aes(y = n, x = `Primary language`, fill = group)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~section, ncol = 2) +
  geom_hline(yintercept = 0.50) +
  scale_y_continuous("Proportion of users", labels = scales::percent_format()) +
  theme_bw(base_family = "Gill Sans", base_size = 14) + theme(legend.position = "bottom") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  coord_flip() +
  ggtitle("Sampling of users by primary language and engagement with Portal",
          subtitle = "Using only top 10 most preferred languages by # of users")
ggsave("lang_props.png", p, path = "figures", width = 10, height = 5, dpi = 300)
rm(top_langs, proportions_overall, proportions_clickthroughed, p)

ctr_daily <- sessions %>%
  group_by(date) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  mutate(group = "Combined")
ctr_overall <- sessions %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  mutate(group = "Combined")
ctr_groups <- sessions %>%
  group_by(group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough))
p <- sessions %>%
  group_by(date, group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  ggplot(data = .) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "black"),
        panel.grid.major = element_line(color = "grey60", size = 0.25),
        panel.grid.minor = element_line(color = "grey80", size = 0.1)) +
  geom_line(data = ctr_daily, aes(x = date, y = ctr, color = group), size = 0.8, alpha = 0.75) +
  geom_hline(data = ctr_overall, aes(yintercept = ctr, color = group), size = 1.1, alpha = 1) +
  geom_line(aes(x = date, y = ctr, color = group), size = 0.8, alpha = 0.5) +
  geom_hline(data = ctr_groups, aes(yintercept = ctr, color = group), size = 1.1, alpha = 1) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set1")[1:2], "black")) +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent_format(), limits = c(0.45, 0.65)) +
  scale_x_datetime("Date", date_breaks = "4 days", date_labels = "%a (%d %b)", date_minor_breaks = "1 day") +
  ggtitle("Daily & overall clickthrough rate (CTR)",
          subtitle = "Combined overall CTR is 57%; controls' (A) overall CTR is 57.17%; test group's (B) overall CTR is 56.95%")
ggsave("daily_ctr.png", p, path = "figures", width = 10, height = 5, dpi = 300)
rm(p, ctr_daily, ctr_overall, ctr_groups)

# Let's check if daily primary/secondary link clickthrough was impacted (not accounting for language)
ctr_overall <- sessions %>%
  filter(date < "2016-04-14") %>%
  group_by(group) %>%
  summarize(primary = sum(`primary links clicks` > 0)/n(),
            secondary = sum(`secondary links clicks` > 0)/n()) %>%
  tidyr::gather(link, ctr, -group)
p <- sessions %>%
  filter(date < "2016-04-14") %>%
  group_by(date, group) %>%
  summarize(primary = sum(`primary links clicks` > 0)/n(),
            secondary = sum(`secondary links clicks` > 0)/n()) %>%
  ungroup %>%
  tidyr::gather(link, ctr, -c(date, group)) %>%
  ggplot(aes(x = date, y = ctr, color = group)) +
  geom_line(alpha = 0.5, size = 0.5) +
  geom_hline(data = ctr_overall, aes(yintercept = ctr, color = group), size = 1) +
  facet_wrap(~link, scales = "free_y", labeller = function(x) {
    x$link <- paste("Link:", x$link); return(x)
  }, nrow = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Clickthrough Rate", x = "Date",
       title = "Clickthrough rate by group & section, not accounting for language") +
  theme_bw(base_family = "Gill Sans", base_size = 14) + theme(legend.position = "bottom")
ggsave("ctr_by_group_link_all.png", p, path = "figures", width = 8, height = 12, dpi = 300)
rm(p, ctr_overall)

# Let's check if daily primary/secondary link clickthrough was impacted, accounting for language
ctr_overall <- sessions %>%
  filter(date < "2016-04-14") %>%
  filter(preferred_languages != "en") %>%
  group_by(group, `Includes English`) %>%
  summarize(primary = sum(`primary links clicks` > 0)/n(),
            secondary = sum(`secondary links clicks` > 0)/n()) %>%
  tidyr::gather(link, ctr, -c(1:2)) %>%
  mutate(`Includes English` = as.character(factor(`Includes English`, c(TRUE, FALSE),
    c("Accept-Language includes English", "Accept-Language doesn't include English")))) %>%
  ungroup
ctr_overall %>%
  select(c(`Includes English`, link, group, ctr)) %>%
  arrange(`Includes English`, link, group) %>%
  mutate(ctr = 100 * ctr) %>%
  rename(`Clickthrough Rate (%)` = ctr) %>%
  knitr::kable(digits = 2, format = "latex", caption = "Clickthrough rate by language, link, and group.")
p <- sessions %>%
  filter(date < "2016-04-14") %>%
  filter(preferred_languages != "en") %>%
  group_by(date, group, `Includes English`) %>%
  summarize(primary = sum(`primary links clicks` > 0)/n(),
            secondary = sum(`secondary links clicks` > 0)/n()) %>%
  ungroup %>%
  tidyr::gather(link, ctr, -c(date, group, `Includes English`)) %>%
  mutate(`Includes English` = as.character(factor(`Includes English`, c(TRUE, FALSE),
    c("Accept-Language includes English", "Accept-Language doesn't include English")))) %>%
  ggplot(aes(x = date, y = ctr, color = group)) +
  geom_line(alpha = 0.5, size = 0.5) +
  geom_hline(data = ctr_overall, aes(yintercept = ctr, color = group), size = 1) +
  facet_grid(link~`Includes English`, scales = "free_y") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Clickthrough Rate (CTR)", x = "Date") +
  ggtitle("Daily clickthrough rate (CTR) by group & section, accounting for language",
          subtitle = "Excluding users whose only preferred language is English; thick line represents overall CTR across days") +
  theme_bw(base_family = "Gill Sans", base_size = 14) + theme(legend.position = "bottom")
ggsave("ctr_by_group_link_nonenglish.png", p, path = "figures", width = 10, height = 6, dpi = 300)
rm(p, ctr_overall)
