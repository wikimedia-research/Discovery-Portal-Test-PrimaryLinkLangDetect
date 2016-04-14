sessions <- readr::read_rds("data/portal-lang-detect-test.rds")

library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2) # devtools::install_github("hadley/ggplot2")
library(mgcv)

# Let's check if daily search clickthrough was impacted (it should not have been)
sessions %>%
  filter(date > "2016-03-22") %>%
  filter(section == "search" | is.na(section)) %>%
  group_by(date, group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  ungroup %>%
  ggplot(aes(x = date, y = ctr, color = group)) +
  geom_line()
# Ok, let's check how overall search clickthrough was impacted
sessions %>%
  filter(date > "2016-03-22") %>%
  filter(section == "search" | is.na(section)) %>%
  group_by(group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough))
# 50%/50%, okay, that's to be expected, good

# Let's check if daily primary/secondary link clickthrough was impacted (not accounting for language)
ctr_overall <- sessions %>%
  filter(date > "2016-03-22") %>%
  group_by(group) %>%
  summarize(search = sum(clickthrough & section == "search")/length(clickthrough),
            primary = sum(clickthrough & section == "primary links")/length(clickthrough),
            secondary = sum(clickthrough & section == "secondary links")/length(clickthrough)) %>%
  tidyr::gather(link, ctr, -group)
p <- sessions %>%
  filter(date > "2016-03-22") %>%
  group_by(date, group) %>%
  summarize(search = sum(clickthrough & section == "search")/length(clickthrough),
            primary = sum(clickthrough & section == "primary links")/length(clickthrough),
            secondary = sum(clickthrough & section == "secondary links")/length(clickthrough)) %>%
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
ggsave("ctr_by_group_link_all.png", p, path = "figures", width = 8, height = 12, dpi = 150)

# Let's check if daily primary/secondary link clickthrough was impacted, accounting for language
ctr_overall <- sessions %>%
  filter(date > "2016-03-22") %>%
  filter(`Primary language` != "English") %>%
  group_by(group, `Includes English`) %>%
  summarize(search = sum(clickthrough & section == "search")/length(clickthrough),
            primary = sum(clickthrough & section == "primary links")/length(clickthrough),
            secondary = sum(clickthrough & section == "secondary links")/length(clickthrough)) %>%
  tidyr::gather(link, ctr, -c(1:2)) %>%
  mutate(`Includes English` = as.character(factor(`Includes English`, c(TRUE, FALSE),
    c("Accept-Language includes English", "Accept-Language doesn't include English"))))
# foo <- function(x) {
#   2 * as.numeric(factor(x)) - 3
# }
p <- sessions %>%
  filter(date > "2016-03-22") %>%
  filter(`Primary language` != "English") %>%
  group_by(date, group, `Includes English`) %>%
  summarize(search = sum(clickthrough & section == "search")/length(clickthrough),
            primary = sum(clickthrough & section == "primary links")/length(clickthrough),
            secondary = sum(clickthrough & section == "secondary links")/length(clickthrough)) %>%
  ungroup %>%
  tidyr::gather(link, ctr, -c(date, group, `Includes English`)) %>%
  mutate(`Includes English` = as.character(factor(`Includes English`, c(TRUE, FALSE),
    c("Accept-Language includes English", "Accept-Language doesn't include English")))) %>%
  ggplot(aes(x = date, y = ctr, color = group)) +
  geom_line(alpha = 0.5, size = 0.5) +
  geom_hline(data = ctr_overall, aes(yintercept = ctr, color = group), size = 1) +
  # geom_text(data = ctr_overall,
  #           aes(label = sprintf("%.1f%%", 100*ctr),
  #               color = group,
  #               x = as.POSIXct(lubridate::ymd("2016-04-13"))),
  #           hjust = 1, fontface = "bold") +
  facet_grid(link~`Includes English`, scales = "free_y") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Clickthrough Rate (CTR)", x = "Date") +
  ggtitle("Daily clickthrough rate (CTR) by group & section, accounting for language",
          subtitle = "Excluding users whose primary preferred language isn't English; thick line represents overall CTR across days") +
  theme_bw(base_family = "Gill Sans", base_size = 14) + theme(legend.position = "bottom")
ggsave("ctr_by_group_link_nonenglish.png", p, path = "figures", width = 10, height = 12, dpi = 72)
