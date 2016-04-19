sessions <- readr::read_rds("data/portal-lang-detect-test.rds")

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
  filter(`Primary language` %in% top_langs & !is.na(section)) %>%
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
          subtitle = "Using only top 10 primarily preferred languages by # of users")
ggsave("lang_props.png", p, path = "figures", width = 10, height = 5, dpi = 300)
rm(top_langs, proportions_overall, proportions_clickthroughed, proportions_by_section, p)

ctr_daily <- sessions %>%
  filter(date > "2016-03-22") %>%
  group_by(date) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  mutate(group = "Combined")
ctr_overall <- sessions %>%
  filter(date > "2016-03-22") %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough)) %>%
  mutate(group = "Combined")
ctr_groups <- sessions %>%
  filter(date > "2016-03-22") %>%
  group_by(group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough))
p <- sessions %>%
  filter(date > "2016-03-22") %>%
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
          subtitle = "Combined overall CTR is 57.06%; controls' (A) overall CTR is 57.17%; test group's (B) overall CTR is 56.95%")
ggsave("daily_ctr.png", p, path = "figures", width = 10, height = 5, dpi = 300)
rm(p, ctr_daily, ctr_overall, ctr_groups)

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
ggsave("ctr_by_group_link_all.png", p, path = "figures", width = 8, height = 12, dpi = 300)
rm(p, ctr_overall)

# Let's check if daily primary/secondary link clickthrough was impacted, accounting for language
ctr_overall <- sessions %>%
  filter(date > "2016-03-22") %>%
  filter(preferred_languages != "en") %>%
  group_by(group, `Includes English`) %>%
  summarize(search = sum(clickthrough & section == "search")/length(clickthrough),
            primary = sum(clickthrough & section == "primary links")/length(clickthrough),
            secondary = sum(clickthrough & section == "secondary links")/length(clickthrough)) %>%
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
# foo <- function(x) {
#   2 * as.numeric(factor(x)) - 3
# }
p <- sessions %>%
  filter(date > "2016-03-22") %>%
  filter(preferred_languages != "en") %>%
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
          subtitle = "Excluding users whose only preferred language is English; thick line represents overall CTR across days") +
  theme_bw(base_family = "Gill Sans", base_size = 14) + theme(legend.position = "bottom")
ggsave("ctr_by_group_link_nonenglish.png", p, path = "figures", width = 10, height = 12, dpi = 300)
rm(p, ctr_overall)

# Basically, we want to see if people who would have used a secondary link to get to Wikipedia
#   in their primary language or one of their preferred langugages (if in the control group),
#   used a dynamic primary link instead.

foo <- function(data) {
  language_clicks <- data %>%
    filter(section %in% c("primary links", "secondary links")) %>%
    mutate(`Visited Wikipedia` = sub("https://(.*)\\.wikipedia\\.org/?", "\\1", destination),
           `Primary Lang` = vapply(preferred_languages, function(codes) {
             return(strsplit(codes, ",")[[1]][1])
           }, FUN.VALUE = "en"),
           `Visited Wikipedia in a preferred language` = mapply(function(wiki_prefix, preferred_langs) {
             return(grepl(wiki_prefix, preferred_langs, fixed = TRUE))
           }, wiki_prefix = `Visited Wikipedia`, preferred_langs = preferred_languages),
           `Visited Wikipedia in primary preferred language` = `Visited Wikipedia` == `Primary Lang`)
  visit_aggregates <- language_clicks %>%
    mutate(includes_english = ifelse(`Includes English`,
                                     "Accept-Language includes English",
                                     "Accept-Language doesn't include English")) %>%
    group_by(group, section, includes_english) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              # `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`)) %>%
              # `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>%
    group_by(includes_english, section) %>%
    mutate(`In one of their preferred languages` = `In one of their preferred languages`/Total,
           # `Not in any of their preferred languages` = `Not in any of their preferred languages`/Total,
           `In their most preferred language` = `In their most preferred language`/Total) %>%
           # `Not in their most preferred language` = `Not in their most preferred language`/Total) %>%
    select(-Total) %>%
    tidyr::gather(visit_type, prop_users, -(1:3))
  visit_aggregates_all <- language_clicks %>%
    group_by(group, section) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              # `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`)) %>%
    # `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>%
    group_by(section) %>%
    mutate(`In one of their preferred languages` = `In one of their preferred languages`/Total,
           # `Not in any of their preferred languages` = `Not in any of their preferred languages`/Total,
           `In their most preferred language` = `In their most preferred language`/Total) %>%
    # `Not in their most preferred language` = `Not in their most preferred language`/Total) %>%
    select(-Total) %>%
    tidyr::gather(visit_type, prop_users, -(1:2)) %>%
    mutate(includes_english = "Combined (Not split by Eng.)")
  # View(visit_aggregates)
  p <- visit_aggregates %>% rbind(visit_aggregates_all) %>%
    ggplot(aes(y = prop_users, x = visit_type, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(section~includes_english) +
    labs(x = "Type of Wikipedia visited", y = "Proportion of users") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = scales::percent_format(),
                       limits = c(0, 1.1)) +
    theme_bw(base_family = "Gill Sans", base_size = 16) +
    theme(legend.position = "bottom") +
    geom_text(aes(label = sprintf("%.1f%%", 100*prop_users), color = group),
              position = position_dodge(width = 1), fontface = "bold", hjust = -0.1) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    coord_flip()
  return(p)
}

p1 <- foo(sessions) +
  ggtitle("Which Wikipedia the users head to from Portal",
          subtitle = "All users")
ggsave("wikipedia_visits_all.png", p1, path = "figures", width = 15, height = 5, dpi = 300)
p2 <- foo(filter(sessions, `Primary language` != "English")) +
  ggtitle("Which Wikipedia the users head to from Portal",
          subtitle = "Excluding users whose primarily preferred language is English")
ggsave("wikipedia_visits_non-prime-En.png", p2, path = "figures", width = 15, height = 5, dpi = 300)
p3 <- foo(filter(sessions, preferred_languages != "en")) +
  ggtitle("Which Wikipedia the users head to from Portal",
          subtitle = "Excluding users whose only preferred language is English")
ggsave("wikipedia_visits_non-En.png", p3, path = "figures", width = 15, height = 5, dpi = 300)
p4 <- foo(filter(sessions, `Number of Accept-Languages` > 1)) +
  ggtitle("Which Wikipedia the users head to from Portal",
          subtitle = "Only multilingual users; subgroups with <10 users were excluded for quality-control")
ggsave("wikipedia_visits_multilang.png", p4, path = "figures", width = 15, height = 5, dpi = 300)
rm(p1, p2, p3, p4)
