first_visits <- readr::read_rds("data/refined_1st_visits.rds")

library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2) # devtools::install_github("hadley/ggplot2")
# library(mgcv) # for geom_smooth(method = "gam")

# Basically, we want to see if people who would have used a secondary link to get to Wikipedia
#   in their primary language or one of their preferred langugages (if in the control group),
#   used a dynamic primary link instead.

foo <- function(data) { # data <- first_visits
  language_clicks <- data %>%
    filter(clicked %in% c("primary links", "secondary links")) %>%
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
    group_by(group, clicked, includes_english) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              # `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`)) %>%
    # `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>%
    group_by(includes_english, clicked) %>%
    mutate(`In one of their preferred languages` = `In one of their preferred languages`/Total,
           # `Not in any of their preferred languages` = `Not in any of their preferred languages`/Total,
           `In their most preferred language` = `In their most preferred language`/Total) %>%
    # `Not in their most preferred language` = `Not in their most preferred language`/Total) %>%
    ungroup %>%
    # select(-Total) %>%
    tidyr::gather(visit_type, prop_users, -(1:4))
  visit_aggregates_all <- language_clicks %>%
    group_by(group, clicked) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              # `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`)) %>%
    # `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>%
    group_by(clicked) %>%
    mutate(`In one of their preferred languages` = `In one of their preferred languages`/Total,
           # `Not in any of their preferred languages` = `Not in any of their preferred languages`/Total,
           `In their most preferred language` = `In their most preferred language`/Total) %>%
    # `Not in their most preferred language` = `Not in their most preferred language`/Total) %>%
    ungroup %>%
    # select(-Total) %>%
    tidyr::gather(visit_type, prop_users, -(1:3)) %>%
    mutate(includes_english = "Combined (Not split by Eng.)")
  # View(visit_aggregates)
  p <- visit_aggregates %>% rbind(visit_aggregates_all) %>%
    mutate(visit_type = ordered(visit_type, levels = rev(sort(unique(visit_type))))) %>%
    ggplot(aes(y = prop_users, x = visit_type, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(clicked~includes_english) +
    labs(x = "Type of Wikipedia visited", y = "Proportion of users") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = scales::percent_format(),
                       limits = c(0, 1.25)) +
    theme_bw(base_family = "Gill Sans", base_size = 16) +
    theme(legend.position = "bottom") +
    geom_text(aes(label = sprintf("%.1f%% (%s)", 100*prop_users, polloi::compress(Total)), color = group),
              position = position_dodge(width = 1), fontface = "bold", hjust = -0.1) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    coord_flip()
  return(p)
}

p1 <- foo(first_visits) +
  ggtitle("Which Wikipedia the users head to from Portal on their first visit",
          subtitle = "All users")
ggsave("wikipedia_visits_all.png", p1, path = "figures", width = 15, height = 5, dpi = 300)
p2 <- foo(filter(first_visits, `Primary language` != "English")) +
  ggtitle("Which Wikipedia the users head to from Portal on their first visit",
          subtitle = "Excluding users whose most preferred language is English")
ggsave("wikipedia_visits_non-prime-En.png", p2, path = "figures", width = 15, height = 5, dpi = 300)
p3 <- foo(filter(first_visits, preferred_languages != "en")) +
  ggtitle("Which Wikipedia the users head to from Portal on their first visit",
          subtitle = "Excluding users whose only preferred language is English")
ggsave("wikipedia_visits_non-En.png", p3, path = "figures", width = 15, height = 5, dpi = 300)
p4 <- foo(filter(first_visits, `Number of Accept-Languages` > 1)) +
  ggtitle("Which Wikipedia the users head to from Portal on their first visit",
          subtitle = "Only multilingual users; subgroups with <10 users were excluded for quality-control")
ggsave("wikipedia_visits_multilang.png", p4, path = "figures", width = 15, height = 5, dpi = 300)
rm(p1, p2, p3, p4)
