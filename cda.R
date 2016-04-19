sessions <- readr::read_rds("data/portal-lang-detect-test.rds")

library(dplyr)
library(BCDA)

make_table <- function(data) {
  output <- as.matrix(data[, -1])
  colnames(output) <- names(data[, -1])
  rownames(output) <- as.character(data[[1]])
  return(output)
}
flip_cols <- function(x) { return(x[, rev(1:(dim(x)[2]))]) }
flip_rows <- function(x) { return(x[rev(1:(dim(x)[1])), ]) }

ctr_overall <- sessions %>%
  filter(date > "2016-03-22") %>%
  filter(preferred_languages != "en") %>%
  group_by(group, `Includes English`) %>%
  # group_by(group) %>%
  summarize(abandoned = sum(is.na(section)),
            total = length(clickthrough),
            search = sum(clickthrough & section == "search"),
            primary = sum(clickthrough & section == "primary links"),
            secondary = sum(clickthrough & section == "secondary links"))

set.seed(0)
ctr_overall %>%
  filter(`Includes English`) %>%
  mutate(nonprimary = total - primary) %>%
  select(group, nonprimary, primary) %>%
  make_table %>%
  flip_cols %>%
  flip_rows %T>%
  { print(prop.table(., margin = 1)) } %>%
  beta_binom %T>%
  { print(plot(., interval_type = "HPD")) } %>%
  summary(interval_type = "HPD") %>%
  knitr::kable(digits = 4)
  
set.seed(0)
ctr_overall %>%
  filter(!`Includes English`) %>%
  mutate(nonprimary = total - primary) %>%
  select(group, nonprimary, primary) %>%
  make_table %>%
  flip_cols %>%
  flip_rows %T>%
  { print(prop.table(., margin = 1)) } %>%
  beta_binom %T>%
  { print(plot(., interval_type = "HPD")) } %>%
  summary(interval_type = "HPD") %>%
  knitr::kable(digits = 4)

foo <- function(data) { # data <- sessions
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
  visit_aggregates_split <- language_clicks %>%
    mutate(includes_english = ifelse(`Includes English`,
                                     "Accept-Language includes English",
                                     "Accept-Language doesn't include English")) %>%
    group_by(group, section, includes_english) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`),
              `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>% ungroup %>% select(-Total)
  visit_aggregates_combined <- language_clicks %>%
    group_by(group, section) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`),
              `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>% ungroup %>% select(-Total) %>%
    mutate(includes_english = "Combined (Not split by Eng.)")
  visit_aggregates <- dplyr::bind_rows(visit_aggregates_split, visit_aggregates_combined)
  
  set.seed(0)
  cat("Primary Links, Accept-Language doesn't include English\n")
  p1 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Primary Links, Accept-Language doesn't include English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language doesn't include English\n")
  p2 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Primary Links, Accept-Language doesn't include English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language includes English\n")
  p3 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Primary Links, Accept-Language includes English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language includes English\n")
  p4 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Primary Links, Accept-Language includes English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Combined (Not split by Eng.)\n")
  p5 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Primary Links, Combined (Not split by Eng.)") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Combined (Not split by Eng.)\n")
  p6 <- visit_aggregates %>%
    filter(section == "primary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Primary Links, Combined (Not split by Eng.)") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  ## Secondary Links
  
  set.seed(0)
  cat("Secondary Links, Accept-Language doesn't include English\n")
  p7 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Secondary Links, Accept-Language doesn't include English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language doesn't include English\n")
  p8 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Secondary Links, Accept-Language doesn't include English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language includes English\n")
  p9 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Secondary Links, Accept-Language includes English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language includes English\n")
  p10 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Secondary Links, Accept-Language includes English") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Combined (Not split by Eng.)\n")
  p11 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
            subtitle = "Secondary Links, Combined (Not split by Eng.)") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Combined (Not split by Eng.)\n")
  p12 <- visit_aggregates %>%
    filter(section == "secondary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    beta_binom %T>%
    { cat(knitr::kable(summary(., interval_type = "HPD"), digits = 3), fill = TRUE) } %>%
    plot(interval_type = "HPD") +
    ggtitle("Probability of visiting Wikipedia in their most preferred language",
            subtitle = "Secondary Links, Combined (Not split by Eng.)") +
    theme_bw(base_family = "Gill Sans", base_size = 10) +
    geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  return(cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 4, nrow = 3))
  
}

(p <- foo(sessions))
