first_visits <- readr::read_rds("data/refined_1st_visits.rds")

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
  visit_aggregates_split <- language_clicks %>%
    mutate(includes_english = ifelse(`Includes English`,
                                     "Accept-Language includes English",
                                     "Accept-Language doesn't include English")) %>%
    group_by(group, clicked, includes_english) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`),
              `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>% ungroup # %>% select(-Total)
  visit_aggregates_combined <- language_clicks %>%
    group_by(group, clicked) %>%
    summarize(`Total` = n(),
              `In one of their preferred languages` = sum(`Visited Wikipedia in a preferred language`),
              `Not in any of their preferred languages` = n() - sum(`Visited Wikipedia in a preferred language`),
              `In their most preferred language` = sum(`Visited Wikipedia in primary preferred language`),
              `Not in their most preferred language` = n() - sum(`Visited Wikipedia in primary preferred language`)) %>%
    filter(Total > 10) %>% ungroup %>% # select(-Total) %>%
    mutate(includes_english = "Combined (Not split by Eng.)")
  visit_aggregates <- dplyr::bind_rows(visit_aggregates_split, visit_aggregates_combined)
  
  results <- as.list(character(12))
  
  format_confint <- function(est, ci, digits = 2, units = "") {
    if (units == "%") {
      units <- paste0(units, units)
    }
    type <- switch(typeof(est), "character" = "s", "double" = "f", "integer" = "i")
    x <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), est)
    y <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[1])
    z <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[2])
    return(paste0(x, " (", y, ", ", z, ")"))
  }
  
  set.seed(0)
  cat("Primary Links, Accept-Language doesn't include English\n")
  p1 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[1]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Primary Links, Accept-Language doesn't include English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language doesn't include English\n")
  p2 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[2]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Primary Links, Accept-Language doesn't include English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language includes English\n")
  p3 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[3]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Primary Links, Accept-Language includes English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Accept-Language includes English\n")
  p4 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[4]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Primary Links, Accept-Language includes English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Combined (Not split by Eng.)\n")
  p5 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[5]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Primary Links, Combined (Not split by Eng.)") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Primary Links, Combined (Not split by Eng.)\n")
  p6 <- visit_aggregates %>%
    filter(clicked == "primary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[6]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Primary Links, Combined (Not split by Eng.)") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  ## Secondary Links
  
  set.seed(0)
  cat("Secondary Links, Accept-Language doesn't include English\n")
  p7 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[7]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Secondary Links, Accept-Language doesn't include English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language doesn't include English\n")
  p8 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Accept-Language doesn't include English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[8]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Secondary Links, Accept-Language doesn't include English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language includes English\n")
  p9 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[9]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                            pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                  100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                            pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                  100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                            prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                       100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                            rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                      as.numeric(summary_tbl[4, 3:4])),
                            odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                        as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Secondary Links, Accept-Language includes English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Accept-Language includes English\n")
  p10 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Accept-Language includes English") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[10]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                             pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                   100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                             pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                   100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                             prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                        100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                             rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                       as.numeric(summary_tbl[4, 3:4])),
                             odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                         as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Secondary Links, Accept-Language includes English") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Combined (Not split by Eng.)\n")
  p11 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In one of their preferred languages`, `Not in any of their preferred languages`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[11]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                             pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                   100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                             pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                   100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                             prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                        100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                             rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                       as.numeric(summary_tbl[4, 3:4])),
                             odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                         as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in one of their preferred languages",
  #         subtitle = "Secondary Links, Combined (Not split by Eng.)") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  set.seed(0)
  cat("Secondary Links, Combined (Not split by Eng.)\n")
  p12 <- visit_aggregates %>%
    filter(clicked == "secondary links") %>%
    filter(includes_english == "Combined (Not split by Eng.)") %>%
    select(group, `In their most preferred language`, `Not in their most preferred language`) %>%
    make_table %>%
    flip_rows %T>%
    { cat(knitr::kable(prop.table(., margin = 1), digits = 3), fill = TRUE) } %>%
    {
      summary_tbl <- summary(beta_binom(.), interval_type = "HPD")
      margins <- as.numeric(margin.table(., 1))
      results[[12]] <<- list(n_B = polloi::compress(margins[1], 1), n_A = polloi::compress(margins[2], 1),
                             pi_B = format_confint(100 * as.numeric(summary_tbl[1, 1]),
                                                   100 * as.numeric(summary_tbl[1, 3:4]), digits = 1),
                             pi_A = format_confint(100 * as.numeric(summary_tbl[2, 1]),
                                                   100 * as.numeric(summary_tbl[2, 3:4]), digits = 1),
                             prop_diff = format_confint(100 * as.numeric(summary_tbl[3, 1]),
                                                        100 * as.numeric(summary_tbl[3, 3:4]), digits = 1),
                             rel_risk = format_confint(as.numeric(summary_tbl[4, 1]),
                                                       as.numeric(summary_tbl[4, 3:4])),
                             odds_ratio = format_confint(as.numeric(summary_tbl[5, 1]),
                                                         as.numeric(summary_tbl[5, 3:4])))
      cat(knitr::kable(summary_tbl, digits = 3), fill = TRUE)
    } # %>%
  # plot(interval_type = "HPD") +
  # ggtitle("Probability of visiting Wikipedia in their most preferred language",
  #         subtitle = "Secondary Links, Combined (Not split by Eng.)") +
  # theme_bw(base_family = "Gill Sans", base_size = 10) +
  # geom_vline(xintercept = c(0, 1), linetype = "dashed")
  
  return(list(stats = do.call(rbind, results),
              plots = cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12,
                                         ncol = 4, nrow = 3)))
  
}

# p <- foo(first_visits)
# p <- foo(filter(first_visits, `Primary language` != "English"))
# p <- foo(filter(first_visits, preferred_languages != "en"))
# p <- foo(filter(first_visits, `Number of Accept-Languages` > 1)) # errors
# ^ use data=filter(first_visits, `Number of Accept-Languages` > 1) and go through foo() manually
# doing only clickeds where A-L INCLUDES English or that are combined (not split by English)
# then: p <- list(stats = do.call(rbind, results))

data.frame(clicked = c(rep("Prim", 6),
                       rep("Sec", 6)),
           includes_eng = rep(c(rep("A-L doesn't include En", 2),
                                rep("A-L includes En", 2),
                                rep("Combined (Not split by Eng.", 2)), 2),
           visited_wikipedia = c("one of preferred",
                                 "most preferred")) %>%
  cbind(p$stats) %>%
  set_names(c("Link", "Includes English", "Visited Wikipedia",
              "N (B)", "N (A)", "PrB (%)", "PrA (%)",
              "% Diff (B vs A)", "Relative Risk", "Odds Ratio")) %>%
  knitr::kable(format = "latex", caption = "...caption...")

# print(p$plots)
