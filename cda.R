sessions <- readr::read_rds("data/refined_sessions.rds")

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
  filter(preferred_languages != "en") %>%
  group_by(group, `Includes English`) %>%
  # group_by(group) %>%
  summarize(abandoned = sum(`total clicks` == 0),
            total = n(),
            primary = sum(`primary links clicks` > 0),
            secondary = sum(`secondary links clicks` > 0))

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
