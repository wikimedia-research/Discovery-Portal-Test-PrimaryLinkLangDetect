library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange)

events <- tbl_df(readr::read_rds("data/portal-lang-detect-test-v2.rds"))

events %<>%
  group_by(session_id) %>%
  arrange(ts) %>%
  mutate(visit = cumsum(type == "landing"))

# In some case we may have a clickthrough recorded before a landing event:
sum(events$visit == 0) # 187 events (0.24% of the dataset)
events %<>% keep_where(visit > 0)

first_visits <- events %>%
  keep_where(visit == 1) %>%
  group_by(session_id, visit) %>%
  summarize(date = head(date, 1), group = head(test_group, 1),
            clickthrough = "clickthrough" %in% type,
            clicked = tail(section_used, 1),
            destination = tail(destination, 1),
            preferred_languages = head(preferred_languages, 1),
            `Number of languages preceeding English` = head(`Number of languages preceeding English`, 1),
            `Primary language` = head(`Primary language`, 1),
            `Includes English` = head(`Includes English`, 1),
            `Number of Accept-Languages` = head(`Number of Accept-Languages`, 1),
            seconds_between = difftime(max(ts), min(ts), units = "secs")) %>%
  mutate(seconds_between = ifelse(is.na(clicked), as.numeric(NA), seconds_between))

readr::write_rds(first_visits, "data/refined_1st_visits.rds", "gz")

second_visits <- events %>%
  keep_where(visit == 2) %>%
  group_by(session_id, visit) %>%
  summarize(date = head(date, 1), group = head(test_group, 1),
            clickthrough = "clickthrough" %in% type,
            clicked = tail(section_used, 1),
            destination = tail(destination, 1),
            preferred_languages = head(preferred_languages, 1),
            `Number of languages preceeding English` = head(`Number of languages preceeding English`, 1),
            `Primary language` = head(`Primary language`, 1),
            `Includes English` = head(`Includes English`, 1),
            `Number of Accept-Languages` = head(`Number of Accept-Languages`, 1),
            seconds_between = difftime(max(ts), min(ts), units = "secs")) %>%
  mutate(seconds_between = ifelse(is.na(clicked), as.numeric(NA), seconds_between))

readr::write_rds(second_visits, "data/refined_2nd_visits.rds", "gz")

sessions <- events %>%
  group_by(session_id) %>%
  summarize(date = head(date, 1), group = head(test_group, 1),
            `page visits` = sum(type == "landing", na.rm = TRUE),
            `total clicks` = sum(type == "clickthrough", na.rm = TRUE),
            `primary links clicks` = sum(section_used == "primary links", na.rm = TRUE),
            `secondary links clicks` = sum(section_used == "secondary links", na.rm = TRUE),
            preferred_languages = head(preferred_languages, 1),
            `Number of languages preceeding English` = head(`Number of languages preceeding English`, 1),
            `Primary language` = head(`Primary language`, 1),
            `Includes English` = head(`Includes English`, 1),
            `Number of Accept-Languages` = head(`Number of Accept-Languages`, 1),
            `seconds between` = difftime(max(ts), min(ts), units = "secs")) %>%
  mutate(`seconds between` = ifelse(`page visits` < 2, as.numeric(NA), `seconds between`),
         `clickthrough rate` = `total clicks`/`page visits`,
         clickthrough = `total clicks` > 0)

readr::write_rds(sessions, "data/refined_sessions.rds", "gz")
