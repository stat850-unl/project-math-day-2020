library(tidyverse)
library(dplyr)
# this is a csv file containing the monthnames
monthnames <- read.csv("~/Downloads/months.csv", header = TRUE, na = c("NA NA NA"))
# this is a subset of the data (only 300 rows instead of >2000)
# Using read_csv and specific column types ensures types are standardized and can be pivoted

## NOTE: this is hard coded. need to fix 1416 to ncol(dbpartial)
# Specifying an NA character helps with typing as well
#db_partial <- read_csv("~/Downloads/partial_song_data.csv", col_types = ctypes, na = "-")
db_partial <- read_csv("~/Downloads/dbtest.csv",col_types = inittypes, na = c('-', 'NA'))
inittypes <- rep("cnnnnnnnnnnnn", times = ((1433-12)/13))

db_intro_rows <- read_csv("~/Downloads/dbtestn.csv",col_types = 'nnn', na = c('-', 'NA'))


db_partial <- cbind(db_intro_rows, db_partial)

db_partial_names <- db_partial %>%
  select("SongName.108", "SongID")

# Better to just use the .X names so that you have a more sure way to know how many times the variable name has been repeated
# colrepnames <- c("SongName", "TimeInHours",	"TimeInPlays",	"RankOutOfN",	"ValueOutOf1",
#                  "dHours", "dPlays",	"dRank",	"dValue",
#                  "TotaldTime",	"TotaldPlays",	"TotaldRank",	"TotaldValue")
# names(db_partial)[13:ncol(db_partial)] <- rep(colrepnames, times = ((ncol(db_partial)-12)/13))

diff_names <- nrow(monthnames) - ncol(db_partial)

monthnames <- monthnames %>% fill(dates) %>%
  mutate(variable = c(names(db_partial), rep("End_info", diff_names))) %>%
  # Use same strategy to separate variables into # times they're repeated
  separate(variable, into = c("var", "rep"), sep = "\\.") %>%
  # Replate NAs with 0 -- first time the variable has appeared
  mutate(rep = ifelse(is.na(rep), 0, as.numeric(rep))) %>%
  # This isn't strictly necessary but makes all blocks of variable/month combos consistent
  mutate(rep = ifelse(grepl("Total", var), rep + 1, rep))


db_partial1 <- db_partial[1:300,]

db_partial2 <- db_partial[301:600,]

db_partial3 <- db_partial[601:900,]

db_partial4 <- db_partial[901:ncol(db_partial),]


cleaned1 <- db_partial1 %>%
  # Get rid of name column, ugh.
  select(-matches("SongName")) %>%
  # Very long form
  pivot_longer(cols = -c(1:3), names_to = "variable", values_to = "value") %>%
  # Use same strategy to separate variables into # times they're repeated
  separate(variable, into = c("var", "rep"), sep = "\\.") %>%
  # Replate NAs with 0 -- first time the variable has appeared
  mutate(rep = ifelse(is.na(rep), 0, as.numeric(rep))) %>%
  # This isn't strictly necessary but makes all blocks of variable/month combos consistent
  mutate(rep = ifelse(grepl("Total", var), rep + 1, rep)) %>%
  # Merge
  left_join(select(monthnames, dates, var, rep)) %>%
  # Rep is now redundant information
  select(-rep) %>%
  # Pivot wider using cleaned variable names
  pivot_wider(id_cols = c(1:3, dates), names_from = "var", values_from = "value")


cleaned <- rbind(cleaned1, cleaned2, cleaned3, cleaned4)


cleaned <- left_join(cleaned, db_partial_names, by = "SongID")
colnames(cleaned)[colnames(cleaned) == 'SongName.108'] <- 'SongName'

write_csv(cleaned, '~/Downloads/cleaned.csv')



