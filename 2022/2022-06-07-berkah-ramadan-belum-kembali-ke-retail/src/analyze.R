dirYear <- "2022"
dirProject <- "2022-06-07-berkah-ramadan-belum-kembali-ke-retail"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(tidyxl)
library(unpivotr)


# Data ----

rsiRaw <- xlsx_cells(
  here(dirYear, dirProject, "data", "bi-rsi-2022-05-12.xlsx"),
  sheets = "Tabel 1"
)

rsiLong <- rsiRaw |> 
  filter(!is_blank, row > 3) |> 
  behead("left", "category_idn") |> 
  behead("right", "category_eng") |>
  behead("up", "year") |>
  filter(!is.na(category_idn)) |> 
  select(category_idn, category_eng, year, rsi = numeric)

# Create a `date` column manually using the `year` column and row numbers
# within category and year. Then clean the category labels
rsiClean <- rsiLong |> 
  fill(year) |> 
  group_by(category_idn, year) |> 
  mutate(
    row_num = row_number(year),
    date = case_when(
      row_num < 10 ~ paste0(year, "-0", row_num, "-01"),
      TRUE ~ paste(year, row_num, "01", sep = "-")
    ),
    date = as.Date(date)
  ) |> 
  ungroup() |> 
  mutate(
    across(
      .cols = contains(match = "category"), 
      .fns = ~ str_remove_all(.x, "-\\so/w")
    ),
    across(
      .cols = contains("category"),
      .fns = ~ str_remove_all(.x, "INDE(KS|X)")
    ),
    across(
      .cols = contains("category"), 
      .fns = ~ str_replace_all(.x, "\\s(dan|and)\\s", " & ")
    ),
    across(.cols = contains("category"), .fns = str_squish),
    across(.cols = contains("category"), .fns = str_to_sentence),
    category_idn = str_replace(category_idn, "Total", "Keseluruhan"),
    category_eng = str_replace(category_eng, "Total", "Overall")
  ) |> 
  select(category_idn, category_eng, year, date, rsi)

rsiZscore <- rsiClean |> 
  group_by(category_idn) |> 
  mutate(
    rsi_mean = mean(rsi, na.rm = TRUE),
    rsi_sd = sd(rsi)
  ) |> 
  ungroup() |> 
  mutate(z_score = (rsi - rsi_mean) / rsi_sd) |> 
  filter(date == as.Date("2022-04-01")) |> 
  select(-year)

write_csv(rsiZscore, here(dirYear, dirProject, "result", "rsi-z-score.csv"))