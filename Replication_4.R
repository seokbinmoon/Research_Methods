## Course Title: Research Methods
## Author: Seokbin Moon
## Replication of Saretto and Tookes (2013)

## Instruction
## 1. Import dataset
## 2. Run propensity score using the following variables:
## - List of X variables: BookLeverage, MarketToBook, FixedAsset, Profitability,
##                        Size, Volatility, AbEarn, InvestmentTaxCredit, LossCarryForward, Rating, Rated, InvGrade
## 3. Evaluate the effectiveness of propensity score matching
## − Present summary statistics for the subsamples: CDS=1 and CDS=0.
## − Assess the balance between the two groups.

library(readxl)
library(tidyverse)
library(MatchIt)

df <- read_excel("Data_Lecture 9.xlsx")

vars <- c("BookLeverage", "MarketToBook", "FixedAsset", "Profitability",
          "Size", "Volatility", "AbEarn", "InvestmentTaxCredit",
          "LossCarryForward", "Rating", "Rated", "InvGrade")

data <- df %>%
  select(CDS, all_of(vars)) %>%
  na.omit() %>%
  mutate(CDS = as.integer(CDS))

# psm = propensity score matching
psm_formula <- as.formula(paste("CDS ~", paste(vars, collapse = " + ")))

set.seed(1227)
match_out <- matchit(psm_formula, data = data, method = "nearest", ratio = 1)


sum_stats <- function(sub, label) {
  sub %>%
    select(all_of(vars)) %>%
    summarise(across(everything(),
                     list(Mean = ~mean(., na.rm = TRUE),
                          p50  = ~median(., na.rm = TRUE),
                          SD   = ~sd(., na.rm = TRUE),
                          Min  = ~min(., na.rm = TRUE),
                          Max  = ~max(., na.rm = TRUE)),
                     .names = "{.col}__{.fn}")) %>%
    tidyr::pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "__") %>%
    tidyr::pivot_wider(names_from = Stat, values_from = value) %>%
    print(digits = 3)
}


cat("BEFORE MATCHING")
sum_stats(data %>% filter(CDS == 1), sprintf("CDS = 1", sum(data$CDS == 1)))
sum_stats(data %>% filter(CDS == 0), sprintf("CDS = 0", sum(data$CDS == 0)))


matched <- match.data(match_out)
cat("AFTER MATCHING")
sum_stats(matched %>% filter(CDS == 1), sprintf("CDS = 1", sum(matched$CDS == 1)))
sum_stats(matched %>% filter(CDS == 0), sprintf("CDS = 0", sum(matched$CDS == 0)))