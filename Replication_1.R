## Course Title: Research Methods
## Author: Seokbin Moon
## Replication of Saretto and Tookes (2013)

## Instruction
## 1. Apply some filters
## - Choose only S&P 500 firms (SNP500=1)
## - Sample period: From 2002 to 2010
## 2. Generate variables based on variable description
## - If LossCarryForward is missing, replace it with 0.
## 3. Generate summary statistics.
## 4. Compare your results with the original table. Why are they different?


library(tidyverse)
library(readxl)
library(modelsummary)

df <- read_excel("merged_data.xlsx")

df_filtered <- df %>%
  filter(SNP500 == 1) %>%
  filter(FYEAR >= 2002 & FYEAR <= 2010)

df_clean <- df_filtered %>%
  mutate(TLCF = replace_na(TLCF, 0)) %>%
  mutate(
    BookLev    = (DLTT + DLC) / AT,
    MktLev     = (DLTT + DLC) / (AT - CEQ + MKVALT + TXDB),
    MB         = (AT + MKVALT - (SEQ - PSTKL + TXDITC)) / AT,
    FixedAsset = PPENT / AT,
    Profit     = EBIT / AT,
    Size       = log(SALE), 
    TaxCredit  = TXDITC / AT,
    LCF        = TLCF / AT
  )

winsorize <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1-p), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

df_final <- df_clean %>%
  mutate(across(c(BookLev, MktLev, MB, FixedAsset,
                  Profit, Size, Volatility, AbEarn,
                  TaxCredit, LCF), winsorize))

summary_table <- df_final %>%
  select(BookLev, MktLev, MB, FixedAsset,
         Profit, Size, Volatility, AbEarn,
         TaxCredit, LCF, Rating, Rated, InvGrade) %>%
  summarise(across(everything(), list(
    Mean   = ~mean(.,   na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Stdev  = ~sd(.,     na.rm = TRUE),
    Min    = ~min(.,    na.rm = TRUE),
    Max    = ~max(.,    na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") %>%
  mutate(across(where(is.numeric), ~round(., 2)))

View(summary_table)