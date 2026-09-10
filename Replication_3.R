## Course Title: Research Methods
## Author: Seokbin Moon
## Replication of Saretto and Tookes (2013)

## Instruction
## 1. Import dataset
## - See variable description
## 2. Create treatment and post dummies (DiD)
## − CDStraded: 1 if the firm has traded CDS during the sample period, and 0 otherwise.
## − CDStrading: 1 after the introduction of CDS, 0 otherwise.
## − “CDSintroyear”: the year of CDS introduction
## 3. Run regressions (Table 3, column 1, 2, 5, 6)
## − Include variables in the regressions.
## − Include time, industry (column 1, 5), and firm (column 2, 6) fixed effects.
## − Use robust standard errors. 

library(readxl)
library(tidyverse)
library(fixest)

df_raw <- read_excel("Data_ST.xlsx")
summary(df_raw)

df <- df_raw %>%
  mutate(
    CDStraded  = ifelse(!is.na(CDSintroyear), 1, 0),
    CDStrading = ifelse(!is.na(CDSintroyear) & FYEAR >= CDSintroyear, 1, 0)
  )

vars <- c("DebtMat","ind_BLev","ind_MLev","MB","FixAsset","Profit","Size",
          "Volatility","AbEarn","TaxCredit","LCF","Rated","InvGrade","CPprogram",
          "CDStraded","CDStrading", "BLev", "MLev", "GVKEY","FYEAR","SICH3","SnP500")

reg_df <- df 


# fixest::feols syntax
# feols(y ~ x1 + x2 | fe1 + fe2, cluster = ~id, data = df)

m1 <- feols(BLev ~ DebtMat + ind_BLev + MB + FixAsset + Profit + Size +
              Volatility + AbEarn + TaxCredit + LCF + Rated + InvGrade + CPprogram +
              CDStraded + CDStrading | FYEAR + SICH3, cluster = ~GVKEY, data = reg_df)

m2 <- feols(BLev ~ DebtMat + ind_BLev + MB + FixAsset + Profit + Size +
              Volatility + AbEarn + TaxCredit + LCF + Rated + InvGrade + CPprogram +
              CDStraded + CDStrading | FYEAR + GVKEY, cluster = ~GVKEY, data = reg_df)

m5 <- feols(MLev ~ DebtMat + ind_MLev + MB + FixAsset + Profit + Size +
              Volatility + AbEarn + TaxCredit + LCF + Rated + InvGrade + CPprogram +
              CDStraded + CDStrading | FYEAR + SICH3, cluster = ~GVKEY, data = reg_df)

m6 <- feols(MLev ~ DebtMat + ind_MLev + MB + FixAsset + Profit + Size +
              Volatility + AbEarn + TaxCredit + LCF + Rated + InvGrade + CPprogram +
              CDStraded + CDStrading | FYEAR + GVKEY, cluster = ~GVKEY, data = reg_df)

etable(m1, m2, m5, m6)