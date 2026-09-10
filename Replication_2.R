## Course Title: Research Methods
## Author: Seokbin Moon
## Replication of Rauh and Sufi (2010)

## Instruction
## 1. Import dataset
## - See variable description
## 2. Apply some filters
## - Total capital has non-missing value
## 3. Generate summary statistics.
## - Table 1, Panel B
## 4. Run regressions
## - Table 3, Panel A
## - Must include year-fixed effect 

library(readxl)
library(lmtest)
library(sandwich)

df <- read_excel("Data_RS.xlsx", sheet = "RS_data")
df <- df[!is.na(df$a2), ]


# Table 1, Panel B 
vars <- c("a", "a2", "oiadp_a2", "ppent_a2", "debt_mv", "d_a2", "mb")
stats <- t(sapply(df[vars], function(x)
  c(Mean = mean(x, na.rm=TRUE), SD = sd(x, na.rm=TRUE), Median = median(x, na.rm=TRUE))
))
rownames(stats) <- c("Book Assets", "Total Capital", "Profitability",
                     "Tangibility", "Debt/Market Value", "Debt/Total Capital", "Market/Book")
print(round(stats, 3))


# Table 3, Panel A 
dep_vars   <- c("d_a2", "bankout_a2", "prog_a2", "bonds_a2", "pp_a2", "cv_a2", "mgeqoth_a2")
dep_labels <- c("Total Debt", "Bank", "Program", "Bonds", "PPs", "Convertibles", "All Other")
key_vars   <- c("oiadp_a2", "ppent_a2", "mb", "lnsale", "(Intercept)")
key_labels <- c("Profitability", "Tangibility", "M/B", "ln(Sales)", "Constant")

year_fe <- paste0("`_Ifyear_", 1997:2006, "`", collapse = " + ")
rhs     <- paste("oiadp_a2 + ppent_a2 + mb + lnsale +", year_fe)
df_reg  <- df[complete.cases(df[c("oiadp_a2","ppent_a2","mb","lnsale")]), ]

models_lm <- setNames(lapply(dep_vars, function(dv)
  lm(as.formula(paste(dv, "~", rhs)), data = df_reg)), dep_vars)
models_ct <- setNames(lapply(dep_vars, function(dv)
  coeftest(models_lm[[dv]], vcov = vcovCL(models_lm[[dv]], cluster = ~gvkey))), dep_vars)

# Replication of Rauh and Sufi (2010): Table 3 Panel A
tab <- NULL
rn  <- character(0)
for (ki in seq_along(key_vars)) {
  kv    <- key_vars[ki]
  est   <- sapply(dep_vars, function(dv) models_ct[[dv]][kv, "Estimate"])
  se    <- sapply(dep_vars, function(dv) models_ct[[dv]][kv, "Std. Error"])
  pv    <- sapply(dep_vars, function(dv) models_ct[[dv]][kv, "Pr(>|t|)"])
  stars <- ifelse(pv < 0.01, "***", ifelse(pv < 0.05, "**", ifelse(pv < 0.10, "*", "")))
  tab   <- rbind(tab, paste0(round(est, 3), stars), paste0("(", round(se, 3), ")"))
  rn    <- c(rn, key_labels[ki], "")
}
tab <- rbind(tab, sapply(models_lm, function(m) round(summary(m)$adj.r.squared, 2)))
rn  <- c(rn, "Adj. R-Squared")
rownames(tab) <- rn
colnames(tab) <- dep_labels

oldw <- getOption("width")
options(width = 200)
print(tab, quote = FALSE)
options(width = oldw)