## 0. CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls())

## 1. PACKAGES & DATA ---------------------------------------------------------
library(NHANES)
library(dplyr)
library(naniar)
library(mice)
library(ggplot2)
library(Metrics)
library(broom)

data("NHANES")
df_raw <- NHANES

## 2. CHOOSE PREDICTOR VARIABLES ---------------------------------------------
vars_age_predictors <- c(
  "Education", "MaritalStatus",
  "HHIncome", "HHIncomeMid", "Poverty",
  "HomeRooms", "HomeOwn", "Work",
  "Weight", "Height", "BMI", "Pulse",
  "BPSysAve", "BPDiaAve",
  "DirectChol", "TotChol",
  "Diabetes",
  "HealthGen", "DaysPhysHlthBad", "DaysMentHlthBad",
  "LittleInterest", "Depressed",
  "PhysActive"
)

## 3. BASE DATA: AGE + PREDICTORS --------------------------------------------
df_base <- df_raw %>%
  select(Age, all_of(vars_age_predictors))

## 4. ATTACH LENGTH & FIX MISSING HEIGHT (CM) --------------------------------
df_prep <- df_base %>%
  mutate(Length = df_raw$Length) %>%
  mutate(
    Height = case_when(
      !is.na(Height) ~ Height,                                            # keep existing
      is.na(Height) & !is.na(Length) & Age < 2 ~ Length,                  # infants
      is.na(Height) & !is.na(Length) & Age >= 2 & Age <= 3 ~ Length - 0.7,# toddlers
      is.na(Height) & !is.na(Length) ~ Length,                            # catch-all
      TRUE ~ Height
    )
  )

## 5. STRUCTURAL RULES (ONLY WHERE CURRENT VALUE IS NA) ----------------------
df_prep <- df_prep %>%
  mutate(
    # Education for Age < 20
    Education = case_when(
      is.na(Education) & Age < 5 ~ "NoSchool",
      is.na(Education) & Age >= 5 & Age < 12 ~ "Elementary",
      is.na(Education) & Age >= 12 & Age < 20 ~ "HighSchool",
      TRUE ~ Education
    ),
    
    # Marital status for Age < 18 (others patched later)
    MaritalStatus = case_when(
      is.na(MaritalStatus) & Age < 18 ~ "NeverMarried",
      TRUE ~ MaritalStatus
    ),
    
    # Work for Age < 16
    Work = case_when(
      is.na(Work) & Age < 16 ~ "NeverWorked",
      TRUE ~ Work
    ),
    
    # Depression items = 0 for Age < 18 (if NA)
    LittleInterest = ifelse(is.na(LittleInterest) & Age < 18, 0, LittleInterest),
    Depressed      = ifelse(is.na(Depressed)      & Age < 18, 0, Depressed),
    
    # HealthGen = Excellent for Age < 12 (if NA)
    HealthGen = case_when(
      is.na(HealthGen) & Age < 12 ~ "Excellent",
      TRUE ~ HealthGen
    ),
    
    # Days phys/ment health bad = 0 for Age < 12 (if NA)
    DaysPhysHlthBad = ifelse(is.na(DaysPhysHlthBad) & Age < 12, 0, DaysPhysHlthBad),
    DaysMentHlthBad = ifelse(is.na(DaysMentHlthBad) & Age < 12, 0, DaysMentHlthBad),
    
    # Kids < 18 assumed physically active if NA
    PhysActive = case_when(
      is.na(PhysActive) & Age < 18 ~ "Yes",
      TRUE ~ PhysActive
    )
  )

## 6. DROP LENGTH ------------------------------------------------------------
df_prep <- df_prep %>%
  select(-Length)

## 7. KEEP ONLY PARTICIPANTS WITH BP ELIGIBILITY (AGE 8–79) ------------------
df_prep <- df_prep %>%
  filter(Age >= 8) %>%
  filter(Age <= 79)

## 8. FINAL MARITALSTATUS PATCH ----------------------------------------------
df_prep <- df_prep %>%
  mutate(
    MaritalStatus = ifelse(is.na(MaritalStatus), "NeverMarried", MaritalStatus)
  )

## 9. FIX DUPLICATED EDUCATION LEVEL LABEL -----------------------------------
df_prep$Education[df_prep$Education == "HighSchool"] <- "High School"
df_prep$Education <- factor(df_prep$Education)

## (Optional) CHECK MISSINGNESS ----------------------------------------------
df_vis <- df_prep %>% arrange(Age)
gg_miss_var(df_vis, show_pct = TRUE)

## 10. PREP DATA TYPES FOR MICE ----------------------------------------------
mice_data <- df_prep %>%
  mutate(
    Education    = factor(Education),
    MaritalStatus= factor(MaritalStatus),
    HomeOwn      = factor(HomeOwn),
    Work         = factor(Work),
    Diabetes     = factor(Diabetes),
    HealthGen    = factor(HealthGen),
    PhysActive   = factor(PhysActive),
    # PHQ-2 items numeric
    LittleInterest = as.numeric(LittleInterest),
    Depressed      = as.numeric(Depressed)
  )

## 11. SET UP MICE (DO NOT IMPUTE AGE) ---------------------------------------
meth <- make.method(mice_data)
meth["Age"] <- ""   # Age not imputed

imp <- mice(
  mice_data,
  m      = 5,
  maxit  = 10,
  method = meth,
  seed   = 123
)

## 12. FULL MODEL (FOR COMPARISON) -------------------------------------------
fit_full <- with(
  imp,
  lm(
    Age ~ Education + MaritalStatus + HHIncomeMid + Poverty +
      HomeRooms + HomeOwn + Work + Weight + Height + BMI +
      Pulse + BPSysAve + BPDiaAve + DirectChol + TotChol +
      Diabetes + HealthGen + DaysPhysHlthBad + DaysMentHlthBad +
      LittleInterest + Depressed + PhysActive
  )
)

## 13. REDUCED MODEL ---------------------------------------------------------
fit_reduced <- with(
  imp,
  lm(
    Age ~ Education + MaritalStatus + HHIncomeMid + Poverty +
      HomeOwn + Work +
      Pulse + BPSysAve + BPDiaAve +
      DirectChol + TotChol + Diabetes +
      DaysPhysHlthBad + DaysMentHlthBad +
      LittleInterest + Depressed + PhysActive
  )
)

## 14. R-SQUARED (POOLED) ----------------------------------------------------
R2_full    <- pool.r.squared(fit_full)
R2_reduced <- pool.r.squared(fit_reduced)

R2_full_est    <- R2_full[,"est"]
R2_reduced_est <- R2_reduced[,"est"]

## 15. RMSE & AIC ACROSS COMPLETED DATASETS ----------------------------------
rmse_full_list <- c()
rmse_red_list  <- c()
AIC_full_list  <- c()
AIC_red_list   <- c()

for (i in 1:5) {
  comp_i <- complete(imp, i)
  
  full_i <- lm(
    Age ~ Education + MaritalStatus + HHIncomeMid + Poverty +
      HomeRooms + HomeOwn + Work + Weight + Height + BMI +
      Pulse + BPSysAve + BPDiaAve + DirectChol + TotChol +
      Diabetes + HealthGen + DaysPhysHlthBad + DaysMentHlthBad +
      LittleInterest + Depressed + PhysActive,
    data = comp_i
  )
  
  red_i <- lm(
    Age ~ Education + MaritalStatus + HHIncomeMid + Poverty +
      HomeOwn + Work +
      Pulse + BPSysAve + BPDiaAve +
      DirectChol + TotChol + Diabetes +
      DaysPhysHlthBad + DaysMentHlthBad +
      LittleInterest + Depressed + PhysActive,
    data = comp_i
  )
  
  rmse_full_list[i] <- rmse(comp_i$Age, predict(full_i))
  rmse_red_list[i]  <- rmse(comp_i$Age, predict(red_i))
  AIC_full_list[i]  <- AIC(full_i)
  AIC_red_list[i]   <- AIC(red_i)
}

RMSE_full <- mean(rmse_full_list)
RMSE_reduced <- mean(rmse_red_list)
AIC_full <- mean(AIC_full_list)
AIC_reduced <- mean(AIC_red_list)

comparison_table <- data.frame(
  Model = c("Full Model", "Reduced Model"),
  R2    = c(R2_full_est, R2_reduced_est),
  AIC   = c(AIC_full, AIC_reduced),
  RMSE  = c(RMSE_full, RMSE_reduced)
)

comparison_table_round <- comparison_table %>%
  mutate(
    R2   = round(R2, 3),
    AIC  = round(AIC, 1),
    RMSE = round(RMSE, 2)
  )

print(comparison_table_round)

## 16. TIDY TABLE OF REDUCED MODEL COEFFICIENTS ------------------------------
reduced_pooled <- pool(fit_reduced)
tidy_reduced <- summary(reduced_pooled)

tidy_reduced_table <- tidy_reduced %>%
  mutate(
    estimate   = round(estimate, 3),
    std.error  = round(std.error, 3),
    statistic  = round(statistic, 2),
    p.value    = signif(p.value, 3)
  )

print(tidy_reduced_table)

## 17. FINAL REDUCED MODEL FIT & PREDICTION PLOT -----------------------------
comp1 <- complete(imp, 1)

fit_red_1 <- lm(
  Age ~ Education + MaritalStatus + HHIncomeMid + Poverty +
    HomeOwn + Work +
    Pulse + BPSysAve + BPDiaAve +
    DirectChol + TotChol + Diabetes +
    DaysPhysHlthBad + DaysMentHlthBad +
    LittleInterest + Depressed + PhysActive,
  data = comp1
)

comp1$PredictedAge <- predict(fit_red_1)

ggplot(comp1, aes(x = Age, y = PredictedAge)) +
  geom_point(alpha = 0.20) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", linewidth = 1) +
  labs(
    title = "Predicted vs Actual Age",
    x = "Actual Age",
    y = "Predicted Age"
  ) +
  theme_minimal(base_size = 14)

## 18. OPTIONAL: SAVE OBJECTS SO YOU DON'T RERUN MICE ------------------------
saveRDS(df_prep,             "df_prep_clean.rds")
saveRDS(imp,                 "nhanes_imp_age_5m.rds")
saveRDS(fit_reduced,         "fit_reduced_mids.rds")
saveRDS(comparison_table_round, "comparison_table_round.rds")
saveRDS(tidy_reduced_table,  "tidy_reduced_table.rds")

# In a fresh R session later you can do:
# df_prep <- readRDS("df_prep_clean.rds")
# imp     <- readRDS("nhanes_imp_age_5m.rds")
# tidy_reduced_table <- readRDS("tidy_reduced_table.rds")
# etc., and skip the long imputation.

