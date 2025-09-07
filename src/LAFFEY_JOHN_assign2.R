library(dplyr)
library(haven)
library(tidyr)

# Read SAS data
wvs <- read_sas("/home/laff/school/FIN6779/lecture-1/data/wvs_dataset.sas7bdat")

# Keep labelled variables for display
wvs1 <- 
  wvs %>% 
  mutate(
    # Trust variable (A165)
    trust = if_else(A165 == 1, 1, 0), 
    trust = replace(trust, A165 < 1, NA), 
    
    # Income variables (X047CS)
    inc = X047CS, 
    inc = replace(inc, inc < 1, NA),
    inc = if_else(X047CS >= 840001 & X047CS <= 840010, X047CS - 840000, inc), 
    inc = if_else(X047CS >= 840011 & X047CS <= 840020, X047CS - 840010, inc),
    inc = if_else(X047CS >= 840041 & X047CS <= 840050, X047CS - 840040, inc), 
    inc = if_else(X047CS >= 840051 & X047CS <= 840060, X047CS - 840050, inc), 
    hinc = if_else(inc >= 8, 1, 0),
    
    # A008: Happiness
    happiness = if_else(A008 > 0, 5 - A008, NA_integer_),
    happiness = if_else(happiness >= 3, 1, if_else(happiness <=2, 0, NA_integer_)),
    
    # E179: Party vote
    party_vote = if_else(E179 > 0, E179, NA_integer_),
    party_vote = replace(party_vote, party_vote == 840001, "Republican"),
    party_vote = replace(party_vote, party_vote == 840002, "Democrat"),
    party_vote = replace(party_vote, party_vote == 840003, "Independent"),
    party_vote = replace(party_vote, party_vote == 840004, "Libertarian"),
    party_vote = replace(party_vote, party_vote == 840005, "Reform Party"),
    
    # A030: Important child quality: hard work
    child_hardwork = if_else(A030 >= 0, A030, NA_integer_),
    
    # A038: Important child quality: thrift
    child_thrift = if_else(A038 >= 0, A038, NA_integer_),
    
    # C011–C021: Job importance
    job_pay           = if_else(C011 >= 0, C011, NA_integer_),
    job_pressure      = if_else(C012 >= 0, C012, NA_integer_),
    job_security      = if_else(C013 >= 0, C013, NA_integer_),
    job_respect       = if_else(C014 >= 0, C014, NA_integer_),
    job_hours         = if_else(C015 >= 0, C015, NA_integer_),
    job_initiative    = if_else(C016 >= 0, C016, NA_integer_),
    job_holidays      = if_else(C017 >= 0, C017, NA_integer_),
    job_achievement   = if_else(C018 >= 0, C018, NA_integer_),
    job_responsibility= if_else(C019 >= 0, C019, NA_integer_),
    job_interesting   = if_else(C020 >= 0, C020, NA_integer_),
    job_abilities     = if_else(C021 >= 0, C021, NA_integer_),
    
    # E045: Attitude toward major changes
    change_attitude = if_else(E045 > 0, E045, NA_integer_),
    
    # E039: Competition good/harmful 
    competition = if_else(E039 > 0, E039, NA_integer_),
    
    # X001: Gender
    gender = if_else(X001 > 0, X001, NA_integer_),
    gender = replace(gender, gender == 1, 'm'),
    gender = replace(gender, gender == 2, 'f'),
    gender = replace(gender, gender <= 0, NA_integer_),
    
    # X003: Age
    age = if_else(X003 > 0 & X003 < 999, X003, NA_integer_),
    
    # X007: Marital status 
    marital_status = case_when(
      X007 %in% c(1, 2, 8)~1,
      X007 %in% c(3, 4, 5, 6, 7)~0,
      X007 < 1 ~ NA_integer_
    ),
    
    # X025: Education level
    education_level = if_else(X025 > 0, X025, NA_integer_),
    
    # X028: Employment status
    employment_status = if_else(X028 > 0, X028, NA_integer_),
    employment_status = replace(employment_status, employment_status == 1, 'Full Time'),
    employment_status = replace(employment_status, employment_status == 2, 'Part Time'),
    employment_status = replace(employment_status, employment_status == 3, 'Self Employed'),
    employment_status = replace(employment_status, employment_status == 4, 'Retired'),
    employment_status = replace(employment_status, employment_status == 5, 'Housewife'),
    employment_status = replace(employment_status, employment_status == 6, 'Students'),
    employment_status = replace(employment_status, employment_status == 7, 'Unemployed'),
    employment_status = replace(employment_status, employment_status == 8, 'Other'),
    employment_status = replace(employment_status, employment_status == -1, "Don't know"),
    employment_status = replace(employment_status, employment_status == -2, "No answer"),
    employment_status = replace(employment_status, employment_status == -3, "Not applicable"),
    employment_status = replace(employment_status, employment_status == -4, 'Not asked in survey'),
    employment_status = replace(employment_status, employment_status == -5, 'Unknown'),
    employment_status = replace(employment_status, employment_status < -5, NA_integer_),
  ) %>%
  # FIXED: Removed the problematic "Income Variables" column with spaces
  select(year, S003, trust, inc, hinc, happiness, party_vote, 
         child_hardwork, child_thrift, starts_with("job_"), 
         change_attitude, competition, gender, age, marital_status, 
         education_level, employment_status)

View(wvs1)
summary(wvs1)

numeric_vars <- wvs1 %>%
  select(-year, -S003, -gender, -party_vote, -employment_status) %>%
  select_if(is.numeric) %>%
  names()

# Calculate averages by gender
gender_averages <- wvs1 %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    across(all_of(numeric_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  )

# Create Table -- Gender Presentation
gender_table <- gender_averages %>%
  pivot_longer(cols = -c(gender, n), names_to = "Variable", values_to = "Average") %>%
  pivot_wider(names_from = gender, values_from = Average) %>%
  mutate(
    Variable = case_when(
      Variable == "trust" ~ "Trust",
      Variable == "inc" ~ "Income (Country-Specific Decile)",
      Variable == "hinc" ~ "High Income",
      Variable == "happiness" ~ "Happiness",
      Variable == "child_hardwork" ~ "Child: Hard Work",
      Variable == "child_thrift" ~ "Child: Thrift",
      Variable == "job_pay" ~ "Job: Pay",
      Variable == "job_pressure" ~ "Job: Pressure",
      Variable == "job_security" ~ "Job: Security",
      Variable == "job_respect" ~ "Job: Respect",
      Variable == "job_hours" ~ "Job: Hours",
      Variable == "job_initiative" ~ "Job: Initiative",
      Variable == "job_holidays" ~ "Job: Holidays",
      Variable == "job_achievement" ~ "Job: Achievement",
      Variable == "job_responsibility" ~ "Job: Responsibility",
      Variable == "job_interesting" ~ "Job: Interesting",
      Variable == "job_abilities" ~ "Job: Abilities",
      Variable == "change_attitude" ~ "Change Attitude",
      Variable == "competition" ~ "Competition",
      Variable == "age" ~ "Age",
      Variable == "marital_status" ~ "Marital Status",
      Variable == "education_level" ~ "Education Level",
      TRUE ~ Variable
    )
  ) %>%
  arrange(Variable)

View(gender_averages)
write.csv(gender_averages, "/home/laff/school/FIN6779/lecture-1/output/gender_averages.csv")

# Get predictor variables (exclude inc itself)
predictors <- setdiff(numeric_vars, "inc")

# Run simple regressions of inc ~ predictor
rsq_results <- lapply(predictors, function(var) {
  formula <- as.formula(paste("inc ~", var))
  model <- lm(formula, data = wvs1)
  tibble(
    Variable = var,
    R2 = summary(model)$r.squared
  )
}) %>%
  bind_rows() %>%
  arrange(desc(R2))

# Create a nicely formatted R² results table
rsq_table <- rsq_results %>%
  mutate(
    Variable_Name = case_when(
      Variable == "trust" ~ "Trust",
      Variable == "hinc" ~ "High Income",
      Variable == "happiness" ~ "Happiness",
      Variable == "child_hardwork" ~ "Child: Hard Work",
      Variable == "child_thrift" ~ "Child: Thrift",
      Variable == "job_pay" ~ "Job: Pay",
      Variable == "job_pressure" ~ "Job: Pressure",
      Variable == "job_security" ~ "Job: Security",
      Variable == "job_respect" ~ "Job: Respect",
      Variable == "job_hours" ~ "Job: Hours",
      Variable == "job_initiative" ~ "Job: Initiative",
      Variable == "job_holidays" ~ "Job: Holidays",
      Variable == "job_achievement" ~ "Job: Achievement",
      Variable == "job_responsibility" ~ "Job: Responsibility",
      Variable == "job_interesting" ~ "Job: Interesting",
      Variable == "job_abilities" ~ "Job: Abilities",
      Variable == "change_attitude" ~ "Change Attitude",
      Variable == "competition" ~ "Competition",
      Variable == "age" ~ "Age",
      Variable == "marital_status" ~ "Marital Status",
      Variable == "education_level" ~ "Education Level",
      TRUE ~ Variable
    ),
    R2_rounded = round(R2, 4),
    Rank = row_number()
  ) %>%
  select(Rank, Variable_Name, R2_rounded) %>%
  rename("R-squared" = R2_rounded)


# Save formatted R² results
write.csv(rsq_table, "/home/laff/school/FIN6779/lecture-1/output/rsq_results_formatted.csv", row.names = FALSE)

# Get top 3 predictors
top3 <- rsq_results %>%
  slice_max(R2, n = 3) %>%
  pull(Variable)

# print(paste("Top 3 predictors:", paste(top3, collapse = ", ")))

# Joint regression with top 3 predictors
joint_formula <- as.formula(
  paste("inc ~", paste(top3, collapse = " + "))
)

joint_model <- lm(joint_formula, data = wvs1)

# Create a summary table for the joint regression
joint_summary <- summary(joint_model)
joint_results <- data.frame(
  Model = "Joint Regression (Top 3)",
  R_squared = round(joint_summary$r.squared, 4),
  Adj_R_squared = round(joint_summary$adj.r.squared, 4),
  F_statistic = round(joint_summary$fstatistic[1], 2),
  p_value = format.pval(pf(joint_summary$fstatistic[1], 
                          joint_summary$fstatistic[2], 
                          joint_summary$fstatistic[3], 
                          lower.tail = FALSE), digits = 4)
)

# Save joint regression results
write.csv(joint_results, "/home/laff/school/FIN6779/lecture-1/output/joint_regression_summary.csv", row.names = FALSE)