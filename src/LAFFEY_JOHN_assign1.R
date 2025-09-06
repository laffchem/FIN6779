library(dplyr)
library(haven)

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
  select(year, S003, trust, X047CS, inc, hinc, happiness, party_vote, 
         child_hardwork, child_thrift, starts_with("job_"), 
         change_attitude, competition, gender, age, marital_status, 
         education_level, employment_status)

View(wvs1)
summary(wvs1)



