


library(tidycensus)
library(dplyr)
library(stringr)


# Find sample size of an inquiry

# Example: pull 2023 ACS 5-year PUMS data for surgeons

# # occupations you want to search for by label
# search_terms <- c("MED-Surgeons", "MED-Physicians")
#
# # 1. Look up OCCP codes dynamically from val_label
# occ_code_mans <- pums_variables %>%
#   filter(year == 2023,
#          survey == "acs5",
#          str_detect(val_label, regex(paste(search_terms, collapse = "|"), ignore_case = TRUE))) %>%
#   distinct(val_label, occ_code_man = val_min)



#####  ALL ONE SCRIPT  #####

# ====================================================
# Step-by-step filtering of PUMS data
# Mirrors the filter_pums_data() function
# ====================================================
# --- PARAMETERS (set these before sourcing) ---
sex_code_man_female   <- 2        # 1 = male, 2 = female (example)
sex_code_man_male    <- 1     # 1 = male, 2 = female (example)
mar_code_man   <- 5        # e.g. 1 = married, or NULL for no filter
edu_code_man   <- 23       # e.g. c(16, 17) or NULL
edu_label_man  <- NULL     # e.g. "bachelor" or NULL
min_edu_man    <- NULL     # e.g. high school = 16, set NULL for no filter
age_min_man    <- 40
age_max_man    <- 64
occup_code_man <- 3090     # MED-Physicians




# OCCUPATION_GAP_ANALYSIS <- c("MED-Physicians" = 3090,
# "MED-Surgeons",
# "MED-Dentists",
# "MED-Pharmacists",
# "LGL-Lawyers, And Judges, Magistrates, And Other Judicial Workers",
# "EDU-Postsecondary Teachers",
# "MGR-General And Operations Managers",
# "EDU-Secondary School Teachers",
# "MED-Registered Nurses",
# "ENG-Engineer",
# "FIN-Accountants And Auditors",
# "MGR-Sales Managers")


# Common codes:
#   - SEX: "1" = Male, "2" = Female
# - MAR: "1" = Married, "2" = Widowed, "3" = Divorced, "4" = Separated, "5" = Never married
# - SCHL: Common education levels:
#   * "16" = Regular high school diploma
# * "18" = Some college, but less than 1 year
# * "21" = Bachelor's degree
#   * "22" = Master's degree
# * "23" = Professional degree beyond bachelor's
#   * "24" = Doctorate degree


# --- HELPER FUNCTION to report sample size & mean ---
report <- function(df, label) {
  cat("\n---", label, "---\n")
  cat("Sample size:", nrow(df), "\n")
  cat("Mean income:", mean(df$PERNP, na.rm = TRUE), "\n")
}


# --- GET SPECIFIED DATA FROM THE PUMS DATABASE ---

pums_data <- get_pums(
  variables = c("SEX", "AGEP", "PERNP", "OCCP", "SCHL", "MAR"),  # include any other needed vars
  state = "TX",
  survey = "acs5",
  year = 2023,
  recode = TRUE
)


# ====================================================
# STEP 1: Start with raw data
# ====================================================
step1 <- pums_data

report(step1, "Step 1: Raw data")


#####  FOR FEMALES  #####
# ====================================================
# STEP 2: FEMALES Filter by sex and age
# ====================================================
step2 <- step1 %>%
  filter(
    SEX == sex_code_man_female,
    AGEP >= age_min & AGEP <= age_max
  )

report(step2, "Step 2: Sex + Age")



# ====================================================
# STEP 3: FEMALES Apply occupation code (if set)
# ====================================================
step3 <- step2 %>%
  filter(
    PERNP > 0,
    !is.na(PERNP)
  )

report(step3, "Step 3: Earnings")





# ====================================================
# STEP 4: FEMALES Apply marital status (if set)
# ====================================================
if (!is.null(mar_code_man_man)) {
  step4 <- step3 %>% filter(MAR == mar_code_man_man)
} else {
  step4 <- step3
}

report(step4, "Step 4: Marital Status")



# ====================================================
# STEP 5: FEMALES Apply education codes (if set)
# ====================================================
if (!is.null(edu_code_man_man)) {
  if (length(edu_code_man_man) == 1) {
    step5 <- step4 %>% filter(SCHL == edu_code_man_man)
  } else {
    step5 <- step4 %>% filter(SCHL %in% edu_code_man_man)
  }
} else {
  step5 <- step4
}

report(step5, "Step 5: Education Codes")



# ====================================================
# STEP 6: FEMALES Apply education label (if set)
# ====================================================
if (!is.null(edu_label)) {
  step6 <- step5 %>%
    filter(str_detect(SCHL_label, regex(edu_label, ignore_case = TRUE)))
} else {
  step6 <- step5
}
report(step6, "Step 6: Education Label")


# ====================================================
# STEP 7: FEMALES Apply minimum education filter
# ====================================================
if (!is.null(min_edu)) {
  step7 <- step6 %>% filter(SCHL >= min_edu)
} else {
  step7 <- step6
}

report(step7, "Step 7: Minimum Education")


# ====================================================
# FINAL RESULT FOR FEMALES
# ====================================================
final_data <- step7
report(final_data, "Final Result")






#####  FOR MALES  #####
# ====================================================
# STEP 2: MALE Filter by sex and age
# ====================================================
step2 <- step1 %>%
  filter(
    SEX == sex_code_man_male
  )

report(step2, "Step 2: Sex")


step2.5 <- step2 %>%
  filter(
    AGEP >= age_min & AGEP <= age_max
    )




# ====================================================
# STEP 3: MALE Apply occupation code (if set)
# ====================================================
step3 <- step2.5 %>%
  filter(
    PERNP > 0,
    !is.na(PERNP)
  )

report(step3, "Step 3: Earnings")





# ====================================================
# STEP 4: MALE Apply marital status (if set)
# ====================================================
if (!is.null(mar_code_man)) {
  step4 <- step3 %>% filter(MAR == mar_code_man)
} else {
  step4 <- step3
}

report(step4, "Step 4: Marital Status")



# ====================================================
# STEP 5: MALE Apply education codes (if set)
# ====================================================
if (!is.null(edu_code_man)) {
  if (length(edu_code_man) == 1) {
    step5 <- step4 %>% filter(SCHL == edu_code_man)
  } else {
    step5 <- step4 %>% filter(SCHL %in% edu_code_man)
  }
} else {
  step5 <- step4
}

report(step5, "Step 5: Education Codes")



# ====================================================
# STEP 6: MALE Apply education label (if set)
# ====================================================
if (!is.null(edu_label)) {
  step6 <- step5 %>%
    filter(str_detect(SCHL_label, regex(edu_label, ignore_case = TRUE)))
} else {
  step6 <- step5
}
report(step6, "Step 6: Education Label")


# ====================================================
# STEP 7: MALE Apply minimum education filter
# ====================================================
if (!is.null(min_edu)) {
  step7 <- step6 %>% filter(SCHL >= min_edu)
} else {
  step7 <- step6
}

report(step7, "Step 7: Minimum Education")


# ====================================================
# FINAL RESULT FOR MALES
# ====================================================
final_data <- step7
report(final_data, "Final Result")








