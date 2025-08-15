library(tidycensus)
library(dplyr)
library(survey)
library(srvyr)
library(stringr)

# Set your Census API key (do this once)
# census_api_key("YOUR_API_KEY_HERE", install = TRUE)


# Get PUMS data for Texas - Never-married females, all occupations
tx_female_never_married <- get_pums(
  variables = c("SEX", "MAR", "OCCP", "PINCP", "PERNP", "AGEP", "COW"),
  state = "TX",
  survey = "acs5",  # 5-year for larger sample size
  year = 2022,
  recode = TRUE  # This gives us readable labels
)


# Get PUMS data for Texas - All males, all marital statuses
tx_male_all <- get_pums(
  variables = c("SEX", "MAR", "OCCP", "PINCP", "PERNP", "AGEP", "COW"),
  state = "TX",
  survey = "acs5",
  year = 2022,
  recode = TRUE
)


# Filter the datasets using numeric codes (recode=TRUE didn't work as expected)
# SEX: 1=Male, 2=Female | MAR: 1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married


# Female, never married, working age (25-64), with earnings > 0
female_never_married <- tx_female_never_married %>%
  dplyr::filter(
    SEX == "2",        # 2 = Female
    MAR == "5",        # 5 = Never married
    AGEP >= 25 & AGEP <= 64,
    PERNP > 0,         # Has earnings
    OCCP != "000N",    # Exclude not applicable occupation codes
    !is.na(OCCP),
    !is.na(PERNP)
  )



# Male, all marital statuses, working age (25-64), with earnings > 0
male_all <- tx_male_all %>%
  dplyr::filter(
    SEX == "1",        # 1 = Male
    AGEP >= 25 & AGEP <= 64,
    PERNP > 0,         # Has earnings
    OCCP != "000N",    # Exclude not applicable occupation codes
    !is.na(OCCP),
    !is.na(PERNP)
  )



# Check filter results
cat("=== FILTER RESULTS ===\n")
cat("Never-married females (age 25-64 with earnings):", nrow(female_never_married), "\n")
cat("All males (age 25-64 with earnings):", nrow(male_all), "\n")




# Combine datasets for comparison with group identifier
combined_data <- dplyr::bind_rows(
  female_never_married %>% dplyr::mutate(group = "Female_Never_Married"),
  male_all %>% dplyr::mutate(group = "Male_All")
)

# Create survey design object for proper weighting
svy_design <- combined_data %>%
  srvyr::as_survey_design(weights = PWGTP)

# Filter for surgeons specifically and check if data exists
# Note: OCCP codes for surgeons typically start with 311X
surgeons_filtered <- combined_data %>%
  dplyr::filter(stringr::str_detect(OCCP_label, "Surgeon") |
         OCCP %in% c("3110", "3111", "3112"))  # Common surgeon codes

# Check if we have surgeon data before proceeding
if (nrow(surgeons_filtered) > 0) {
  cat("Found", nrow(surgeons_filtered), "surgeon records\n")

  surgeons_data <- surgeons_filtered %>%
    srvyr::as_survey_design(weights = PWGTP)

  # Summary statistics for surgeons by group
  surgeon_summary <- surgeons_data %>%
    dplyr::group_by(group) %>%
    srvyr::summarise(
      count = srvyr::survey_total(),
      median_earnings = srvyr::survey_median(PERNP, na.rm = TRUE),
      mean_earnings = srvyr::survey_mean(PERNP, na.rm = TRUE),
      q25_earnings = srvyr::survey_quantile(PERNP, 0.25, na.rm = TRUE),
      q75_earnings = srvyr::survey_quantile(PERNP, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  # Display results
  print("=== SURGEON EARNINGS COMPARISON ===")
  print(surgeon_summary)
} else {
  cat("No surgeon data found. Checking occupation labels for similar matches...\n")

  # Show occupation labels that might contain surgeon-related terms
  surgeon_like <- combined_data %>%
    dplyr::filter(stringr::str_detect(OCCP_label, stringr::regex("surg|medical|doctor|physician", ignore_case = TRUE))) %>%
    dplyr::distinct(OCCP_label) %>%
    dplyr::arrange(OCCP_label)

  if (nrow(surgeon_like) > 0) {
    cat("Found these medical/surgical occupations:\n")
    print(surgeon_like$OCCP_label)
  } else {
    cat("No medical/surgical occupations found in the data.\n")
  }
}

# Additional analysis: Top occupations for never-married females
top_occupations_female <- female_never_married %>%
  srvyr::as_survey_design(weights = PWGTP) %>%
  dplyr::group_by(OCCP_label) %>%
  srvyr::summarise(
    count = srvyr::survey_total(),
    median_earnings = srvyr::survey_median(PERNP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(count >= 30) %>%  # Filter for statistical reliability
  dplyr::arrange(desc(median_earnings)) %>%
  dplyr::slice_head(n = 20)

print("=== TOP 20 OCCUPATIONS BY MEDIAN EARNINGS (Never-Married Females) ===")
print(top_occupations_female)

# Function to analyze specific occupation with better error handling
analyze_occupation <- function(occupation_pattern) {
  occupation_filtered <- combined_data %>%
    dplyr::filter(stringr::str_detect(OCCP_label, stringr::regex(occupation_pattern, ignore_case = TRUE)))

  if(nrow(occupation_filtered) > 0) {
    cat("\nFound", nrow(occupation_filtered), "records for", occupation_pattern, "\n")

    occupation_data <- occupation_filtered %>%
      srvyr::as_survey_design(weights = PWGTP)

    # Check if we have both groups
    groups_present <- unique(occupation_filtered$group)

    if(length(groups_present) == 2) {
      summary_stats <- occupation_data %>%
        dplyr::group_by(group) %>%
        srvyr::summarise(
          count = srvyr::survey_total(),
          median_earnings = srvyr::survey_median(PERNP, na.rm = TRUE),
          mean_earnings = srvyr::survey_mean(PERNP, na.rm = TRUE),
          .groups = "drop"
        )

      cat("=== ANALYSIS FOR:", occupation_pattern, "===\n")
      print(summary_stats)

      return(summary_stats)
    } else {
      cat("Note: Only found data for group(s):", paste(groups_present, collapse = ", "), "\n")
      return(NULL)
    }
  } else {
    cat("No data found for occupation pattern:", occupation_pattern, "\n")
    return(NULL)
  }
}

# Example usage for other occupations
analyze_occupation("Physician")
analyze_occupation("Accountant")
analyze_occupation("Lawyer")

# === PAY GAP ANALYSIS ===

# Function to calculate pay gap and test significance
calculate_pay_gap <- function(data, occupation_filter = NULL) {

  if (!is.null(occupation_filter)) {
    filtered_data <- data %>%
      dplyr::filter(stringr::str_detect(OCCP_label, stringr::regex(occupation_filter, ignore_case = TRUE)))
  } else {
    filtered_data <- data
  }

  # Check if we have enough data
  if (nrow(filtered_data) < 10) {
    cat("Insufficient data for",
        ifelse(is.null(occupation_filter), "overall analysis", occupation_filter),
        "(n =", nrow(filtered_data), ")\n")
    return(NULL)
  }

  # Check if we have both groups
  groups_present <- unique(filtered_data$group)
  if (length(groups_present) < 2) {
    cat("Only one group present for",
        ifelse(is.null(occupation_filter), "overall analysis", occupation_filter),
        ":", paste(groups_present, collapse = ", "), "\n")
    return(NULL)
  }

  # Create survey design
  svy_data <- filtered_data %>%
    srvyr::as_survey_design(weights = PWGTP)

  # Get summary stats by group
  group_stats <- svy_data %>%
    dplyr::group_by(group) %>%
    srvyr::summarise(
      count = srvyr::survey_total(),
      median_earnings = srvyr::survey_median(PERNP, na.rm = TRUE),
      mean_earnings = srvyr::survey_mean(PERNP, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate pay gaps if we have both groups
  if (nrow(group_stats) == 2) {
    female_median <- group_stats$median_earnings[group_stats$group == "Female_Never_Married"]
    male_median <- group_stats$median_earnings[group_stats$group == "Male_All" | group_stats$group == "Male_Never_Married"]

    female_mean <- group_stats$mean_earnings[group_stats$group == "Female_Never_Married"]
    male_mean <- group_stats$mean_earnings[group_stats$group == "Male_All" | group_stats$group == "Male_Never_Married"]

    # Calculate gaps (positive = male earnings higher)
    median_gap_dollars <- male_median - female_median
    median_gap_percent <- ((male_median - female_median) / female_median) * 100

    mean_gap_dollars <- male_mean - female_mean
    mean_gap_percent <- ((male_mean - female_mean) / female_mean) * 100

    # Statistical test using survey t-test
    tryCatch({
      test_result <- survey::svyttest(PERNP ~ group, svy_data)
      p_value <- test_result$p.value
    }, error = function(e) {
      cat("Warning: Could not perform t-test for",
          ifelse(is.null(occupation_filter), "overall", occupation_filter), "\n")
      p_value <- NA
    })

    # Create results summary
    results <- list(
      occupation = ifelse(is.null(occupation_filter), "ALL OCCUPATIONS", occupation_filter),
      group_stats = group_stats,
      median_gap_dollars = median_gap_dollars,
      median_gap_percent = round(median_gap_percent, 1),
      mean_gap_dollars = mean_gap_dollars,
      mean_gap_percent = round(mean_gap_percent, 1),
      t_test_p_value = p_value,
      significant = !is.na(p_value) && p_value < 0.05,
      female_count = group_stats$count[group_stats$group == "Female_Never_Married"],
      male_count = group_stats$count[group_stats$group != "Female_Never_Married"]
    )

    return(results)
  } else {
    cat("Insufficient data for comparison in",
        ifelse(is.null(occupation_filter), "overall analysis", occupation_filter), "\n")
    return(NULL)
  }
}

# Overall pay gap analysis
overall_gap <- calculate_pay_gap(combined_data)

if (!is.null(overall_gap)) {
  cat("\n=== OVERALL PAY GAP ANALYSIS ===\n")
  cat("Comparison: Never-Married Females vs All Males (Texas, Age 25-64)\n")
  cat("Female Sample Size:", round(overall_gap$female_count), "\n")
  cat("Male Sample Size:", round(overall_gap$male_count), "\n\n")

  cat("MEDIAN EARNINGS:\n")
  cat("- Gap: $", format(round(overall_gap$median_gap_dollars), big.mark = ","),
      " (", overall_gap$median_gap_percent, "%)\n", sep = "")

  cat("MEAN EARNINGS:\n")
  cat("- Gap: $", format(round(overall_gap$mean_gap_dollars), big.mark = ","),
      " (", overall_gap$mean_gap_percent, "%)\n", sep = "")

  cat("\nSTATISTICAL SIGNIFICANCE:\n")
  cat("- T-test p-value:", format(overall_gap$t_test_p_value, scientific = TRUE), "\n")
  cat("- Significant at α=0.05:", overall_gap$significant, "\n")

  if (overall_gap$significant) {
    cat("- Result: STATISTICALLY SIGNIFICANT pay gap\n")
  } else {
    cat("- Result: No statistically significant pay gap\n")
  }
}

# Occupation-specific pay gap analysis
occupations_to_test <- c("Physician", "Surgeon", "Lawyer", "Accountant",
                        "Engineer", "Teacher", "Nurse", "Manager")

cat("\n=== OCCUPATION-SPECIFIC PAY GAP ANALYSIS ===\n")

occupation_results <- list()
for (occ in occupations_to_test) {
  gap_result <- calculate_pay_gap(combined_data, occ)

  if (!is.null(gap_result)) {
    occupation_results[[occ]] <- gap_result

    cat("\n", occ, ":\n", sep = "")
    cat("- Female count:", round(gap_result$female_count),
        " | Male count:", round(gap_result$male_count), "\n")
    cat("- Median gap: $", format(round(gap_result$median_gap_dollars), big.mark = ","),
        " (", gap_result$median_gap_percent, "%)\n", sep = "")
    cat("- P-value:", format(gap_result$t_test_p_value, scientific = TRUE), "\n")
    cat("- Significant:", gap_result$significant, "\n")
  }
}

# Summary table of results
if (length(occupation_results) > 0) {
  cat("\n=== SUMMARY TABLE ===\n")
  summary_df <- data.frame(
    Occupation = names(occupation_results),
    Female_Count = sapply(occupation_results, function(x) round(x$female_count)),
    Male_Count = sapply(occupation_results, function(x) round(x$male_count)),
    Median_Gap_Percent = sapply(occupation_results, function(x) x$median_gap_percent),
    P_Value = sapply(occupation_results, function(x) round(x$t_test_p_value, 4)),
    Significant = sapply(occupation_results, function(x) x$significant)
  )

  print(summary_df)
}

# Alternative analysis: Never-married females vs never-married males (more comparable)
cat("\n=== ALTERNATIVE ANALYSIS: Never-Married Females vs Never-Married Males ===\n")

# Get never-married males for better comparison
tx_male_never_married <- get_pums(
  variables = c("SEX", "MAR", "OCCP", "PINCP", "PERNP", "AGEP", "COW"),
  state = "TX",
  survey = "acs5",
  year = 2022,
  recode = TRUE
)

male_never_married <- tx_male_never_married %>%
  dplyr::filter(
    SEX == "1",        # 1 = Male
    MAR == "5",        # 5 = Never married
    AGEP >= 25 & AGEP <= 64,
    PERNP > 0,
    OCCP != "000N",    # Exclude not applicable occupation codes
    !is.na(OCCP),
    !is.na(PERNP)
  )

# Combine for apples-to-apples comparison
comparable_data <- dplyr::bind_rows(
  female_never_married %>% dplyr::mutate(group = "Female_Never_Married"),
  male_never_married %>% dplyr::mutate(group = "Male_Never_Married")
)

# Calculate comparable pay gap
comparable_gap <- calculate_pay_gap(comparable_data)

if (!is.null(comparable_gap)) {
  cat("Never-Married Females vs Never-Married Males:\n")
  cat("- Median gap:", comparable_gap$median_gap_percent, "%\n")
  cat("- Significant:", comparable_gap$significant, "\n")
  cat("- P-value:", format(comparable_gap$t_test_p_value, scientific = TRUE), "\n")
}

# Show all unique occupation labels (for reference)
cat("\n=== UNIQUE OCCUPATIONS IN DATASET (first 50) ===\n")
unique_occs <- combined_data %>%
  dplyr::distinct(OCCP_label) %>%
  dplyr::arrange(OCCP_label) %>%
  dplyr::slice_head(n = 50)
print(unique_occs$OCCP_label)
