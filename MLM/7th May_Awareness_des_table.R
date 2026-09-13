library(haven)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(flextable)


# descriptive analysis

library(haven)
library(dplyr)  
library(tidyverse)
library(gtsummary)
library(flextable)

mlm_2015 <- read_dta("final_2015_long.dta")




# Outcome
table(mlm_2015$pre_hypertension_)  
table(mlm_2015$sr_hypertension_)

table(mlm_2015$pre_diabetes_)  
table(mlm_2015$sr_diabetes_)

table(mlm_2015$pre_dyslipidemia_)  
table(mlm_2015$sr_dyslipidemia_)


# Predictors
lapply(mlm_2015[, c("urban_rural", "education", "pche_quartile", "gender", "marital", "age_group")], table)

levels(mlm_2015$urban_rural)
mlm_2015$urban_rural <- relevel(factor(mlm_2015$urban_rural), ref = "0")



mlm_2015 <- mlm_2015 %>% mutate(
  urban_rural = factor(urban_rural),
  education = factor(education),
  gender = factor(gender),
  marital = factor(marital),
  pche_quartile = factor(pche_quartile),
  age_group = factor(age_group)
)

mlm_2015_sr_hypertension <- mlm_2015 %>% filter(pre_hypertension_ == 1)
mlm_2015_sr_diabetes <- mlm_2015 %>% filter(pre_diabetes_ == 1)
mlm_2015_sr_dyslipidemia <- mlm_2015 %>% filter(pre_dyslipidemia_ == 1)




# 1. 合并三个数据集并创建长格式数据 (保持不变)
sr_combined_long <- bind_rows(
  mlm_2015_sr_hypertension %>% mutate(Outcome = "SR-Hypertension"),
  mlm_2015_sr_diabetes %>% mutate(Outcome = "SR-Diabetes"),
  mlm_2015_sr_dyslipidemia %>% mutate(Outcome = "SR-Dyslipidemia")
) %>%
  filter(
    !is.na(urban_rural),
    !is.na(education),
    !is.na(pche_quartile),
    !is.na(gender),
    !is.na(marital),
    !is.na(age_group)
  ) %>%
  mutate(
    Status = case_when(
      Outcome == "SR-Hypertension" ~ factor(sr_hypertension_, levels = c(0, 1), labels = c("No", "Yes")),
      Outcome == "SR-Diabetes" ~ factor(sr_diabetes_, levels = c(0, 1), labels = c("No", "Yes")),
      Outcome == "SR-Dyslipidemia" ~ factor(sr_dyslipidemia_, levels = c(0, 1), labels = c("No", "Yes"))
    ),
    urban_rural = factor(urban_rural),
    education = factor(education),
    pche_quartile = factor(pche_quartile),
    gender = factor(gender),
    marital = factor(marital),
    age_group = factor(age_group)
  )

# 2. 创建合并的大表（调整为行百分比）
sr_combined_table <- sr_combined_long %>%
  tbl_strata(
    strata = "Outcome",
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        by = Status,
        include = c(
          urban_rural,
          education,
          pche_quartile,
          gender,
          marital,
          age_group
        ),
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 1),
        missing = "no"
      ) %>%
      add_overall(
        last = FALSE,
        col_label = "**Total**"
      ) %>%
      modify_spanning_header(
        c(stat_1, stat_2) ~ "**Self-reported Status**"
      )
  ) %>%
  modify_header(label ~ "**Predictor**") %>%
  bold_labels() %>%
  modify_caption(
    "Characteristics by Health Outcomes"
  )

# 3. 打印合并的大表
cat("\n\nCombined Characteristics Table with Row Percentages\n")
sr_combined_table %>%
  as_flex_table() %>%
  set_caption("Combined Characteristics Showing Row Percentages") %>%
  autofit() %>%
  print()

# 4. 合并的卡方检验结果（保持不变）
run_combined_chi_tests <- function(data, outcomes, predictors) {
  purrr::map_dfr(
    outcomes,
    function(outcome) {
      purrr::map_dfr(
        predictors,
        function(var) {
          tbl <- table(
            data$Status[data$Outcome == outcome],
            data[[var]][data$Outcome == outcome]
          )
          
          if (any(dim(tbl) < 2)) {
            return(tibble(
              Outcome = outcome,
              Variable = var,
              `χ²` = NA_character_,
              df = NA_integer_,
              `p-value` = "N/A (dim<2)",
              `Test Used` = "None",
              Significance = ""
            ))
          }
          
          test_result <- tryCatch(
            {
              chi_test <- chisq.test(tbl, correct = FALSE)
              
              if (any(chi_test$expected < 5)) {
                sim_test <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
                list(
                  test = sim_test,
                  warning = "Some expected counts <5, using simulated p-value",
                  method = "Chi-square (simulated)"
                )
              } else {
                list(
                  test = chi_test,
                  warning = NA_character_,
                  method = "Chi-square"
                )
              }
            },
            error = function(e) {
              return(NULL)
            }
          )
          
          if (is.null(test_result)) {
            return(tibble(
              Outcome = outcome,
              Variable = var,
              `χ²` = NA_character_,
              df = NA_integer_,
              `p-value` = "N/A (test failed)",
              `Test Used` = "None",
              Significance = ""
            ))
          }
          
          test <- test_result$test
          p_stars <- case_when(
            test$p.value < 0.001 ~ "***",
            test$p.value < 0.01 ~ "**",
            test$p.value < 0.05 ~ "*",
            TRUE ~ ""
          )
          
          tibble(
            Outcome = outcome,
            Variable = var,
            `χ²` = sprintf("%.2f", test$statistic),
            df = ifelse(test_result$method == "Chi-square (simulated)", 
                        NA_integer_, 
                        test$parameter),
            `p-value` = ifelse(test$p.value < 0.001, 
                               "<0.001", 
                               sprintf("%.3f", test$p.value)),
            `Test Used` = test_result$method,
            `Warning` = ifelse(is.na(test_result$warning), 
                               NA_character_, 
                               test_result$warning),
            Significance = p_stars
          )
        }
      )
    }
  )
}

# 使用示例（保持不变）
outcomes <- c("SR-Hypertension", "SR-Diabetes", "SR-Dyslipidemia")
predictors <- c("urban_rural", "education", "pche_quartile", "gender", "marital", "age_group")

if (exists("sr_combined_long")) {
  sr_combined_test_results <- run_combined_chi_tests(
    data = sr_combined_long,
    outcomes = outcomes,
    predictors = predictors
  )
  
  cat("\n\nCombined Association Test Results for All Three SR Health Outcomes\n")
  sr_combined_test_results %>%
    flextable() %>%
    set_caption("Combined Association Test Results") %>%
    merge_v(j = "Outcome") %>%
    autofit() %>%
    add_footer_lines(
      "Significance codes: ***p<0.001, **p<0.01, *p<0.05\n
      Note: When expected counts <5, p-values are obtained via Monte Carlo simulation (2000 replicates)"
    ) %>%
    print()
}