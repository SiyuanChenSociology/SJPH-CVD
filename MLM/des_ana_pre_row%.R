library(haven)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(flextable)

# Load data
mlm_2015 <- read_dta("final_2015_long.dta")

# Outcome frequencies
table(mlm_2015$pre_hypertension_)  
table(mlm_2015$sr_hypertension_)
table(mlm_2015$pre_diabetes_)  
table(mlm_2015$sr_diabetes_)
table(mlm_2015$pre_dyslipidemia_)  
table(mlm_2015$sr_dyslipidemia_)

# Predictor frequencies
lapply(mlm_2015[, c("urban_rural", "education", "pche_quartile", "gender", "marital", "age_group")], table)

# Set reference levels and factor variables
mlm_2015$urban_rural <- relevel(factor(mlm_2015$urban_rural), ref = "0")

mlm_2015 <- mlm_2015 %>% mutate(
  urban_rural = factor(urban_rural),
  education = factor(education),
  gender = factor(gender),
  marital = factor(marital),
  pche_quartile = factor(pche_quartile),
  age_group = factor(age_group)
)

# Define predictor and outcome variables
predictors <- c("urban_rural", "education", "gender", "marital", "pche_quartile", "age_group")
outcomes <- c("pre_hypertension_", "pre_diabetes_", "pre_dyslipidemia_")

# Create descriptive statistics tables with TRUE row percentages
for (outcome in outcomes) {
  cat("\n\n=== Descriptive Statistics for", outcome, "(Row Percentages) ===\n")
  
  data_subset <- mlm_2015 %>% 
    filter(!is.na(!!sym(outcome))) %>%
    mutate(
      outcome_status = factor(!!sym(outcome), 
                              levels = c(0, 1),
                              labels = c("No", "Yes"))
    )
  
  # 正确计算行百分比的方法
  desc_table <- data_subset %>%
    tbl_summary(
      include = all_of(predictors),  # 预测变量作为行
      by = outcome_status,           # 结果变量作为列
      statistic = list(all_categorical() ~ "{n} ({p}%)"),
      digits = all_categorical() ~ c(0, 1),
      missing = "no",
      percent = "row"  # 关键参数：计算行百分比
    ) %>%
    modify_header(label ~ "**Predictor**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Outcome Status**") %>%
    bold_labels() %>%
    modify_caption(paste("Row Percentages for", outcome, 
                         "(Percentages within each predictor category)")) %>%
    add_p()
  
  # 打印表格
  desc_table %>%
    as_flex_table() %>%
    autofit() %>%
    print()
}
# Combined Chi-square test results function (unchanged)
run_chi_square_tests <- function(data, outcome, predictors) {
  data_subset <- data %>% filter(!is.na(!!sym(outcome)))
  
  results <- lapply(predictors, function(predictor) {
    # 创建列联表
    tbl <- table(data_subset[[outcome]], data_subset[[predictor]])
    
    # 检查是否可以进行卡方检验
    if (any(dim(tbl) < 2)) {
      return(tibble(
        Variable = predictor,
        `χ²` = NA_real_,
        df = NA_integer_,
        `p-value` = NA_character_,
        `Test Used` = "Insufficient data",
        `Warning` = "Table has <2 categories"
      ))
    }
    
    # 进行卡方检验
    chi_test <- chisq.test(tbl, correct = FALSE)
    
    # 检查是否需要Fisher精确检验
    if (any(chi_test$expected < 5)) {
      fisher_p <- fisher.test(tbl)$p.value
      warning_msg <- "Some expected counts <5, reporting Fisher's p-value"
    } else {
      fisher_p <- NA_real_
      warning_msg <- NA_character_
    }
    
    # 准备结果
    tibble(
      Variable = predictor,
      `χ²` = round(chi_test$statistic, 2),
      df = chi_test$parameter,
      `Chi-square p-value` = ifelse(chi_test$p.value < 0.001, 
                                    "<0.001", 
                                    sprintf("%.3f", chi_test$p.value)),
      `Fisher's p-value` = ifelse(is.na(fisher_p), NA_character_,
                                  ifelse(fisher_p < 0.001,
                                         "<0.001",
                                         sprintf("%.3f", fisher_p))),
      `Test Used` = ifelse(is.na(fisher_p), "Chi-square", "Chi-square (Fisher's p)"),
      `Warning` = warning_msg
    )
  })
  
  bind_rows(results)
}

# 生成合并结果
sr_combined_test_results <- map_dfr(outcomes, function(outcome) {
  run_chi_square_tests(mlm_2015, outcome, predictors) %>%
    mutate(Outcome = outcome) %>%
    select(Outcome, Variable, `Test Used`, `χ²`, df, 
           `Chi-square p-value`, `Fisher's p-value`, Warning)
})

# 打印专业结果表格
sr_combined_test_results %>%
  flextable() %>%
  set_caption("Association Analysis Results") %>%
  merge_v(j = "Outcome") %>%
  colformat_num(j = c("χ²", "df"), digits = 2) %>%
  autofit() %>%
  add_footer_lines(
    "Interpretation:\n1. χ² and df always reported from Pearson's chi-square test\n2. When expected counts <5, Fisher's exact p-value is provided\n3. p-values <0.001 are marked as '<0.001'"
  ) %>%
  print()