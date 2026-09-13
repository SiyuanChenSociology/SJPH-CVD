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


# Define predictor variables
predictors <- c("urban_rural", "education", "gender", "marital", "pche_quartile", "age_group")

# Define outcome variables
outcomes <- c("pre_hypertension_", "pre_diabetes_", "pre_dyslipidemia_")

# Create descriptive statistics tables for each outcome
for (outcome in outcomes) {
  cat("\n\n=== Descriptive Statistics for", outcome, "===\n")
  
  # Filter only NA values for the current outcome (keep others)
  data_subset <- mlm_2015 %>% filter(!is.na(!!sym(outcome)))
  
  # Generate descriptive statistics table (stratified by outcome Yes/No)
  desc_table <- data_subset %>%
    mutate(
      outcome_status = factor(
        !!sym(outcome),
        levels = c(0, 1),
        labels = c("No", "Yes")
      )
    ) %>%
    tbl_summary(
      by = outcome_status,
      include = all_of(predictors),
      statistic = all_categorical() ~ "{n} ({p}%)",
      digits = all_categorical() ~ c(0, 1),
      missing = "no"
    ) %>%
    add_overall(
      last = FALSE,
      col_label = "**Total**"
    ) %>%
    modify_header(label ~ "**Variable**") %>%
    bold_labels() %>%
    modify_caption(
      paste("Descriptive Statistics for", outcome)
    )
  # Print the table
  desc_table %>%
    as_flex_table() %>%
    autofit() %>%
    print()
}

# Combined Chi-square test results function
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





# chi-square !!! use this
# 首先确保pre_hypertension_是因子变量
# 首先确保pre_hypertension_是因子变量
mlm_2015$pre_hypertension_ <- factor(mlm_2015$pre_hypertension_)

# 创建卡方检验函数
# hypertension
perform_chisq_tests <- function(data, outcome_var, predictor_vars) {
  results <- list()
  for (var in predictor_vars) {
    test <- chisq.test(data[[outcome_var]], data[[var]])
    results[[var]] <- data.frame(
      Predictor = var,
      Chi_square = round(test$statistic, 2),
      df = test$parameter,
      p_value = ifelse(test$p.value < 0.001, "<0.001", round(test$p.value, 3))
    )  # 这里添加了data.frame的右括号
  }    # 这里添加了for循环的右括号
  do.call(rbind, results)
}

# 指定预测变量
predictors <- c("urban_rural", "education", "gender", "marital", "pche_quartile", "age_group")

# 执行卡方检验
chisq_results <- perform_chisq_tests(mlm_2015, "pre_hypertension_", predictors)

# 打印专业格式的表格结果
knitr::kable(chisq_results, 
             caption = "卡方检验结果: pre_hypertension_与各预测变量的关系",
             align = c("l", "r", "r", "r"),
             col.names = c("预测变量", "卡方值", "自由度", "p值"))




# diabetes
perform_chisq_tests <- function(data, outcome_var, predictor_vars) {
  results <- list()
  for (var in predictor_vars) {
    test <- chisq.test(data[[outcome_var]], data[[var]])
    results[[var]] <- data.frame(
      Predictor = var,
      Chi_square = round(test$statistic, 2),
      df = test$parameter,
      p_value = ifelse(test$p.value < 0.001, "<0.001", round(test$p.value, 3))
    )  # 这里添加了data.frame的右括号
  }    # 这里添加了for循环的右括号
  do.call(rbind, results)
}

# 指定预测变量
predictors <- c("urban_rural", "education", "gender", "marital", "pche_quartile", "age_group")

# 执行卡方检验
chisq_results <- perform_chisq_tests(mlm_2015, "pre_diabetes_", predictors)

# 打印专业格式的表格结果
knitr::kable(chisq_results, 
             caption = "卡方检验结果: pre_diabetes_与各预测变量的关系",
             align = c("l", "r", "r", "r"),
             col.names = c("预测变量", "卡方值", "自由度", "p值"))



# dyslipidemia
perform_chisq_tests <- function(data, outcome_var, predictor_vars) {
  results <- list()
  for (var in predictor_vars) {
    test <- chisq.test(data[[outcome_var]], data[[var]])
    results[[var]] <- data.frame(
      Predictor = var,
      Chi_square = round(test$statistic, 2),
      df = test$parameter,
      p_value = ifelse(test$p.value < 0.001, "<0.001", round(test$p.value, 3))
    )  # 这里添加了data.frame的右括号
  }    # 这里添加了for循环的右括号
  do.call(rbind, results)
}

# 指定预测变量
predictors <- c("urban_rural", "education", "gender", "marital", "pche_quartile", "age_group")

# 执行卡方检验
chisq_results <- perform_chisq_tests(mlm_2015, "pre_dyslipidemia_", predictors)

# 打印专业格式的表格结果
knitr::kable(chisq_results, 
             caption = "卡方检验结果: pre_dyslipidemia_与各预测变量的关系",
             align = c("l", "r", "r", "r"),
             col.names = c("预测变量", "卡方值", "自由度", "p值"))

