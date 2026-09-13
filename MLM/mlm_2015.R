# Chapter 1
# MLM
# Re do: data analysis
# 20th April, 2025 @UGent

library(haven)
library(dplyr)  
library(lme4)     
library(glmmTMB)    
library(broom.mixed)
    
mlm <- read_dta("final_2015_long.dta")
mlm_2015 <- mlm %>% filter(year == 2015)


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
mlm_2015$urban_rural <- relevel(factor(mlm_2015$urban_rural), ref = "1")



mlm_2015 <- mlm_2015 %>% mutate(
  urban_rural = factor(urban_rural),
  education = factor(education),
  pche_quartile = factor(pche_quartile),
  gender = factor(gender),
  marital = factor(marital),
  age_group = factor(age_group)
)







# MLM
# ICC ~ prevalence
null.model_1 <- glmmTMB(
  pre_dyslipidemia_ ~ 1 + (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4)
                           )
)

summary(null.model_1)

# 提取方差成分
vc <- VarCorr(null.model_1)

# 省份水平方差
province_var <- as.numeric(vc$cond$province)

# 社区水平方差(嵌套在省份中)
community_var <- as.numeric(vc$cond$`communityID:province`)

# 个体水平方差(logit分布固定方差)
individual_var <- (pi^2)/3

# 计算各层次ICC
total_var <- province_var + community_var + individual_var

icc_province <- province_var / total_var
icc_community <- community_var / total_var

# 输出结果
list(
  province_variance = province_var,
  community_variance = community_var,
  icc_province = icc_province,
  icc_community = icc_community
)


# MLM
# ICC ~awareness

mlm_2015_sr_hypertension <- mlm_2015 %>% filter(pre_hypertension_ == 1)

mlm_2015_sr_diabetes <- mlm_2015 %>% filter(pre_diabetes_ == 1)

mlm_2015_sr_dyslipidemia <- mlm_2015 %>% filter(pre_dyslipidemia_ == 1)


null.model_2 <- glmmTMB(
  sr_dyslipidemia_ ~ 1 + (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_dyslipidemia,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4)
  )
)

summary(null.model_2)

# 提取方差成分
vc <- VarCorr(null.model_2)

# 省份水平方差
province_var <- as.numeric(vc$cond$province)

# 社区水平方差(嵌套在省份中)
community_var <- as.numeric(vc$cond$`communityID:province`)

# 个体水平方差(logit分布固定方差)
individual_var <- (pi^2)/3

# 计算各层次ICC
total_var <- province_var + community_var + individual_var

icc_province <- province_var / total_var
icc_community <- community_var / total_var

# 输出结果
list(
  province_variance = province_var,
  community_variance = community_var,
  icc_province = icc_province,
  icc_community = icc_community
)






model.hypertension_pre_2015 <- glmmTMB(
  pre_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.hypertension_pre_2015)

# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  pre_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  pre_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.hypertension_pre_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.hypertension_pre_2015, reduced_model_community, test = "Chisq")


# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.hypertension_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行






model.diabetes_pre_2015 <- glmmTMB(
  pre_diabetes_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group + 
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.diabetes_pre_2015)


# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  pre_diabetes_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  pre_diabetes_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.diabetes_pre_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.diabetes_pre_2015, reduced_model_community, test = "Chisq")



# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.diabetes_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行





model.dyslipidemia_pre_2015 <- glmmTMB(
  pre_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group + 
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.dyslipidemia_pre_2015)


# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.dyslipidemia_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行


# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  pre_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  pre_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.dyslipidemia_pre_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.dyslipidemia_pre_2015, reduced_model_community, test = "Chisq")





# Awareness
mlm_2015_sr_hypertension <- mlm_2015 %>% filter(pre_hypertension_ == 1)

model.hypertension_sr_2015 <- glmmTMB(
  sr_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_hypertension,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.hypertension_sr_2015)


# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  sr_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_hypertension,
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  sr_hypertension_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_hypertension,
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.hypertension_sr_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.hypertension_sr_2015, reduced_model_community, test = "Chisq")



# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.hypertension_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行






mlm_2015_sr_diabetes <- mlm_2015 %>% filter(pre_diabetes_ == 1)

model.diabetes_sr_2015 <- glmmTMB(
  sr_diabetes_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group + 
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_diabetes,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.diabetes_sr_2015)


# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  sr_diabetes_ ~
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_diabetes,
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  sr_diabetes_ ~
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_diabetes,
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.diabetes_sr_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.diabetes_sr_2015, reduced_model_community, test = "Chisq")



# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.diabetes_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行




mlm_2015_sr_dyslipidemia <- mlm_2015 %>% filter(pre_dyslipidemia_ == 1)

model.dyslipidemia_sr_2015 <- glmmTMB(
  sr_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group + 
    (1 | province/communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_dyslipidemia,
  control = glmmTMBControl(optimizer = nlminb, optArgs = list(iter.max = 1e4))
)

summary(model.dyslipidemia_sr_2015)


# LRT
# 简化模型1(只保留province随机效应)
reduced_model_province <- glmmTMB(
  sr_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | province),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_dyslipidemia,
)

# 简化模型2(只保留communityID随机效应)
reduced_model_community <- glmmTMB(
  sr_dyslipidemia_ ~ 
    urban_rural + education + pche_quartile + gender + marital + age_group +
    (1 | communityID),
  family = binomial(link = "logit"),
  data = mlm_2015_sr_dyslipidemia,
)

# 似然比检验
# 检验communityID随机效应(比较完整模型和只含province的模型)
anova(model.dyslipidemia_sr_2015, reduced_model_province, test = "Chisq")

# 检验province随机效应(比较完整模型和只含communityID的模型)
anova(model.dyslipidemia_sr_2015, reduced_model_community, test = "Chisq")





# 获取整理后的结果（OR + 95% CI）
tidy_results <- tidy(model.dyslipidemia_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(effect == "fixed") %>%  # 只保留固定效应
  mutate(
    # 格式化 OR 和 CI
    OR_CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    # 添加显著性标记
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, OR_CI, significance)

# 查看结果
print(tidy_results, n = Inf)  # 显示所有行




# or, 95% ci
# 加载必要的包
library(broom.mixed)
library(gtsummary)
library(dplyr)
library(tidyr)
library(gt)

# 创建函数来提取模型结果并计算OR和CI
extract_model_results <- function(model, outcome_name) {
  # 使用broom.mixed提取系数
  tidy_results <- broom.mixed::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # 筛选固定效应结果
  fixed_effects <- tidy_results %>% 
    filter(effect == "fixed" & term != "(Intercept)") %>%
    mutate(
      # 添加显著性星号
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      # 合并OR和CI，用HTML换行符<br>分隔
      OR_CI = sprintf(
        "%.2f%s<br>(%.2f-%.2f)", 
        estimate, 
        significance,
        conf.low, 
        conf.high
      ),
      # 单独保留p值用于排序（不显示）
      p.value = p.value
    ) %>%
    select(term, OR_CI, p.value)
  
  # 添加结果名称
  fixed_effects$Outcome <- outcome_name
  
  return(fixed_effects)
}

# 提取所有模型的结果
results_list <- list(
  extract_model_results(model.hypertension_pre_2015, "Hypertension (Prevalence)"),
  extract_model_results(model.diabetes_pre_2015, "Diabetes (Prevalence)"),
  extract_model_results(model.dyslipidemia_pre_2015, "Dyslipidemia (Prevalence)"),
  extract_model_results(model.hypertension_sr_2015, "Hypertension (Awareness)"),
  extract_model_results(model.diabetes_sr_2015, "Diabetes (Awareness)"),
  extract_model_results(model.dyslipidemia_sr_2015, "Dyslipidemia (Awareness)")
)

# 合并所有结果
combined_results <- bind_rows(results_list) %>%
  pivot_wider(
    names_from = Outcome,
    values_from = c(OR_CI, p.value),
    names_glue = "{Outcome}_{.value}"
  )

# 创建出版级表格
final_table <- combined_results %>%
  gt() %>%
  # 隐藏p值列（只用于排序）
  cols_hide(ends_with("p.value")) %>%
  # 添加列组标题
  tab_spanner_delim(delim = "_") %>%
  # 修改变量名显示
  cols_label(
    term = "Variable"
  ) %>%
  # 设置缺失值显示
  fmt_missing(columns = everything(), missing_text = "") %>%
  # 添加标题
  tab_header(
    title = "Multilevel Logistic Regression Results for Chronic Disease Prevalence and Awareness",
    subtitle = "Adjusted Odds Ratios (95% Confidence Intervals)"
  ) %>%
  # 设置表格选项
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    column_labels.font.weight = "bold"
  ) %>%
  # 设置HTML换行符生效
  fmt_markdown(columns = everything()) %>%
  # 添加脚注说明显著性
  tab_footnote(
    footnote = "* p<0.05, ** p<0.01, *** p<0.001",
    locations = cells_title(groups = "title")
  )

# 显示表格
final_table

# 如果需要保存为Word文档
# install.packages("flextable")
# final_table %>% as_flex_table() %>% flextable::save_as_docx(path = "results_table.docx")