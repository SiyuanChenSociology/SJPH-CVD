library(glmmTMB)

# 1. 系数提取函数（明确提取0和2 vs 1的系数）
get_coefs <- function(model) {
  coef_table <- summary(model)$coefficients$cond
  
  list(
    urban0_vs_ref = c(  # 0组 vs 参照组1
      estimate = coef_table["urban_rural0", "Estimate"],
      se = coef_table["urban_rural0", "Std. Error"]
    ),
    urban2_vs_ref = c(  # 2组 vs 参照组1
      estimate = coef_table["urban_rural2", "Estimate"],
      se = coef_table["urban_rural2", "Std. Error"]
    )
  )
}

# 2. 提取系数
hypertension <- get_coefs(model.hypertension_sr_2015)
diabetes <- get_coefs(model.diabetes_sr_2015)
dyslipidemia <- get_coefs(model.dyslipidemia_sr_2015)


# 3. Wald检验函数（计算两模型间系数差异）
run_wald_test <- function(coef1, coef2) {
  diff <- coef1["estimate"] - coef2["estimate"]
  se_diff <- sqrt(coef1["se"]^2 + coef2["se"]^2)
  data.frame(
    estimate_diff = diff,
    se_diff = se_diff,
    wald_stat = (diff / se_diff)^2,
    p_value = pchisq((diff / se_diff)^2, df = 1, lower.tail = FALSE)
  )
}

# 4. 执行两组比较（0 vs 1 和 2 vs 1分别进行）
make_comparisons <- function(coef_name) {
  list(
    hyp_vs_dia = run_wald_test(hypertension[[coef_name]], diabetes[[coef_name]]),
    hyp_vs_dys = run_wald_test(hypertension[[coef_name]], dyslipidemia[[coef_name]]),
    dia_vs_dys = run_wald_test(diabetes[[coef_name]], dyslipidemia[[coef_name]])
  )
}

# 对urban0_vs_ref和urban2_vs_ref分别比较
results_urban0 <- do.call(rbind, make_comparisons("urban0_vs_ref"))
results_urban2 <- do.call(rbind, make_comparisons("urban2_vs_ref"))

# 5. 添加比较标识
add_labels <- function(results, urban_level) {
  results$comparison <- c("hypertension vs diabetes", 
                          "hypertension vs dyslipidemia", 
                          "diabetes vs dyslipidemia")
  results$urban_level <- urban_level
  results$reference <- "urban_rural1"  # 明确参照组
  return(results)
}

results_urban0 <- add_labels(results_urban0, "urban_rural0 vs urban_rural1")
results_urban2 <- add_labels(results_urban2, "urban_rural2 vs urban_rural1")

# 6. 合并结果并校正
final_results <- rbind(results_urban0, results_urban2)
final_results$adjusted_p_value <- p.adjust(final_results$p_value, method = "bonferroni")

# 7. 按比较类型排序输出
final_results <- final_results[order(final_results$urban_level), c("urban_level", "reference", "comparison", 
                                                                   "estimate_diff", "se_diff", 
                                                                   "wald_stat", "p_value", 
                                                                   "adjusted_p_value")]
print(final_results)


# 重新组织结果输出
final_results <- final_results %>%
  mutate(
    comparison_type = case_when(
      grepl("urban_rural0", urban_level) ~ "0 vs 1 (Reference)",
      grepl("urban_rural2", urban_level) ~ "2 vs 1 (Reference)"
    ),
    model_comparison = comparison
  ) %>%
  select(comparison_type, model_comparison, everything(), -urban_level, -comparison)

# 按比较类型分组输出
cat("\n=== 城乡变量效应跨模型比较 ===\n")
cat("\n◆ 比较组：0 vs 参照组(1)\n")
print(subset(final_results, comparison_type == "0 vs 1 (Reference)"), 
      row.names = FALSE)

cat("\n◆ 比较组：2 vs 参照组(1)\n")
print(subset(final_results, comparison_type == "2 vs 1 (Reference)"), 
      row.names = FALSE)




# 假设final_results已经生成，以下是完整的表格输出代码
library(knitr)
library(kableExtra)
library(dplyr)

# 1. 格式化结果数据框
formatted_table <- final_results %>%
  mutate(
    across(c(estimate_diff, se_diff), ~ sprintf("%.3f", .x)),
    wald_stat = sprintf("%.2f", wald_stat),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    adjusted_p_value = ifelse(adjusted_p_value < 0.001, "<0.001", sprintf("%.3f", adjusted_p_value))
  ) %>%
  select(
    "比较组" = comparison_type,
    "模型比较" = model_comparison,
    "系数差异" = estimate_diff,
    "标准误" = se_diff,
    "Wald统计量" = wald_stat,
    "p值" = p_value,
    "校正p值" = adjusted_p_value
  )

# 2. 创建出版级表格
final_table <- formatted_table %>%
  kable(
    format = "html",
    caption = "<b>表1. 城乡变量效应in awareness跨模型比较结果</b>",
    align = c("l", "l", "c", "c", "c", "c", "c"),
    row.names = FALSE 
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12,
    position = "center"
  ) %>%
  add_header_above(
    c(" " = 2, "统计量" = 3, "显著性检验" = 2),
    bold = TRUE,
    background = "#f8f8f8"
  ) %>%
  pack_rows(
    index = c("0 vs 1 (参照组)" = 3, "2 vs 1 (参照组)" = 3),
    bold = TRUE,
    background = "#f0f0f0"
  ) %>%
  footnote(
    general = "注：表格显示了不同健康结局模型中城乡变量的系数差异比较结果。",
    general_title = "",
    footnote_as_chunk = TRUE
  ) %>%
  row_spec(0, bold = TRUE, background = "#e6e6e6") %>%  # 表头格式
  column_spec(1, bold = TRUE)  # 第一列加粗

# 3. 输出表格
final_table



