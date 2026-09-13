library(ggplot2)
library(dplyr)
library(broom.mixed)
library(gridExtra)
library(gtable)
library(scales)

# 1. 修正后的数据准备函数
prepare_plot_data <- function() {
  # 第一步：单独处理每个模型
  df1 <- broom.mixed::tidy(model.hypertension_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Hypertension", model_type = "Prevalence")
  
  df2 <- broom.mixed::tidy(model.diabetes_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Diabetes", model_type = "Prevalence")
  
  df3 <- broom.mixed::tidy(model.dyslipidemia_pre_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Dyslipidemia", model_type = "Prevalence")
  
  df4 <- broom.mixed::tidy(model.hypertension_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Hypertension", model_type = "Awareness")
  
  df5 <- broom.mixed::tidy(model.diabetes_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Diabetes", model_type = "Awareness")
  
  df6 <- broom.mixed::tidy(model.dyslipidemia_sr_2015, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("urban_rural", term)) %>%
    mutate(disease = "Dyslipidemia", model_type = "Awareness")
  
  # 第二步：合并所有数据
  all_data <- bind_rows(df1, df2, df3, df4, df5, df6)
  
  # 第三步：统一处理变量
  all_data <- all_data %>%
    mutate(
      urban_label = ifelse(term == "urban_rural0", "Rural hukou in rural areas", "Urban hukou holders"),
      or_ci = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
      significance = ifelse(p.value < 0.001, "***",
                            ifelse(p.value < 0.01, "**",
                                   ifelse(p.value < 0.05, "*", "")))
    )
  
  # 第四步：因子化和排序
  all_data$disease <- factor(all_data$disease, 
                             levels = c("Hypertension", "Diabetes", "Dyslipidemia"))
  all_data$model_type <- factor(all_data$model_type,
                                levels = c("Prevalence", "Awareness"))
  all_data$row_id <- paste(all_data$model_type, all_data$disease, all_data$urban_label, sep = "|")
  all_data$y_pos <- as.numeric(factor(all_data$row_id, levels = unique(all_data$row_id)))
  
  # 返回最终结果
  return(all_data)
}



# 2. 创建文本表格
create_text_table <- function(data) {
  left_text <- data %>%
    distinct(model_type, disease, .keep_all = TRUE) %>%
    mutate(label = paste0(model_type, "\n", disease)) %>%
    select(y_pos, label)
  
  middle_text <- data %>%
    mutate(label = urban_label) %>%
    select(y_pos, label)
  
  right_text <- data %>%
    mutate(label = paste(or_ci, significance)) %>%
    select(y_pos, label)
  
  list(left = left_text, middle = middle_text, right = right_text)
}

# 3. 创建文本图
create_text_plot <- function(text_data, title = NULL) {
  ggplot(text_data, aes(x = 0, y = y_pos, label = label)) +
    geom_text(hjust = 0, size = 4) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = range(text_data$y_pos)) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.margin = margin(0, 0.2, 0, 0.2, "cm"))
}

# 4. 创建森林图
create_forest_plot <- function(data) {
  ggplot(data, aes(x = estimate, y = y_pos)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high, color = urban_label),
                    size = 0.8, linewidth = 1.2, fatten = 2) +
    scale_x_log10(limits = c(0.4, 2.5),
                  breaks = c(0.5, 0.75, 1, 1.5, 2),
                  labels = label_number(accuracy = 0.01)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    labs(x = "Odds Ratio (log scale)") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(0, 0.5, 0, 0, "cm")
    )
}

# 5. 组合图形
create_final_plot <- function(data) {
  text_data <- create_text_table(data)
  
  left_plot <- create_text_plot(text_data$left, "Model Type\n& Disease")
  middle_plot <- create_text_plot(text_data$middle, "Hukou Status")
  right_text_plot <- create_text_plot(text_data$right, "OR (95% CI)")
  forest_plot <- create_forest_plot(data)
  
  grid.arrange(
    left_plot, middle_plot, right_text_plot, forest_plot,
    ncol = 4,
    widths = c(1.2, 1.8, 1.2, 1.5),
    top = textGrob("Urban-Rural Hukou Effects on Chronic Diseases", 
                   gp = gpar(fontsize = 14, fontface = "bold")),
    bottom = textGrob("Reference: Rural hukou in urban areas | Error bars: 95% CI | *p<0.05 **p<0.01 ***p<0.001",
                      gp = gpar(fontsize = 10))
  )
}

# 6. 执行绘图
plot_data <- prepare_plot_data()
final_plot <- create_final_plot(plot_data)

# 7. 保存图形
ggsave("final_forest_plot.png", final_plot, width = 14, height = 10, dpi = 600)