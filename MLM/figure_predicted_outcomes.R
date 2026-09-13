
library(emmeans)
library(ggplot2)
library(patchwork)
library(ggsci)

# 创建函数统一处理模型预测
get_emm_data <- function(model, outcome_name) {
  emm <- emmeans(
    model, 
    specs = ~ urban_rural,
    type = "response"
  )
  
  as.data.frame(emm) %>%
    mutate(
      outcome = outcome_name,
      urban_rural = factor(urban_rural, 
                           levels = c(0, 1, 2),
                           labels = c("Rural residents with rural hukou", 
                                      "Urban residents with rural hukou", 
                                      "Urban hukou holders")),
      prob_label = sprintf("%.1f%%", prob*100),
      ci_label = sprintf("(%.1f-%.1f)", asymp.LCL*100, asymp.UCL*100)
    )
}

# 获取所有模型的预测数据
pre_data <- rbind(
  get_emm_data(model.hypertension_pre_2015, "Hypertension"),
  get_emm_data(model.diabetes_pre_2015, "Diabetes"),
  get_emm_data(model.dyslipidemia_pre_2015, "Dyslipidemia")
) %>% mutate(type = "Prevalence")

sr_data <- rbind(
  get_emm_data(model.hypertension_sr_2015, "Hypertension"),
  get_emm_data(model.diabetes_sr_2015, "Diabetes"),
  get_emm_data(model.dyslipidemia_sr_2015, "Dyslipidemia")
) %>% mutate(type = "Awareness")

# 合并数据
plot_data <- rbind(pre_data, sr_data) %>%
  mutate(
    type = factor(type, levels = c("Prevalence", "Awareness")),
    outcome = factor(outcome, levels = c("Hypertension", "Diabetes", "Dyslipidemia"))
  )
# 修改绘图函数 - 交换x轴和填充变量
# 修改绘图函数 - 调整字体大小和粗细
create_plot <- function(data, plot_title) {
  # 设置统一的y轴范围
  y_range <- c(0, 0.80)
  
  # 计算统一的标签位置偏移量
  label_offset <- 0.80 * 0.05
  
  ggplot(data, aes(x = outcome, y = prob, fill = urban_rural)) +
    # 柱状图
    geom_col(position = position_dodge(width = 0.8), width = 0.7, 
             alpha = 0.9, color = "black", linewidth = 0.3) +
    
    # 误差条
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                  position = position_dodge(width = 0.8),
                  width = 0.25, linewidth = 0.6, color = "black") +
    
    # 合并标签 - 增大字体并加粗
    geom_text(
      aes(
        label = paste0(prob_label, "\n", ci_label),
        y = asymp.UCL + label_offset,
        group = urban_rural
      ),
      position = position_dodge(width = 0.8),  # 用这个来分开城市/农村的柱子
      size = 4.0,
      color = "black",
      angle = 45,
      hjust = 0,        # 左对齐
      vjust = 0.5,
      lineheight = 0.9
    ) +
    
    # 颜色和比例尺
    scale_fill_jco(name = "Urban-Rural Classification") +
    scale_y_continuous(
      limits = y_range,
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    
    # 标签和标题
    labs(
      title = plot_title,
      x = NULL,
      y = "Predicted Value (95% CI)"
    ) +
    
    # 主题设置 - 增大轴文字并加粗
    theme_classic(base_size = 13) +  # 基础字体从12增大到13
    theme(
      legend.position = "top",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题从14增大到16
      axis.text.x = element_text(color = "black", size = 13, face = "bold"),  # x轴文字从12增大到13并加粗
      axis.text.y = element_text(color = "black", size = 13, face = "bold"),  # y轴刻度文字加粗
      axis.title.x = element_text(size = 14, face = "bold"),  # x轴标题加粗
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),  # y轴标题从12增大到14并加粗
      legend.title = element_text(face = "bold", size = 13),  # 图例标题加粗
      legend.text = element_text(face = "bold", size = 12)  # 图例文字加粗
    )
}

# 创建两个子图
prevalence_plot <- create_plot(filter(plot_data, type == "Prevalence"), "Disease Prevalence") +
  labs(title = "Predicted Prevalence by CVD Risk Factors")

awareness_plot <- create_plot(filter(plot_data, type == "Awareness"), "Disease Awareness") +
  labs(title = "Predicted Awareness by CVD Risk Factors",
       x = "Disease Type")

# 组合图表
final_plot <- prevalence_plot / awareness_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.justification = "center")

# 显示和保存图表
print(final_plot)
ggsave("disease_prevalence_awareness.tiff", 
       plot = final_plot,
       width = 8.5, height = 11,  # 稍微增大画布尺寸以适应更大的字体
       dpi = 600, 
       compression = "lzw")




# 创建两个子图 (保持其他部分不变)
prevalence_plot <- create_plot(filter(plot_data, type == "Prevalence"), "Disease Prevalence") +
  labs(title = "Predicted Prevalence by CVD Risk Factors")

awareness_plot <- create_plot(filter(plot_data, type == "Awareness"), "Disease Awareness") +
  labs(title = "Predicted Awareness by CVD Risk Factors",
       x = "Disease Type")  # 只在下方图显示x轴标签

# 组合图表 (保持其他部分不变)
final_plot <- prevalence_plot / awareness_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.justification = "center")

# 显示和保存图表 (保持其他部分不变)
print(final_plot)
ggsave("disease_prevalence_awareness.tiff", 
       plot = final_plot,
       width = 8, height = 10, 
       dpi = 600, 
       compression = "lzw")
