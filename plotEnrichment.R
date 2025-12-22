plotEnrichment <- function(
    pathway, 
    stats, 
    gseaParam = 1, 
    ticksSize = 0.2,
    # 新增可定制参数，提升灵活性
    curve_color = "#2E8B57",    # 富集曲线颜色（深绿，更专业）
    ticks_color = "#696969",    # 基因tick颜色（深灰，不刺眼）
    pos_es_color = "#E64B35",   # 正ES虚线颜色（砖红）
    neg_es_color = "#3498DB",   # 负ES虚线颜色（天蓝）
    es_linewidth = 1.2,         # 富集曲线粗细
    ticks_alpha = 0.7,          # tick透明度
    axis_text_size = 10,        # 坐标轴文字大小
    axis_title_size = 12,       # 坐标轴标题大小
    plot_title = NULL,          # 自定义图标题
    title_size = 14             # 标题大小
) {
  # 计算富集分析绘图数据
  pd <- plotEnrichmentData(
    pathway = pathway, 
    stats = stats, 
    gseaParam = gseaParam
  )
  
  # 提取关键数值（用于标注）
  pos_es <- pd$posES
  neg_es <- pd$negES
  max_rank <- max(pd$curve$rank)
  
  # 构建绘图对象（避免with()，提高代码可读性）
  p <- ggplot() +
    # 1. 绘制富集曲线（核心元素，加粗突出）
    geom_line(
      data = pd$curve,
      aes(x = rank, y = ES),
      color = curve_color,
      linewidth = es_linewidth
    ) +
    # 2. 绘制基因位置tick（调整透明度，避免杂乱）
    
    # 3. 绘制正ES虚线（区分颜色，更易识别）
    geom_hline(
      yintercept = pos_es,
      colour = pos_es_color,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    # 4. 绘制负ES虚线（区分颜色）
    geom_hline(
      yintercept = neg_es,
      colour = neg_es_color,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    # 5. 绘制0基线（加粗，突出参考）
    geom_hline(
      yintercept = 0,
      colour = "black",
      linewidth = 0.6
    ) +
    # 6. 添加ES数值标注（直观显示关键值）
    annotate(
      "text",
      x = max_rank * 0.9,
      y = pos_es,
      label = sprintf("Pos ES: %.3f", pos_es),
      color = pos_es_color,
      size = 3.5,
      hjust = 1
    ) +
    annotate(
      "text",
      x = max_rank * 0.9,
      y = neg_es,
      label = sprintf("Neg ES: %.3f", neg_es),
      color = neg_es_color,
      size = 3.5,
      hjust = 1
    ) +
    # 7. 坐标轴标签（更具描述性）
    labs(
      x = "Gene Rank in Ordered Dataset",
      y = "Enrichment Score (ES)",
      title = plot_title
    ) 
    # 8. 主题优化（专业简洁，符合SCI图表风格）
    #theme_bw() +
    
  
  # 返回绘图对象
  return(p)
}
