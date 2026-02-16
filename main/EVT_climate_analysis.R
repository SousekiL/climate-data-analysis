# =============================================================================
# 基于 CMFD 气候数据的极值分析（EVT）—— 14 城市比较研究
# Extreme Value Analysis of Daily Climate Data (2018)
# 方法：POT (Peaks Over Threshold) + GPD (Generalized Pareto Distribution)
# =============================================================================

# --- 0. 加载包 ---------------------------------------------------------------
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(lubridate)
library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggtext)
library(glue)
library(showtext)
library(extRemes)   # EVT 核心包：GEV / GPD 拟合
library(ggthemes)
library(patchwork)  # 拼图

# 如未安装：
# install.packages(c("extRemes", "patchwork"))

# --- 字体设置 ----------------------------------------------------------------
font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
showtext_auto()

# --- 1. 数据读取 —— 复用你现有的 NetCDF 处理流程 ----------------------------
setwd('/Users/sousekilyu/Documents/Github/climate-data-analysis')

# 城市坐标
city_loc <- fread('data/全国县级以上地名代码及经纬度2.csv')
city_loc %<>%
  rename(lon = `东经`, lat = `北纬`, addr = `地名`) %>%
  mutate(lat2 = paste0(format(round(lat, 1), nsmall = 1), "°N"),
         lon2 = paste0(format(round(lon, 1), nsmall = 1), "°E"),
         loc  = paste0("(", lat2, ", ", lon2, ")")) %>%
  unique()

city_loc_list <- city_loc %>%
  filter(addr %in% c(
    '北京市', '上海市', '广东省广州市', '陕西省西安市',
    '新疆乌鲁木齐市', '黑龙江省哈尔滨市', '云南省昆明市',
    '湖北省武汉市', '四川省成都市', '山东省青岛市',
    '西藏拉萨市', '福建省福州市', '甘肃省兰州市', '河南省郑州市'
  )) %>%
  unique()

# 通用函数：从 NetCDF 提取各城市日序列
extract_city_data <- function(nc_file, var_name) {
  nc <- nc_open(nc_file)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  arr <- ncvar_get(nc, var_name)
  nc_close(nc)

  r_brick <- brick(arr,
                   xmn = min(lat), xmx = max(lat),
                   ymn = min(lon), ymx = max(lon),
                   crs = CRS("+proj=longlat +datum=WGS84"))
  r_brick <- flip(t(r_brick), direction = 'y')

  result <- data.frame()
  for (i in seq_len(nrow(city_loc_list))) {
    series <- raster::extract(
      r_brick,
      SpatialPoints(cbind(city_loc_list$lon[i], city_loc_list$lat[i])),
      method = 'simple'
    )
    df <- data.frame(
      day   = seq(ymd('2018-01-01'), ymd('2018-12-31'), by = '1 day'),
      value = as.numeric(t(series)),
      city  = city_loc_list$addr[i]
    )
    result <- rbind(result, df)
  }
  return(result)
}

# 读取四个变量
prec_df <- extract_city_data('data/prec_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc', 'prec')
temp_df <- extract_city_data('data/temp_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc', 'temp')
shum_df <- extract_city_data('data/shum_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc', 'shum')
pres_df <- extract_city_data('data/pres_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc', 'pres')

# 变量命名 & 单位转换
names(prec_df)[2] <- "precipitation"          # mm/hr
names(temp_df)[2] <- "temperature"
temp_df$temperature <- temp_df$temperature - 273.15  # K → ℃
names(shum_df)[2] <- "humidity"               # kg/kg
names(pres_df)[2] <- "pressure"               # Pa

# 合并
climate_df <- prec_df %>%
  left_join(temp_df, by = c("day", "city")) %>%
  left_join(shum_df, by = c("day", "city")) %>%
  left_join(pres_df, by = c("day", "city")) %>%
  mutate(
    city_abr = case_when(
      city == '新疆乌鲁木齐市'   ~ '乌鲁木齐',
      city == '黑龙江省哈尔滨市' ~ '哈尔滨',
      city == '陕西省西安市'     ~ '西安',
      city == '北京市'           ~ '北京',
      city == '湖北省武汉市'     ~ '武汉',
      city == '上海市'           ~ '上海',
      city == '西藏拉萨市'       ~ '拉萨',
      city == '福建省福州市'     ~ '福州',
      city == '云南省昆明市'     ~ '昆明',
      city == '广东省广州市'     ~ '广州',
      city == '四川省成都市'     ~ '成都',
      city == '山东省青岛市'     ~ '青岛',
      city == '甘肃省兰州市'     ~ '兰州',
      city == '河南省郑州市'     ~ '郑州'
    ),
    city_abr = factor(city_abr, levels = c(
      '乌鲁木齐', '哈尔滨', '兰州', '西安', '郑州', '北京', '青岛',
      '成都', '武汉', '上海', '拉萨', '福州', '昆明', '广州'
    )),
    month = month(day)
  ) %>%
  unique()

# =============================================================================
# --- 2. 探索性分析：识别极值特征 --------------------------------------------
# =============================================================================

# 公共配色（来自 _climate_NCdata.R）
color_list <- c(
  "#6794a7", "#014d64", "#76c0c1", "#01a2d9", "#7ad2f6", "#00887d",
  "#adadad", "#7bd3f6", "#7c260b", "#ee8f71", "#76c0c1", "#a18376"
)

# 公共主题（与 _climate_NCdata.R 中 theme_define 完全一致）
theme_define <-
  theme(
    aspect.ratio = 10 / 16,
    text = element_text(family = 'Canger'),
    panel.background = element_rect(fill = "#EBEBEB"),
    strip.background = element_rect(fill = "#EBEBEB"),
    plot.title = element_textbox_simple(
      size = 100,
      hjust = 0,
      face = "bold",
      margin = margin(t = 20, b = 40)
    ),
    plot.subtitle = element_text(size = 60, color = "grey40",
                                 margin = margin(b = 20)),
    strip.text = element_text(size = 60,
                              margin = margin(t = 10, b = 5)),
    legend.text = element_text(size = 60),
    legend.title = element_text(size = 60),
    axis.text.x = element_text(size = 60),
    axis.text.y = element_text(size = 60),
    axis.title.x = element_text(size = 80,
                                margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(size = 80,
                                margin = margin(l = 10, r = 20)),
    axis.title.y.right = element_text(size = 80,
                                      margin = margin(l = 20, r = 10))
  )

caption_text <- "Source: 阳坤, 何杰等. (2019). 中国区域地面气象要素驱动数据集（1979-2018）. 时空三极环境大数据平台."

# --- Fig 1：各城市日降水量分布（箱线图 + 极端值标注）
p1 <- ggplot(climate_df, aes(x = city_abr, y = precipitation)) +
  geom_boxplot(aes(fill = city_abr), outlier.color = "#7c260b",
               outlier.size = 2, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = color_list[1:14]) +
  labs(x = NULL, y = "降水量(mm hr-1)",
       title = "Fig 1. 各城市日降水量分布与<span style = 'color:darkred'>**极端值**</span>",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(axis.text.x = element_text(size = 60, angle = 45, hjust = 1),
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig1_precipitation_boxplot.png", p1,
       dpi = 300, width = 14, height = 10)
print(p1)

# --- Fig 2：各城市日均气温分布
p2 <- ggplot(climate_df, aes(x = city_abr, y = temperature)) +
  geom_boxplot(aes(fill = city_abr), outlier.color = "#7c260b",
               outlier.size = 2, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = color_list[1:14]) +
  labs(x = NULL, y = "气温(℃)",
       title = "Fig 2. 各城市日均气温分布 —— 识别<span style = 'color:darkred'>**极端高温**</span>与低温事件",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(axis.text.x = element_text(size = 60, angle = 45, hjust = 1),
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig2_temperature_boxplot.png", p2,
       dpi = 300, width = 14, height = 10)
print(p2)

# =============================================================================
# --- 3. POT 极值分析 (Peaks Over Threshold) ---------------------------------
#     分析对象：日降水量 —— 气候极值分析最经典的变量
#     方法：选取超过高阈值的降水事件，拟合 GPD 分布
# =============================================================================

# --- 3.1 阈值选择 -----------------------------------------------------------
# 使用各城市降水量的 90th 分位数作为阈值（只考虑有降水的日子）

threshold_df <- climate_df %>%
  filter(precipitation > 0) %>%
  group_by(city_abr) %>%
  summarise(
    q90 = quantile(precipitation, 0.90, na.rm = TRUE),
    q95 = quantile(precipitation, 0.95, na.rm = TRUE),
    mean_prec = mean(precipitation, na.rm = TRUE),
    max_prec  = max(precipitation, na.rm = TRUE),
    n_rainy   = n(),
    .groups = "drop"
  )
print(threshold_df)

# --- 3.2 Mean Residual Life Plot（平均超额寿命图）—— 辅助阈值选择
# 以广州和北京为例展示
mrl_data <- list()
for (city_name in c("广州", "北京", "武汉", "哈尔滨")) {
  x <- climate_df %>%
    filter(city_abr == city_name, precipitation > 0) %>%
    pull(precipitation)
  thresholds <- seq(quantile(x, 0.5), quantile(x, 0.95), length.out = 50)
  mrl <- sapply(thresholds, function(u) mean(x[x > u] - u))
  mrl_data[[city_name]] <- data.frame(
    threshold = thresholds,
    mean_excess = mrl,
    city = city_name
  )
}
mrl_df <- bind_rows(mrl_data)

p_mrl <- ggplot(mrl_df, aes(x = threshold, y = mean_excess)) +
  geom_line(color = "#014d64", linewidth = 1) +
  geom_point(color = "#7c260b", size = 2) +
  facet_wrap(~city, scales = "free", ncol = 2) +
  labs(x = "阈值(mm hr-1)", y = "平均超额",
       title = "Fig 3. <span style = 'color:darkred'>**平均超额寿命图**</span> (Mean Residual Life Plot)",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig3_mrl_plot.png", p_mrl,
       dpi = 300, width = 12, height = 12)
print(p_mrl)

# --- 3.3 GPD 拟合：对每个城市进行 POT 分析 ----------------------------------

gpd_results <- list()

for (city_name in levels(climate_df$city_abr)) {
  x <- climate_df %>%
    filter(city_abr == city_name) %>%
    pull(precipitation)

  # 阈值：有降水日的 90th 分位数
  x_pos <- x[x > 0 & !is.na(x)]
  if (length(x_pos) < 20) next

  u <- quantile(x_pos, 0.90)
  exceedances <- x_pos[x_pos > u]

  if (length(exceedances) < 5) next

  # 拟合 GPD
  fit <- tryCatch({
    fevd(x_pos, threshold = u, type = "GP", method = "MLE")
  }, error = function(e) NULL)

  if (is.null(fit)) next

  # 提取参数
  params <- fit$results$par
  sigma_hat <- params["scale"]
  xi_hat    <- params["shape"]

  # 超阈值率
  lambda <- length(exceedances) / length(x)  # 365 天中的比例

  # 计算重现水平 (Return Levels)
  # 对于 T 年 (这里 T 用"等效年"概念, 1年 = 365天)
  return_periods <- c(1, 2, 5, 10)  # 年
  return_levels <- sapply(return_periods, function(T_year) {
    m <- T_year * 365  # 总观测数
    p <- 1 / m
    if (abs(xi_hat) < 1e-6) {
      u + sigma_hat * log(lambda / p)
    } else {
      u + (sigma_hat / xi_hat) * ((lambda / p)^xi_hat - 1)
    }
  })

  gpd_results[[city_name]] <- data.frame(
    city     = city_name,
    threshold = u,
    n_exceed = length(exceedances),
    sigma    = sigma_hat,
    xi       = xi_hat,
    rl_1yr   = return_levels[1],
    rl_2yr   = return_levels[2],
    rl_5yr   = return_levels[3],
    rl_10yr  = return_levels[4],
    stringsAsFactors = FALSE
  )
}

gpd_table <- bind_rows(gpd_results)
rownames(gpd_table) <- NULL
print(gpd_table)

# --- 3.4 可视化：GPD 参数城市间比较 ------------------------------------------

gpd_long <- gpd_table %>%
  select(city, rl_1yr, rl_2yr, rl_5yr, rl_10yr) %>%
  pivot_longer(-city, names_to = "return_period", values_to = "level") %>%
  mutate(
    return_period = factor(return_period,
                           levels = c("rl_1yr", "rl_2yr", "rl_5yr", "rl_10yr"),
                           labels = c("1年", "2年", "5年", "10年")),
    city = factor(city, levels = gpd_table$city[order(gpd_table$rl_10yr)])
  )

p3 <- ggplot(gpd_long, aes(x = city, y = level, fill = return_period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("#6794a7", "#01a2d9", "#ee8f71", "#7c260b"),
                    name = "重现期") +
  labs(x = NULL, y = "重现水平(mm hr-1)",
       title = "Fig 4. <span style = 'color:darkred'>**极端降水**</span>重现水平 —— 城市比较",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(axis.text.x = element_text(size = 60, angle = 45, hjust = 1),
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig4_return_levels.png", p3,
       dpi = 300, width = 14, height = 10)
print(p3)

# --- 3.5 形状参数 ξ 比较（反映尾部厚度 / 极端风险大小）
p4 <- ggplot(gpd_table, aes(x = reorder(city, xi), y = xi)) +
  geom_bar(stat = "identity", fill = "#014d64", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "#7c260b", linetype = "dashed", linewidth = 1) +
  labs(x = NULL, y = "形状参数 ξ",
       title = "Fig 5. GPD形状参数ξ —— <span style = 'color:darkred'>**极端降水**</span>尾部风险比较",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(axis.text.x = element_text(size = 60, angle = 45, hjust = 1),
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig5_shape_parameter.png", p4,
       dpi = 300, width = 14, height = 10)
print(p4)

# =============================================================================
# --- 4. 极端高温分析（POT）—— 城市比较 --------------------------------------
# =============================================================================

# 阈值：各城市气温的 95th 分位数（识别极端高温日）
temp_gpd_results <- list()

for (city_name in levels(climate_df$city_abr)) {
  x <- climate_df %>%
    filter(city_abr == city_name) %>%
    pull(temperature)

  x <- x[!is.na(x)]
  if (length(x) < 30) next

  u <- quantile(x, 0.95)
  exceedances <- x[x > u]
  if (length(exceedances) < 5) next

  fit <- tryCatch({
    fevd(x, threshold = u, type = "GP", method = "MLE")
  }, error = function(e) NULL)

  if (is.null(fit)) next

  params <- fit$results$par
  sigma_hat <- params["scale"]
  xi_hat    <- params["shape"]
  lambda    <- length(exceedances) / length(x)

  return_periods <- c(1, 2, 5, 10)
  return_levels <- sapply(return_periods, function(T_year) {
    m <- T_year * 365
    p <- 1 / m
    if (abs(xi_hat) < 1e-6) {
      u + sigma_hat * log(lambda / p)
    } else {
      u + (sigma_hat / xi_hat) * ((lambda / p)^xi_hat - 1)
    }
  })

  temp_gpd_results[[city_name]] <- data.frame(
    city      = city_name,
    threshold = u,
    n_exceed  = length(exceedances),
    sigma     = sigma_hat,
    xi        = xi_hat,
    rl_1yr    = return_levels[1],
    rl_2yr    = return_levels[2],
    rl_5yr    = return_levels[3],
    rl_10yr   = return_levels[4],
    stringsAsFactors = FALSE
  )
}

temp_gpd_table <- bind_rows(temp_gpd_results)
rownames(temp_gpd_table) <- NULL
print(temp_gpd_table)

# 极端高温重现水平比较
temp_gpd_long <- temp_gpd_table %>%
  select(city, rl_1yr, rl_2yr, rl_5yr, rl_10yr) %>%
  pivot_longer(-city, names_to = "return_period", values_to = "level") %>%
  mutate(
    return_period = factor(return_period,
                           levels = c("rl_1yr", "rl_2yr", "rl_5yr", "rl_10yr"),
                           labels = c("1年", "2年", "5年", "10年")),
    city = factor(city, levels = temp_gpd_table$city[order(temp_gpd_table$rl_10yr)])
  )

p5 <- ggplot(temp_gpd_long, aes(x = city, y = level, fill = return_period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("#6794a7", "#01a2d9", "#ee8f71", "#7c260b"),
                    name = "重现期") +
  labs(x = NULL, y = "重现水平(℃)",
       title = "Fig 6. <span style = 'color:darkred'>**极端高温**</span>重现水平 —— 城市比较",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(axis.text.x = element_text(size = 60, angle = 45, hjust = 1),
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig6_temp_return_levels.png", p5,
       dpi = 300, width = 14, height = 10)
print(p5)

# =============================================================================
# --- 5. 综合风险评分 —— 多维极值比较 ----------------------------------------
# =============================================================================

# 将降水和温度的极值风险归一化后综合评分
risk_df <- gpd_table %>%
  select(city, prec_rl10 = rl_10yr, prec_xi = xi) %>%
  left_join(
    temp_gpd_table %>% select(city, temp_rl10 = rl_10yr, temp_xi = xi),
    by = "city"
  ) %>%
  mutate(
    prec_risk = (prec_rl10 - min(prec_rl10)) / (max(prec_rl10) - min(prec_rl10)),
    temp_risk = (temp_rl10 - min(temp_rl10)) / (max(temp_rl10) - min(temp_rl10)),
    composite = 0.5 * prec_risk + 0.5 * temp_risk
  )

p6 <- ggplot(risk_df, aes(x = prec_risk, y = temp_risk)) +
  geom_point(aes(size = composite, color = composite), alpha = 0.8) +
  geom_text(aes(label = city), vjust = -1.5, family = "Canger", size = 20) +
  scale_color_gradient(low = "#6794a7", high = "#7c260b", name = "综合风险") +
  scale_size_continuous(range = c(5, 18), guide = "none") +
  labs(x = "极端降水风险(归一化)",
       y = "极端高温风险(归一化)",
       title = "Fig 7. 城市气候极值<span style = 'color:darkred'>**综合风险**</span>评估",
       caption = caption_text) +
  theme_economist_white() +
  theme_define +
  theme(aspect.ratio = 10 / 10,
        plot.caption = element_markdown(size = 40, hjust = 0,
                                        margin = margin(t = 20)))

ggsave("plot/evt_fig7_risk_scatter.png", p6,
       dpi = 300, width = 12, height = 12)
print(p6)

# =============================================================================
# --- 6. GPD 拟合诊断图 —— 以广州为例 ----------------------------------------
# =============================================================================

x_gz <- climate_df %>%
  filter(city_abr == "广州") %>%
  pull(precipitation)
x_gz <- x_gz[x_gz > 0 & !is.na(x_gz)]
u_gz <- quantile(x_gz, 0.90)

fit_gz <- fevd(x_gz, threshold = u_gz, type = "GP", method = "MLE")

png("plot/evt_fig8_gpd_diagnostic_guangzhou.png",
    width = 10, height = 8, units = "in", res = 300)
par(family = "Canger", mfrow = c(2, 2))
plot(fit_gz, main = "广州极端降水 GPD 拟合诊断")
dev.off()

# =============================================================================
# --- 7. 汇总表输出 -----------------------------------------------------------
# =============================================================================

cat("\n========== 极端降水 GPD 拟合结果 ==========\n")
print(gpd_table %>%
        mutate(across(where(is.numeric), ~round(., 4))) %>%
        arrange(desc(rl_10yr)),
      n = 20)

cat("\n========== 极端高温 GPD 拟合结果 ==========\n")
print(temp_gpd_table %>%
        mutate(across(where(is.numeric), ~round(., 4))) %>%
        arrange(desc(rl_10yr)),
      n = 20)

cat("\n========== 综合风险评分 ==========\n")
print(risk_df %>%
        mutate(across(where(is.numeric), ~round(., 4))) %>%
        arrange(desc(composite)),
      n = 20)

showtext_auto(FALSE)

cat("\n分析完成！图表已保存至 plot/ 目录。\n")
