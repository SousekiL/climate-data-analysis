library(ggthemes)
library(glue)
library(ggtext)
source('~/Documents/R/ClimateR/main/_precipitation.R')
source('~/Documents/R/ClimateR/main/_temper.R')
source('~/Documents/R/ClimateR/main/_pressure.R')
source('~/Documents/R/ClimateR/main/_humidity.R')
setwd('/Users/sousekilyu/Documents/R/ClimateR')
library(showtext)

font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()
showtext_auto()  # 全局自动使用

## data
merge_df <- unique(left_join(toolik_df, temp_df)) %>%
  left_join(shum_df) %>%
  left_join(pres_df) %>%
  left_join(unique(dplyr::select(city_loc_list, addr, lon, lat)), by = join_by(city == addr)) %>%
  mutate(
    city_abr = case_when(
      city == '新疆乌鲁木齐市' ~ '乌鲁木齐',
      city == '黑龙江省哈尔滨市' ~ '哈尔滨',
      city == '陕西省西安市' ~ '西安',
      city == '北京市' ~ '北京',
      city == '湖北省武汉市' ~ '武汉',
      city == '上海市' ~ '上海',
      city == '西藏拉萨市' ~ '拉萨',
      city == '福建省福州市' ~ '福州',
      city == '云南省昆明市' ~ '昆明',
      city == '广东省广州市' ~ '广州',
      city == '四川省成都市' ~ '成都',
      city == '山东省青岛市' ~ '青岛',
      city == '甘肃省兰州市' ~ '兰州',
      city == '河南省郑州市' ~ '郑州'
    ),
    city_abr =  factor(
      city_abr,
      levels = c(
        '乌鲁木齐',
        '哈尔滨',
        '兰州',
        '西安',
        '郑州',
        '北京',
        '青岛',
        '成都',
        '武汉',
        '上海',
        '拉萨',
        '福州',
        '昆明',
        '广州'
      )
    ),
    city_loc = as.character(glue("{city_abr}\n{loc}"))
  )
# plot
## color
color_list <- c(
  "#6794a7",
  "#014d64",
  "#76c0c1",
  "#01a2d9",
  "#7ad2f6",
  "#00887d",
  "#adadad",
  "#7bd3f6",
  "#7c260b",
  "#ee8f71",
  "#76c0c1",
  "#a18376"
)
colors <- ggthemes::ggthemes_data[["economist"]][["fg"]]

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
    strip.text = element_text(size = 60,
                              #hjust = 0,
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

## 温度和降水
ylim.prec <- c(
  min(toolik_df$precipitation_s, na.rm = TRUE),
  max(toolik_df$precipitation_s, na.rm = TRUE)
)    # in this example, temperature
ylim.temp <- c(min(temp_df$temper_s, na.rm = TRUE),
               max(temp_df$temper_s, na.rm = TRUE))   # in this example, precipitation
b <- diff(ylim.prec) / diff(ylim.temp)
a <- ylim.prec[1] - b * ylim.temp[1]

ggplot(data = merge_df) +
  geom_bar(
    stat = "identity",
    aes(x = day,
        y = precipitation_s,
        group = 1),
    color = "#014d64",
    alpha = .5
  ) +
  geom_line(aes(x = day,
                y = temper_s * b + a,
                group = 1),
            color = "#7c260b",
            lwd = 1) +
  # https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
  scale_y_continuous(name = "降水量(mm hr-1)",
                     sec.axis = sec_axis( ~ (. - a) / b, name = "气温(℃)")) +
  scale_x_date(date_labels = "%b") +
  # geom_text(aes(x=ymd("2018-09-01"), y=1.8, label = loc),
  #           color="black", size = 20, hjust = 0, family = 'Canger'
  # ) +
  #geom_smooth(method = "loess") +
  #stat_smooth(aes(x = year, y = precipitation), method = lm, formula = y ~ poly(x, 20), se = FALSE) +
  # scale_colour_manual(values = c("#E47250",  "#5A4A6F", "#EBB261", "#9D5A6C",
  #                                "#E47250",  "#5A4A6F", "#EBB261", "#9D5A6C")) +
  facet_wrap(. ~ city_abr + loc, ncol = 2) +
  guides(color = FALSE) +
  labs(x = "日期",
       title = "Fig 1. <span style = 'color:darkred'>**主要城市**</span>2018年分日降水量、气温",
       caption = "Source: 阳坤, 何杰等. (2019). 中国区域地面气象要素驱动数据集（1979-2018）. 时空三极环境大数据平台."
  ) +     # Set title
  theme_economist_white() +
  theme_define +
  theme(plot.caption = element_markdown(size = 40,
                                        hjust = 0,
                                        margin = margin(t = 20)))

ggsave(
  #tm,
  "plot/temper_eco.png",
  dpi = 300,
  width = 12,
  height = 28
)


## 湿度
ggplot(data = merge_df) +
  geom_area(
    aes(x = day,
        y = humidity_s,
        group = 1),
    color = "#014d64",
    fill = "#014d64",
    alpha = .5
  ) +
  scale_x_date(date_labels = "%b") +
  facet_wrap(. ~ city_abr + loc, ncol = 2) +
  guides(color = FALSE) +
  labs(
    x = "日期",
    y = '湿度(kg kg-1)',
    title = "Fig 1. <span style = 'color:darkred'>**主要城市**</span>2018年分日湿度",
    caption = "Source: 阳坤, 何杰等. (2019). 中国区域地面气象要素驱动数据集（1979-2018）. 时空三极环境大数据平台."
  ) +     # Set title
  theme_economist_white() +
  theme_define +
  theme(plot.caption = element_markdown(size = 40,
                                        hjust = 0,
                                        margin = margin(t = 20)))

ggsave(
  #tm,
  "plot/humidity_eco.png",
  dpi = 300,
  width = 12,
  height = 28
)



## 气压
ggplot(data = merge_df) +
  geom_line(
    aes(x = day,
        y = pres_s,
        group = 1),
    color = "darkred",
    lwd = 1,
    alpha = .5
  ) +
  scale_x_date(date_labels = "%b") +
  #scale_y_continuous(limits = c(80000, 110000)) +
  facet_wrap(. ~ city_abr + loc, ncol = 2, scales = "free") +
  guides(color = FALSE) +
  labs(
    x = "日期",
    y = '气压(Pa)',
    title = "Fig 1. <span style = 'color:darkred'>**主要城市**</span>2018年分日湿度",
    caption = "Source: 阳坤, 何杰等. (2019). 中国区域地面气象要素驱动数据集（1979-2018）. 时空三极环境大数据平台."
  ) +     # Set title
  theme_economist_white() +
  theme_define +
  theme(plot.caption = element_markdown(size = 40,
                                        hjust = 0,
                                        margin = margin(t = 20)))

ggsave(
  #tm,
  "plot/pressure_fixed_eco.png",
  dpi = 300,
  width = 12,
  height = 28
)


### 全国地图
library(hexbin)

hb <- hexbin::hexbin(shum_df_all_year$lon,
                     shum_df_all_year$lat,
                     xbins = 10,
                     IDs = TRUE)
hb_df <- hexTapply(hb, shum_df_all_year$humidity, mean, na.rm = TRUE)
.temp <- data.frame(lon = hb@xcm,
                    lat = hb@ycm,
                    value = hb_df)

ggplot(shum_df_all_year, mapping = aes(x = lon, y = lat, z = humidity)) +
  stat_summary_hex(fun = mean, bins = 10) +
  scale_fill_viridis_c(option = "magma") +
  theme_economist_white() +
  theme_define +
  theme(aspect.ratio = 10 / 10)
ggsave(#tm,
  "plot/hex.png",
  dpi = 300,
  width = 12,
  height = 12)





















showtext_auto(FALSE) # 不在需要就关闭
