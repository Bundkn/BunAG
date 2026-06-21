# Kiểm tra cấu trúc dữ liệu
str()
summary()
class()
# Mô tả toàn bộ biến
install.packages("psych")
library(psych)
describe(m)

# Mô tả biến định tính: số lượng và tỷ lệ (%)
install.packages("janitor")
library(janitor)
tabyl(m$songsot)
# mô tả biến định tính = bảng
install.packages("gtsummary")
library(gtsummary)
m %>%
  select(tuoi, gioitinh, ag1, ag2, lac1, lac2, songsot) %>%
  tbl_summary(by = songsot)
# Kiểm tra phân phối chuẩn
install.packages("stats")
shapiro.test()
# Mô tả biến định lượng 
summary(m$cltm)
# So sánh
install.packages("compareGroups")
library(compareGroups)
createTable(compareGroups(death~ag1+ag2,data=m))
# So sánh biến định lượng 
t.test() #chuẩn
wilcox.test() #không chuẩn

# Tạo bảng mô tả theo phân phối
library(dplyr)
library(purrr)
library(tibble)

vars <- m %>%
  select(where(is.numeric)) %>%
  names()

summary_auto <- map_dfr(vars, function(v) {
  
  x <- m[[v]]
  x <- x[!is.na(x)]
  
  n <- length(x)
  n_unique <- n_distinct(x)
  
  # Shapiro test an toàn
  shapiro_p <- if(n >= 3 && n_unique > 1) {
    shapiro.test(x)$p.value
  } else {
    NA_real_
  }
  
  normal <- !is.na(shapiro_p) & shapiro_p > 0.05
  
  fmt <- function(z) round(z, 1)
  
  description <- case_when(
    
    n_unique <= 1 ~ paste0(fmt(x[1]), " (constant)"),
    
    normal ~ paste0(
      fmt(mean(x)), " ± ", fmt(sd(x))
    ),
    
    TRUE ~ paste0(
      fmt(median(x)), " (",
      fmt(quantile(x, 0.25)), "–",
      fmt(quantile(x, 0.75)), ")"
    )
  )
  
  tibble(
    variable = v,
    n = n,
    unique_value = n_unique,
    shapiro_p = round(shapiro_p, 4),
    distribution = case_when(
      is.na(shapiro_p) ~ "Không đánh giá được",
      normal ~ "Phân phối chuẩn",
      TRUE ~ "Không phân phối chuẩn"
    ),
    description = description
  )
})

print(summary_auto, n = Inf)

write_xlsx(summary_auto, "summary_auto.xlsx")

# Vẽ Histogram
library(ggplot2)
ggplot(m, aes(x = ag1)) + 
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "skyblue",
    color = "black"
  ) +
  geom_density(
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Phân phối Anion gap 1",
    x = "Anion gap lần 1",
    y = "Mật độ"
  )

# Vẽ violin
library(tidyr)
library(ggplot2)
## xoay trục
m_long <- m %>%
  pivot_longer(
    cols = c(ag1, ag2),
    names_to = "time",
    values_to = "ag"
  )
## plot (
ggplot(m_long, aes(x = time, y = ag)) + 
  
  geom_violin(
    fill = "#CFE8D6",
    color = "black",
    trim = FALSE,
    linewidth = 0.6
  ) +  
  
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "#4C9F70",
    color = "black"
  ) +
  
  geom_jitter(
    width = 0.08,
    alpha = 0.6,
    size = 1.8,
    shape = 21,
    fill = "red",
    color = "black"
  ) +
  
  scale_x_discrete(
    labels = c(
      ag1 = "Anion gap lần 1",
      ag2 = "Anion gap lần 2"
    )
  ) +
  
  theme_classic()
