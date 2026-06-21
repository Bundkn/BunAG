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
  
  # kiểm tra có đủ điều kiện chạy Shapiro không
  shapiro_p <- if(length(x) >= 3 && n_distinct(x) > 1) {
    shapiro.test(x)$p.value
  } else {
    NA_real_
  }
  
  normal <- !is.na(shapiro_p) & shapiro_p > 0.05
  
  tibble(
    variable = v,
    n = length(x),
    unique_value = n_distinct(x),
    shapiro_p = round(shapiro_p, 4),
    distribution = case_when(
      is.na(shapiro_p) ~ "Không đánh giá được",
      normal ~ "Phân phối chuẩn",
      TRUE ~ "Không phân phối chuẩn"
    ),
    description = case_when(
      
      is.na(shapiro_p) ~ paste0(
        "Median = ", median(x),
        " (giá trị hằng)"
      ),
      
      normal ~ paste0(
        round(mean(x), 2),
        " ± ",
        round(sd(x), 2)
      ),
      
      TRUE ~ paste0(
        round(median(x), 2),
        " (",
        round(quantile(x, 0.25), 2),
        "–",
        round(quantile(x, 0.75), 2),
        ")"
      )
    )
  )
})

print(summary_auto, n = Inf)
install.packages("writexl")
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
ggplot(m_long, aes(x = time, y = ag)) + 
  geom_violin(
    fill = "skyblue",
    color = "black",
    trim = FALSE
  ) +  
  geom_boxplot(
    width = 0.15,
    outlier.color = "red",
    outlier.size = 2
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.5
  ) + 
  scale_x_discrete(
    labels = c(
      ag1 = "Anion gap lần 1",
      ag2 = "Anion gap lần 2"
    )
  ) +
  labs(
    title = "Violin plot của Anion gap",
    x = "Thời điểm",
    y = "Anion gap"
  )
