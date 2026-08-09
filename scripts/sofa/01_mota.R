# chuẩn bị thư viện
library(tidyverse)
install.packages("pROC")
library(pROC)
# Kiểm tra NA
m %>%
  summarise(
    n = n(),
    death = sum(event28 == 1, na.rm = TRUE),
    mortality = mean(event28 == 1, na.rm = TRUE) * 100,
    missing_event28 = sum(is.na(event28)),
    missing_sofa = sum(is.na(sofa)),
    missing_sofa2 = sum(is.na(sofa2))
  )
# Mô tả 2 sofa
m %>%
  summarise(
    sofa_median = median(sofa, na.rm = TRUE),
    sofa_Q1 = quantile(sofa, 0.25, na.rm = TRUE),
    sofa_Q3 = quantile(sofa, 0.75, na.rm = TRUE),

    sofa2_median = median(sofa2, na.rm = TRUE),
    sofa2_Q1 = quantile(sofa2, 0.25, na.rm = TRUE),
    sofa2_Q3 = quantile(sofa2, 0.75, na.rm = TRUE)
  )
# histogram sofa
ggplot(m, aes(x = sofa)) + 
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
    title = "Phân phối SOFA score",
    x = "SOFA score",
    y = "Mật độ"
  )
# histogram sofa-2
ggplot(m, aes(x = sofa2)) + 
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
    title = "Phân phối SOFA-2 score",
    x = "SOFA-2 score",
    y = "Mật độ"
  )
