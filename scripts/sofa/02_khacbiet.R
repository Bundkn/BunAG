# Tương quan Spearman
cor.test(
  m$sofa,
  m$sofa2,
  method = "spearman",
  exact = FALSE
)
# hoặc cách lấy Cl 
install.packages("Hmisc")
library(Hmisc)
rcorr(
  as.matrix(m[, c("sofa", "sofa2")]),
  type = "spearman"
)
# tạo biến khác biệt
m <- m %>%
  mutate(
    diff_sofa2_sofa = sofa2 - sofa,
    mean_sofa = (sofa + sofa2) / 2
  )
# mô tả khác biệt
mean_diff <- mean(m$diff_sofa2_sofa, na.rm = TRUE)
sd_diff <- sd(m$diff_sofa2_sofa, na.rm = TRUE)
loa_lower <- mean_diff - 1.96 * sd_diff
loa_upper <- mean_diff + 1.96 * sd_diff
mean_diff
loa_lower
loa_upper
sd_diff
# proportional bias
cor.test(
  m$mean_sofa,
  m$diff_sofa2_sofa,
  method = "spearman",
  exact = FALSE
)
# hồi quy difference
fit <- lm(diff_sofa2_sofa ~ mean_sofa, data = m)
summary(fit)
# PLOT SPEARMAN

# 1. Load packages
library(tidyverse)
library(ggplot2)

# 2. Chuẩn bị dữ liệu
dat <- m %>%
  select(sofa, sofa2) %>%
  drop_na() %>%
  filter(is.finite(sofa), is.finite(sofa2))

# 3. Tính tương quan Spearman
test <- cor.test(
  dat$sofa,
  dat$sofa2,
  method = "spearman",
  exact = FALSE
)

rho <- unname(test$estimate)
p_value <- test$p.value
n <- nrow(dat)

# 4. Bootstrap KTC 95% cho Spearman rho
set.seed(123)

B <- 2000

boot_rho <- replicate(B, {
  idx <- sample(seq_len(n), size = n, replace = TRUE)

  suppressWarnings(
    cor(dat$sofa[idx], dat$sofa2[idx],
        method = "spearman")
  )
})

# Loại các mẫu bootstrap không tính được tương quan
boot_rho <- boot_rho[is.finite(boot_rho)]

ci <- quantile(
  boot_rho,
  probs = c(0.025, 0.975),
  na.rm = TRUE
)

# 5. Nội dung chú thích trên hình
p_text <- if (p_value < 0.001) {
  "p < 0.001"
} else {
  paste0("p = ", format.pval(p_value, digits = 3))
}

annotation <- sprintf(
  "Spearman's rho = %.2f\n95%% CI: %.2f to %.2f\n%s\nn = %d",
  rho, ci[1], ci[2], p_text, n
)

# 6. Vẽ biểu đồ
p <- ggplot(dat, aes(x = sofa, y = sofa2)) +
  geom_point(
    color = "#4472C4",
    alpha = 0.65,
    size = 2.5,
    position = position_jitter(width = 0.08, height = 0.08)
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "#C0504D",
    linewidth = 0.9
  ) +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = annotation,
    hjust = 1.05, vjust = -0.3,
    size = 4,
    label.size = 0.25,
    fill = "white"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Correlation between SOFA and SOFA-2",
    x = "SOFA score",
    y = "SOFA-2 score"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(face = "bold")
  )

# 7. Hiển thị hình
print(p)

# 8. Xuất hình chất lượng cao
ggsave(
  "SOFA_SOFA2_Spearman.png",
  plot = p,
  width = 7,
  height = 6,
  dpi = 300,
  bg = "white"
)

# PLOT BLAND ALTMAN
library(ggplot2)
bias <- mean(
  m$diff_sofa2_sofa,
  na.rm = TRUE
)
sd_diff <- sd(
  m$diff_sofa2_sofa,
  na.rm = TRUE
)
loa_upper <- bias + 1.96 * sd_diff
loa_lower <- bias - 1.96 * sd_diff
fit <- lm(
  diff_sofa2_sofa ~ mean_sofa,
  data = m
)
p_prop <- summary(fit)$coefficients[
  "mean_sofa",
  "Pr(>|t|)"
]
# Bland–Altman plot
ggplot(
  m,
  aes(
    x = mean_sofa,
    y = diff_sofa2_sofa
  )
) +
  # Individual observations
  geom_point(
    color = "#557A95",
    alpha = 0.75,
    size = 2.7
  ) +
  # Regression line
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#C65D5D",
    linewidth = 0.9
  ) +
  # Mean bias
  geom_hline(
    yintercept = bias,
    color = "#1F4E5F",
    linewidth = 1.3
  ) +
  # Upper 95% LoA
  geom_hline(
    yintercept = loa_upper,
    color = "#E2A44F",
    linetype = "dashed",
    linewidth = 1
  ) +
  # Lower 95% LoA
  geom_hline(
    yintercept = loa_lower,
    color = "#E2A44F",
    linetype = "dashed",
    linewidth = 1
  ) +
  # Labels
  # Mean bias label
  annotate(
    "text",
    x = Inf,
    y = bias,
    label = paste0(
      "Mean bias = ",
      sprintf("%.2f", bias)
    ),
    hjust = 1.05,
    vjust = -0.7,
    color = "#1F4E5F",
    size = 3.8,
    fontface = "bold"
  ) +

  # Upper LoA label
  annotate(
    "text",
    x = Inf,
    y = loa_upper,
    label = paste0(
      "Upper 95% LoA = ",
      sprintf("%.2f", loa_upper)
    ),
    hjust = 1.05,
    vjust = -0.7,
    color = "#B77C25",
    size = 3.6
  ) +

  # Lower LoA label
  annotate(
    "text",
    x = Inf,
    y = loa_lower,
    label = paste0(
      "Lower 95% LoA = ",
      sprintf("%.2f", loa_lower)
    ),
    hjust = 1.05,
    vjust = 1.4,
    color = "#B77C25",
    size = 3.6
  ) +

  # Proportional bias label
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0(
      "Proportional bias: p = ",
      format.pval(
        p_prop,
        digits = 2,
        eps = 0.001
      )
    ),
    hjust = 1.05,
    vjust = 1.5,
    color = "#C65D5D",
    size = 3.8,
    fontface = "bold"
  ) +
  # Labels and theme
  labs(
    title = "Bland–Altman Analysis: SOFA vs SOFA-2",
    x = "Mean of SOFA and SOFA-2",
    y = "Difference (SOFA-2 − SOFA)"
  ) +

  coord_cartesian(
    clip = "off"
  ) +
  theme_classic(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    plot.margin = margin(
      10, 115, 10, 10
    )
  )
