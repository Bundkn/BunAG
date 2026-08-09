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
