# so sánh trên tử vong
tbl <- m %>%
  select(event28, sofa, sofa2) %>%
  tbl_summary(
    by = event28,
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) {median} ({p25} - {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels()

tbl

# ROC
roc_sofa <- roc(
  m$event28,
  m$sofa,
  ci = TRUE,
  quiet = TRUE
)

roc_sofa2 <- roc(
  m$event28,
  m$sofa2,
  ci = TRUE,
  quiet = TRUE
)
# AUC
auc(roc_sofa)
ci.auc(roc_sofa)
auc(roc_sofa2)
ci.auc(roc_sofa2)
# paired
roc.test(
  roc_sofa,
  roc_sofa2,
  paired = TRUE,
  method = "delong"
)
# PLOT ROC
install.packages("remotes")
remotes::install_github("cardiomoon/multipleROC")
library(multipleROC)
a=multipleROC(event28~sofa,data=m,plot=FALSE)
b=multipleROC(event28~sofa2,data=m,plot=FALSE)
plot_ROC(list(a,b),show.eta=FALSE,show.sens=FALSE)
# Hiển thị
plot_ROC(list(a,b),show.eta=TRUE,show.sens=TRUE)
# Tách plot
  plot_ROC(list(a,b),facet=TRUE)
  plot_ROC(list(a,b),show.eta=FALSE,show.sens=FALSE, facet=TRUE)

  library(pROC)
  library(ggplot2)
  library(dplyr)
  
  # Tính ROC
  roc1 <- roc(m$event28, m$sofa, quiet = TRUE)
  roc2 <- roc(m$event28, m$sofa2, quiet = TRUE)
  
  # Chuyển thành dữ liệu để vẽ
  roc_data <- bind_rows(
    data.frame(
      FPR = 1 - roc1$specificities,
      Sensitivity = roc1$sensitivities,
      Score = "SOFA"
    ),
    data.frame(
      FPR = 1 - roc2$specificities,
      Sensitivity = roc2$sensitivities,
      Score = "SOFA-2"
    )
  )
  
  # Vẽ ROC với màu tùy chỉnh
 p <- ggplot(roc_data, aes(x = FPR, y = Sensitivity, color = Score)) +
    geom_line(linewidth = 1.2) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", color = "grey60"
    ) +
    scale_color_manual(
      values = c(
        "SOFA" = "#4472C4",
        "SOFA-2" = "#C0504D"
      )
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    coord_fixed(ratio = 0.6) +
    labs(
      title = "ROC curves: SOFA vs SOFA-2",
      x = "1 - Specificity",
      y = "Sensitivity",
      color = NULL
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  ggsave(
    filename = "ROC_SOFA_SOFA2.png",
    plot = p,
    width = 9,
    height = 5.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
# CALIBRITION
  # cài gói
  install.packages("rms")
  library(rms)
  # tạo m_cal (3 biến)
  m_cal <- m %>%
    select(event28, sofa, sofa2) %>%
    drop_na()
  nrow(m_cal)
  table(m_cal$event28)
  # tạo mô hình
    dd <- datadist(m_cal)
    options(datadist = "dd")
    
    fit_sofa_rms <- lrm(
      event28 ~ sofa,
      data = m_cal,
      x = TRUE,
      y = TRUE
    )
    
    fit_sofa2_rms <- lrm(
      event28 ~ sofa2,
      data = m_cal,
      x = TRUE,
      y = TRUE
    )
  # bootstrap
  cal_sofa <- calibrate(
    fit_sofa_rms,
    method = "boot",
    B = 1000
  )
  
  cal_sofa2 <- calibrate(
    fit_sofa2_rms,
    method = "boot",
    B = 1000
  )
    # vẽ plot
      plot(
        cal_sofa,
        xlab = "Xác suất tử vong dự đoán",
        ylab = "Xác suất tử vong quan sát"
      )
      
      plot(
        cal_sofa2,
        xlab = "Xác suất tử vong dự đoán",
        ylab = "Xác suất tử vong quan sát"
      )
    # Tạo predicted probability
      m_cal$pred_sofa <- predict(
        fit_sofa_rms,
        type = "fitted"
      )
      
      m_cal$pred_sofa2 <- predict(
        fit_sofa2_rms,
        type = "fitted"
      )
    # Calibration intercept
      cal_intercept_sofa <- glm(
        event28 ~ 1,
        offset = qlogis(pred_sofa),
        data = m_cal,
        family = binomial
      )
      
      cal_intercept_sofa2 <- glm(
        event28 ~ 1,
        offset = qlogis(pred_sofa2),
        data = m_cal,
        family = binomial
      )
      
      coef(cal_intercept_sofa)
      coef(cal_intercept_sofa2)
    # slope 
      cal_slope_sofa <- glm(
        event28 ~ qlogis(pred_sofa),
        data = m_cal,
        family = binomial
      )
      
      cal_slope_sofa2 <- glm(
        event28 ~ qlogis(pred_sofa2),
        data = m_cal,
        family = binomial
      )
      
      coef(cal_slope_sofa)[2]
      coef(cal_slope_sofa2)[2]

# vẽ plot gộp
cal_plot <- bind_rows(
  data.frame(
    predicted = cal_sofa[, "predy"],
    observed = cal_sofa[, "calibrated.corrected"],
    score = "SOFA"
  ),
  data.frame(
    predicted = cal_sofa2[, "predy"],
    observed = cal_sofa2[, "calibrated.corrected"],
    score = "SOFA-2"
  )
)

ggplot(cal_plot, aes(x = predicted, y = observed, color = score)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "#6B7280",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_line(
    linewidth = 1.3
  ) +
  scale_color_manual(
    values = c(
      "SOFA" = "#3B82B6",
      "SOFA-2" = "#D95F5F"
    ),
    labels = c(
      "SOFA (MAE = 0.023)",
      "SOFA-2 (MAE = 0.042)"
    )
  ) +
  labs(
    x = "Predicted probability of 28-day mortality",
    y = "Observed probability of 28-day mortality",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )
