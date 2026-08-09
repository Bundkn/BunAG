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
