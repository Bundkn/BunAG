# Kaplan-Meier
install.packages("survminer")
library(survival)
library(survminer)
fit <- survfit(
  Surv(ngaydie, death) ~ agk1_group,
  data = m
)
# Vẽ biểu đồ
ggsurvplot(
  fit,
  data = m,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  xlab = "Days",
  ylab = "Survival probability",
  legend.title = "AGK group",
  legend.labs = c("Low AGK", "High AGK")
)
# Tạo nhóm theo cutoff ROC
m <- m %>%
  mutate(
    agk1_group = ifelse(
      agk1 >= 17.4,
      "High AGK",
      "Low AGK"
    )
  )
# Cách tìm cut-off
# === 1. pROC
install.packages("pROC")
library(pROC)
# tạo biến death
m$death <- ifelse(m$songsot == "0", 1, 0)
# Tạo đường cong ROC
roc_obj <- roc(
  response = m$death,
  predictor = m$agk1
)
#Tìm điểm cắt tối ưu
coords(
  roc_obj,
  "best",
  ret = c("threshold", "sensitivity", "specificity")
)

# === 2.maxstat
