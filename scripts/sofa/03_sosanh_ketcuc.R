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
