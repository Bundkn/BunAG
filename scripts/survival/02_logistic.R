# 01. Đơn biến ----
install.packages("broom")
library(broom)
library(dplyr)
# chọn u_vars
u_vars <- c(
  "bmi","tuoi","gioitinh","sot","daubung",
  "daunguc","khotho","ho","tieuchay","tietnieu","vangda","tcyt","daukhop",
  "chuongbung","ankem","nonoi","sungdau","phu","tcnieu","tha","suytim","bmv",
  "rln","dtd","ckdc","ckdr","xogan",
  "bpm","ungthu","rlyt","crt","tst","hatt","hatr","nhietdo","rr",
  "spo2","wbc","neu","lym","hb","plt",
  "pt","inr","aptt","glu","ure","cre","egfr","bitp","bitt",
  "ast","alt","natm","ktm","cltm","catm","crp",
  "pct","ph1","pco21","po21","hco31","lac1",
  "ag1","pf1","ph2","pco22","po22","hco32","lac2","ag2","pf2",
  "giocc","ngayicu","tgnv","crrt","thomayicu","sofa","sofa2","thomay","gcs",
  "alb","ag1a","ag2a"
)
# kiểm tra u_vars chỉ có 1 giá trị sẽ gây lỗi 
sapply(u_vars, function(v) length(unique(na.omit(m[[v]]))))
# Chạy đơn biến
results <- lapply(u_vars, function(v) {
  model <- glm(
    as.formula(paste("event28 ~", v)),
    data = m,
    family = binomial
  )
  
  tidy(model, exponentiate = TRUE, conf.int = TRUE)[2, ]
})
donbien <- bind_rows(results)
print(donbien, n = Inf)

## Vẽ hình forrest
library(forestploter)
library(grid)

#========================
# DATA
#========================
dt <- data.frame(
  label = c("Tuổi", "Sốt", "   Có", "Đái tháo đường", "   Có", "Bệnh phổi mạn", "   Có", "Ung thư", "   Có", "Tần số thở", "Natri tĩnh mạch", "Kali tĩnh mạch", "CRP", "Procalcitonin", "pH lần 1", "pH lần 2", "Lactate lần 1", "Lactate lần 2", "SOFA", "Albumin máu"),
  est = c(1.03, NA, 0.386, NA, 0.344, NA, 2.99, NA, 3.33, 1.10, 0.943, 1.53, 1.00, 0.994, 0.828, 0.00134, 1.06, 1.28, 1.21, 0.863),
  low = c(1.00, NA, 0.178, NA, 0.149, NA, 1.06, NA, 1.45, 1.01, 0.892, 1.02, 1.00, 0.986, 0.0173, 0.0000341, 0.951, 1.15, 1.05, 0.794),
  hi = c(1.06, NA, 0.828, NA, 0.749, NA, 8.62, NA, 7.77, 1.22, 0.992, 2.35, 1.01, 1.00, 47.2, 0.0369, 1.17, 1.45, 1.42, 0.927),
  p = c(0.0492, NA, 0.0147, NA, 0.00904, NA, 0.0383, NA, 0.00482, 0.0303, 0.0271, 0.0420, 0.0150, 0.137, 0.925, 0.000186, 0.299, 0.0000342, 0.0111, 0.000166)
)

dt$`OR (95% CI)` <- ifelse(is.na(dt$est), "", sprintf("%.3f (%.3f-%.3f)", dt$est, dt$low, dt$hi))
dt$`P value` <- ifelse(is.na(dt$p), "", ifelse(dt$p < 0.001, "<0.001", sprintf("%.3f", dt$p)))
dt$col <- ifelse(is.na(dt$est), "grey", ifelse(dt$p > 0.05, "grey", ifelse(dt$est > 1, "red", "darkblue")))

# Data hiển thị (Xóa bỏ các khoảng trắng "lừa" ở chữ Plot cũ đi để căn giữa chuẩn)
dt_display <- data.frame(
  Characteristics = dt$label,
  `OR (95% CI)` = dt$`OR (95% CI)`,
  `                                          Plot                            ` = "", #mục tiêu là kéo giãn ô plot nên chừa rất nhiều khoảng trắng
  `P value` = dt$`P value`,
  check.names = FALSE
)

#========================
# THEME (CẤU HÌNH CĂN GIỮA Ở ĐÂY)
#========================
tm <- forest_theme(
  base_size = 10,
  plot_width = unit(10, "cm"), 
  ci_pch = 15,
  ci_lwd = 2,
  refline_col = "grey",
  vertline_col = "grey",
  title_gp = gpar(cex = 1.2, fontface = "bold"),
  
  # 1. Căn giữa cho phần nội dung số liệu (Core)
  # Cột 1 (Characteristics) căn trái "left", các cột 2, 3, 4 căn giữa "center"
  core = list(text_just = c("left", "center", "center", "center")),
  
  # 2. Căn giữa cho phần tiêu đề (Heading)
  # Tương tự: Tiêu đề cột 1 căn trái, các cột còn lại căn giữa
  heading = list(text_just = c("left", "center", "center", "center"))
)

#========================
# FOREST PLOT
#========================
p <- forest(
  data = dt_display,
  est = dt$est,
  lower = dt$low,
  upper = dt$hi,
  ci_column = 3,
  ref_line = 1,
  x_trans = "log",
  xlim = c(0.0001, 50),                  
  ticks_at = c(0.0001, 0.01, 1, 10, 50), 
  theme = tm,
  title = "Hồi quy logistic đơn biến"
)

#========================
# APPLY COLOR PER ROW
#========================
for (i in seq_len(nrow(dt))) {
  if (!is.na(dt$est[i])) {
    tryCatch({
      p <- edit_plot(p, row = i, col = 3, which = "ci", gp = gpar(col = dt$col[i], fill = dt$col[i]))
    }, error = function(e) {
      try({ p <- edit_plot(p, row = i, col = 3, which = "arrow", gp = gpar(col = dt$col[i], fill = dt$col[i])) }, silent = TRUE)
    })
  }
}

p <- add_border(p, part = "header", row = 1, where = "bottom", gp = gpar(lwd = 1.5))

#========================
# EXPORT PNG
#========================
png(
  "Forest_plot_final.png",
  width = 14,                            
  height = 7,
  units = "in",
  res = 300
)
plot(p)
dev.off()	   
# 02. Đa biến ---
## Chọn biến
t_vars <- c(
  "bmi","tuoi","sot","dtd","ungthu",
  "rlyt","hatt","hatr","nhietdo","rr",
  "wbc","neu","plt","inr","cre","bitp","ast","alt","natm","cltm","crp","pct",
  "ph1","lac1","ag1a",
  "ph2","lac2","ag2a",
  "sofa","sofa2")
## kiểm tra đa cộng tuyến
library(car)
fit <- lm(as.formula(paste("bmi ~", paste(t_vars[-1], collapse = "+"))), data = m)
vif(fit)
## xếp thứ tự
	# == VIF < 5: thường chấp nhận được
	# == VIF 5–10: có đa cộng tuyến đáng chú ý
	# == VIF > 10: đa cộng tuyến mạnh
	v <- vif(fit)
	sort(v, decreasing = TRUE)
# kiểm tra tương quan
cor(m[, c("hatt","hatr","natm","cltm")],
    use="complete.obs")
# bỏ sofa2 ra
t_vars <- setdiff(t_vars, "sofa2")
# lasso 
install.packages("glmnet")
library(glmnet)
	x <- model.matrix(~ ., data = m[, t_vars])[, -1]
	y <- m$death
	
	pf <- rep(1, ncol(x))
	pf[which(colnames(x) %in% c("ag1a","sofa"))] <- 0
	
	set.seed(123)
	lasso_model <- cv.glmnet(
	  x,
	  y,
	  family = "binomial",
	  alpha = 1,
	  penalty.factor = pf
	)
	
	coef(lasso_model, s = "lambda.min")
	lasso_model$lambda.min
	lasso_model$lambda.1se
# xem hệ số
coef_min <- coef(lasso_model, s = "lambda.min")
coef_1se <- coef(lasso_model, s = "lambda.1se")

sum(coef_min != 0) - 1   # số biến tại lambda.min (trừ intercept)
sum(coef_1se != 0) - 1   # số biến tại lambda.1se
# vẽ co nhỏ (chưa đạt, cần chỉnh sửa)
plot(lasso_model$glmnet.fit,
     xvar = "lambda",
     label = TRUE)

abline(v = log(lasso_model$lambda.min), lty = 2)
abline(v = log(lasso_model$lambda.1se), lty = 2)

text(log(lasso_model$lambda.min), 0,
     "lambda.min",
     pos = 4)

text(log(lasso_model$lambda.1se), 0,
     "lambda.1se",
     pos = 4)
# BMA
library(BMA)
bma_vars <- c(
  "tuoi",
  "sofa",
  "ag1a",
  "lac1",
  "alb"
)
bma_fit <- bic.glm(
  death ~ .,
  data = m[, c("death", bma_vars)],
  glm.family = binomial
)
summary(bma_fit)

# Logistic đa biến
  model_multi <- glm(y ~ x1 + x2 + x3, family = binomial, data = )
  summary(model_multi)
  exp(cbind(OR = coef(model_multi), confint(model_multi)))
