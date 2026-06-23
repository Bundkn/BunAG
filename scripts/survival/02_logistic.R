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
    as.formula(paste("death ~", v)),
    data = m,
    family = binomial
  )
  
  tidy(model, exponentiate = TRUE, conf.int = TRUE)[2, ]
})
donbien <- bind_rows(results)
print(donbien, n = Inf)
       
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
