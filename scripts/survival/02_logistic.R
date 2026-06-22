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
# lasso 
install.packages("glmnet")
library(glmnet)
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
