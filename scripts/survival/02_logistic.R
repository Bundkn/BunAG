# 01. Đơn biến ----
install.packages("broom")
library(broom)
library(dplyr)
# chọn u_vars
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
