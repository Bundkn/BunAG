# Import data

# Check missing values

# Xử lý mising value
# các biến khác

# biến albumin 
library(mice)
# Khởi tạo
ini <- mice(m, maxit = 0)

# Không impute biến nào ngoài alb
meth <- ini$method
meth[] <- ""
meth["alb"] <- "pmm"

# Không dùng predictor nào mặc định
pred <- ini$predictorMatrix
pred[,] <- 0

# Chọn predictor cho alb
pred["alb", c(
  "tuoi",
  "gioitinh",
  "bmi",
  "plt",
  "inr",
  "ure",
  "cre",
  "bitp",
  "bitt",
  "ast",
  "alt",
  "xogan",
  "ungthu",
  "ckdr",
  "ckdc"
)] <- 1

# Không tự dự đoán chính nó
pred["alb", "alb"] <- 0

# Multiple imputation
imp <- mice(
  m,
  method = meth,
  predictorMatrix = pred,
  m = 50,
  seed = 123
)

# Lấy bộ dữ liệu hoàn chỉnh thứ nhất
m_imp <- complete(imp, 1)
# thay thế trực tiếp vào dataset gốc
m$alb <- complete(imp, 1)$alb

# Albumin corrected AG

# Save cleaned dataset
