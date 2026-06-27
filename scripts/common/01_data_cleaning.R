# Import data
library(readr)
m <- read_csv("agdfupdate2.csv")
View(m)

install.packages("tidyverse")
library(dplyr)
factor_vars <- c(
  "gioitinh", "sot", "daubung", "daunguc", "daudau", "ho", "khotho",
  "tieuchay", "tietnieu", "vangda", "tcyt", "daukhop", "chuongbung",
  "ankem", "nonoi", "sungdau", "phu", "tcnieu",
  "tha", "suytim", "bmv", "rln", "rungnhi", "rlnk", "maytn",
  "coicd", "dtd", "ckdc", "ckdr", "xogan", "bpm", "ungthu",
  "rlyt", "crt", "cocrp", "copct", "ttxk",
  "khoanv", "nhapicu", "songsot", "hientai", "event28", "event14",
  "crrt", "thomay", "thomayicu",
  "pttt", "loaipttt",
  "caymau", "kqmau", 
  "caydam", "kqdam", 
  "caynt", "kqnt", 
  "caydich", "kqdich", 
  "cayapxe", "kqapxe", 
  "caykhac", "kqkhac"
)

keep_chr_vars <- c("sonhapvien", "vkmau", "vkdam", "vkdich", "vknt", "vkkhac", "vkapxe")

m <- m %>%
  mutate(
    across(any_of(factor_vars), as.factor),
    across(
      -any_of(c(factor_vars, keep_chr_vars)),
      as.numeric
    )
  )
# Check missing values
m %>%
  summarise(across(where(is.numeric), ~sum(is.na(.))))

# Xử lý mising value
install.packages("mice")
library(mice) 
# các biến khác
# xử lý NA, m = 50
imp_vars <- c(
  "hatt", "hatr", "ure", "pt", "inr",
  "catm", "bitp", "bitt", "ast", "alt",
  "crp", "pct", "lac1", "na1", "cl1",
  "ph2", "pco22", "po22", "hco32",
  "lac2", "na2", "k2", "cl2", "pf2"
)

imp <- mice(
  m[imp_vars],
  m = 50,
  method = "pmm",
  seed = 123
)

m[imp_vars] <- complete(imp, 1)
# kiểm tra NA trong cột
sum(is.na(m$ph2))
# biến albumin 
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
