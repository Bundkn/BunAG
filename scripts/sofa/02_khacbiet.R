# Tương quan Spearman
cor.test(
  m$sofa,
  m$sofa2,
  method = "spearman",
  exact = FALSE
)
# hoặc cách lấy Cl 
install.packages("Hmisc")
library(Hmisc)
rcorr(
  as.matrix(m[, c("sofa", "sofa2")]),
  type = "spearman"
)
