# load package để dùng mutate
install.packages("tidyverse")
library(dplyr)
library(tidyr)
# tạo biến ag không kali
m <- m %>%
  mutate(
    agk1 = ag1 - k1,
    agk2 = ag2 - k2
  )
# tạo biến ag1a, ag2a ~ AG hiệu chỉnh albumin
m$ag1a <- m$ag1 + 2.5 * (4 - m$alb/10)
m$ag2a <- m$ag2 + 2.5 * (4 - m$alb/10)
# tạo biến sofa
m <- m %>%
  mutate(
    # Nếu thiếu liều vận mạch thì xem như không dùng
    lieunor  = replace_na(lieunor, 0),
    lieuadre = replace_na(lieuadre, 0),
    lieudobu = replace_na(lieudobu, 0),
    lieudopa = replace_na(lieudopa, 0),
    
    # =====================================================
    # 1. Hô hấp: PaO2/FiO2
    # =====================================================
    sofa_resp = case_when(
      is.na(pf1) ~ NA_real_,
      pf1 < 100 & thomay == 1 ~ 4,
      pf1 < 200 & thomay == 1 ~ 3,
      pf1 < 300 ~ 2,
      pf1 < 400 ~ 1,
      pf1 >= 400 ~ 0
    ),
    
    # =====================================================
    # 2. Đông máu: tiểu cầu
    # plt đơn vị: G/L hoặc 10^3/uL
    # =====================================================
    sofa_coag = case_when(
      is.na(plt) ~ NA_real_,
      plt < 20 ~ 4,
      plt < 50 ~ 3,
      plt < 100 ~ 2,
      plt < 150 ~ 1,
      plt >= 150 ~ 0
    ),
    
    # =====================================================
    # 3. Gan: bilirubin toàn phần
    # bitp đơn vị: µmol/L
    # =====================================================
    sofa_liver = case_when(
      is.na(bitp) ~ NA_real_,
      bitp > 204 ~ 4,
      bitp >= 102 ~ 3,
      bitp >= 33 ~ 2,
      bitp >= 20 ~ 1,
      bitp < 20 ~ 0
    ),
    
    # =====================================================
    # 4. Tim mạch
    # Liều dopamine, dobutamine, noradrenaline, adrenaline
    # đơn vị: mcg/kg/phút
    # =====================================================
    sofa_cv = case_when(
      lieudopa > 15 | lieuadre > 0.1 | lieunor > 0.1 ~ 4,
      lieudopa > 5  | lieuadre > 0   | lieunor > 0   ~ 3,
      lieudopa > 0  | lieudobu > 0 ~ 2,
      TRUE ~ 0
    ),
    
    # =====================================================
    # 5. Thần kinh: Glasgow coma scale
    # =====================================================
    sofa_cns = case_when(
      is.na(gcs) ~ NA_real_,
      gcs < 6 ~ 4,
      gcs <= 9 ~ 3,
      gcs <= 12 ~ 2,
      gcs <= 14 ~ 1,
      gcs == 15 ~ 0
    ),
    
    # =====================================================
    # 6. Thận: creatinine
    # cre đơn vị: µmol/L
    # Chưa tính nước tiểu vì chưa có biến urine output
    # =====================================================
    sofa_renal = case_when(
      is.na(cre) ~ NA_real_,
      cre > 440 ~ 4,
      cre >= 300 ~ 3,
      cre >= 171 ~ 2,
      cre >= 110 ~ 1,
      cre < 110 ~ 0
    ),
    
    # =====================================================
    # Tổng SOFA gốc
    # =====================================================
    sofa = sofa_resp + sofa_coag + sofa_liver +
      sofa_cv + sofa_cns + sofa_renal
  )

# ============================================================
# ĐOẠN 2: TÍNH SOFA-2 SCORE
# ============================================================

age <- age %>%
  mutate(
    # Xử lý missing liều thuốc: nếu NA thì xem như không dùng
    lieunor  = replace_na(lieunor, 0),
    lieuadre = replace_na(lieuadre, 0),
    lieudobu = replace_na(lieudobu, 0),
    lieudopa = replace_na(lieudopa, 0),
    lieuvaso = replace_na(lieuvaso, 0),
    
    # SOFA-2 dùng tổng norepinephrine + epinephrine
    # Đơn vị: mcg/kg/phút
    ne_epi_sum = lieunor + lieuadre,
    
    # Other vasopressor / inotrope:
    # gồm dopamine, dobutamine, vasopressin
    other_vaso_inotrope = if_else(
      lieudobu > 0 | lieudopa > 0 | lieuvaso > 0,
      1, 0
    ),
    
    # 1. Brain: GCS
    sofa2_brain = case_when(
      is.na(gcs) ~ NA_real_,
      gcs == 15 ~ 0,
      gcs >= 13 & gcs <= 14 ~ 1,
      gcs >= 9  & gcs <= 12 ~ 2,
      gcs >= 6  & gcs <= 8  ~ 3,
      gcs <= 5 ~ 4
    ),
    
    # 2. Respiration: P/F ratio
    # thomay = 1 dùng như proxy cho advanced ventilatory support
    sofa2_resp = case_when(
      is.na(pf1) ~ NA_real_,
      pf1 > 300 ~ 0,
      pf1 <= 300 & pf1 > 225 ~ 1,
      pf1 <= 225 & pf1 > 150 ~ 2,
      pf1 <= 150 & pf1 > 75 & thomay == 1 ~ 3,
      pf1 <= 75 & thomay == 1 ~ 4,
      
      # Nếu P/F <=150 nhưng không thở máy, không lên mức 3-4
      pf1 <= 150 & thomay == 0 ~ 2
    ),
    
    # 3. Cardiovascular
    sofa2_cv = case_when(
      ne_epi_sum > 0.4 ~ 4,
      ne_epi_sum > 0.2 & ne_epi_sum <= 0.4 & other_vaso_inotrope == 1 ~ 4,
      
      ne_epi_sum > 0.2 & ne_epi_sum <= 0.4 ~ 3,
      ne_epi_sum > 0 & ne_epi_sum <= 0.2 & other_vaso_inotrope == 1 ~ 3,
      
      ne_epi_sum > 0 & ne_epi_sum <= 0.2 ~ 2,
      ne_epi_sum == 0 & other_vaso_inotrope == 1 ~ 2,
      
      ne_epi_sum == 0 & other_vaso_inotrope == 0 ~ 0
    ),
    
    # 4. Liver: bilirubin toàn phần, đơn vị µmol/L
    sofa2_liver = case_when(
      is.na(bitp) ~ NA_real_,
      bitp <= 20.6 ~ 0,
      bitp <= 51.3 ~ 1,
      bitp <= 102.6 ~ 2,
      bitp <= 205 ~ 3,
      bitp > 205 ~ 4
    ),
    
    # 5. Kidney: creatinine, đơn vị µmol/L
    # Chưa tính urine output/RRT vì chưa có biến
    sofa2_kidney = case_when(
      is.na(cre) ~ NA_real_,
      cre <= 110 ~ 0,
      cre <= 170 ~ 1,
      cre <= 300 ~ 2,
      cre > 300 ~ 3
    ),
    
    # 6. Hemostasis: platelet
    # plt đơn vị G/L hoặc 10^3/uL
    sofa2_hemostasis = case_when(
      is.na(plt) ~ NA_real_,
      plt > 150 ~ 0,
      plt <= 150 & plt > 100 ~ 1,
      plt <= 100 & plt > 80 ~ 2,
      plt <= 80 & plt > 50 ~ 3,
      plt <= 50 ~ 4
    ),
    
    # Tổng SOFA-2
    sofa2 = sofa2_brain + sofa2_resp + sofa2_cv +
      sofa2_liver + sofa2_kidney + sofa2_hemostasis
  )

# Kiểm tra nhanh SOFA-2
m %>%
  select(
    sofa2,
    sofa2_brain, sofa2_resp, sofa2_cv,
    sofa2_liver, sofa2_kidney, sofa2_hemostasis
  ) %>%
  head(20)
