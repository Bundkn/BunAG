# tạo biến ag không kali
agx <- agx %>%
  mutate(
    agk1 = ag1 - k1,
    agk2 = ag2 - k2
  )
# tạo biến ag1a, ag2a ~ AG hiệu chỉnh albumin
m$ag1a <- m$ag1 + 2.5 * (4 - m$alb/10)
m$ag2a <- m$ag2 + 2.5 * (4 - m$alb/10)
