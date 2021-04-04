# set.seed(2020)
dacc <- NULL
df1 <- NULL
dmcc <- NULL

for (i in 1:10000) {
  ds <- sample(c(rep(1, 200), rep(0, 1800)))
  dt <- sample(c(rep(1, 200), rep(0, 1800)))

  tp <- as.numeric(sum(ds & dt))
  tn <- as.numeric(sum(ds == 0 & dt == 0))
  fn <- as.numeric(sum(ds == 1 & dt == 0))
  fp <- as.numeric(sum(ds == 0 & dt == 1))

  acc <- (tp + tn) / length(ds)
  f1 <- (2 * tp) / (2 * tp + fp + fn)
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

  dacc <- c(dacc, acc)
  df1 <- c(df1, f1)
  dmcc <- c(dmcc, mcc)
}

hist(dacc, breaks = 125)
hist(df1, breaks = 125)
hist(dmcc, breaks = 125)

mean(dacc); mean(df1); mean(dmcc)
