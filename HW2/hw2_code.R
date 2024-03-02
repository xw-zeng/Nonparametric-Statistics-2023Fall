# Exercise 1
1 / choose(8, 4)

# Exercise 2
t1 = c(10, 15, 50)
t2 = c(12, 17, 19)
# 2a
mean(t1); mean(t2); mean(t1) - mean(t2)
pt1 = combn(c(t1, t2), 3)
pt2 = pt1[, seq(choose(6, 3), 1, -1)]
ptm = colMeans(pt1) - colMeans(pt2)
sum(ptm >= 9)
sum(ptm >= 9) / choose(6, 3)
# 2b
pts = colSums(pt1)
sum(pts >= 75) / choose(6, 3)

# Exercise 3
median(t1); median(t2); median(t1) - median(t2)
ptmd = sapply(data.frame(pt1), median) - sapply(data.frame(pt2), median)
sum(ptmd <= -2) / choose(6, 3)

# Exercise 7
rural = c(3, 2, 1, 1, 2, 1, 3, 2, 2, 2, 2, 5,
          1, 4, 1, 1, 1, 1, 6, 2, 2, 2, 1, 1)
urban = c(1, 0, 1, 1, 0, 0, 1, 1, 1, 8, 1, 1, 1, 0, 1, 1, 2)
combined_sample = c(rural, urban)
m = length(rural)
n = length(urban)
N = m + n
# 7a
rank_total = rank(combined_sample)
W_rural = sum(rank_total[1:m])
set.seed(2023)
b = 50000
permuted_sample = rep(0, b)
for (i in 1:b){permuted_sample[i] = sum(sample(rank_total, m))}
sum(permuted_sample >= W_rural) / b
# 7b
S2 = (sum((rural - mean(rural)) ^ 2) +
        sum((urban - mean(urban)) ^ 2)) / (m + n - 2)
t = (mean(rural) - mean(urban)) / sqrt(S2 / m + S2 / n)
pt(t, df = m + n - 2, lower.tail = F)

S2 = (sum((rural - mean(rural)) ^ 2) +
        sum((urban[-10] - mean(urban[-10])) ^ 2)) / (m + (n - 1) - 2)
t = (mean(rural) - mean(urban[-10])) / sqrt(S2 / m + S2 / (n - 1))
pt(t, df = m + (n - 1) - 2, lower.tail = F)
# t.test(rural, urban, alternative = 'greater', var.equal = TRUE)
# t.test(rural, urban[-10], alternative = 'greater', var.equal = TRUE)

# Exercise 8
S_rural = sum(rural)
set.seed(2023)
b = 50000
permuted_sample = rep(0, b)
for (i in 1:b){permuted_sample[i] = sum(sample(combined_sample, m))}
sum(permuted_sample >= S_rural) / b

# Exercise 10
s1 = c(5, 11, 16, 8, 12)
s2 = c(17, 14, 15, 21, 19, 13)
combined_sample = c(s1, s2)
m = length(s1); n = length(s2); N = m + n
waerden_score = function(i, N){qnorm(i / (N + 1))}
rank_total = rank(combined_sample)
scores = waerden_score(rank_total, N)
score1 = sum(scores[1:m])
set.seed(2023)
b = 50000
permuted_sample = rep(0, b)
for (i in 1:b){permuted_sample[i] = sum(sample(scores, m))}
sum(permuted_sample <= score1) / b

# Exercise 12
pwd_table = matrix(rep(s1, n), nrow = n, byrow = T) -
  matrix(rep(s2, m), ncol = m)
pwd_sorted = sort(pwd_table)
pwd_sorted[6]; pwd_sorted[25]; median(pwd_table)
# Exercise 18
g1 = c(11, 33, 48, 34, 112, 369, 64, 44)
g2 = c(177, 80, 141, 332)
combined_sample = c(g1, g2)
m = length(g1); n = length(g2); N = m + n
# Wilcoxon rank-sum test
wilcox.test(g1, g2, alternative = 'less')
# Van Der Waerden scores test
rank_total = rank(combined_sample)
scores = waerden_score(rank_total, N)
score1 = sum(scores[1:m])
pt1 = combn(scores, m)
pts = colSums(pt1)
sum(pts <= score1) / choose(N, m)
# Exponential scores test
exponential = function(i, N){sum(1 / seq(N, N + 1 - i, - 1))}
scores = sapply(rank_total, exponential, N = N)
score1 = sum(scores[1:m])
pt1 = combn(scores, m)
pts = colSums(pt1)
sum(pts <= score1) / choose(N, m)
# Permutation test
pt1 = combn(combined_sample, m)
pts = colSums(pt1)
sum(pts <= sum(g1)) / choose(N, m)
# Removing an outlier
g1 = g1[-6]
combined_sample = c(g1, g2)
m = length(g1); n = length(g2); N = m + n
# Wilcoxon rank-sum test
wilcox.test(g1, g2, alternative = 'less')
# Van Der Waerden scores test
rank_total = rank(combined_sample)
scores = waerden_score(rank_total, N)
score1 = sum(scores[1:m])
pt1 = combn(scores, m)
pts = colSums(pt1)
sum(pts <= score1) / choose(N, m)
# Exponential scores test
exponential = function(i, N){sum(1 / seq(N, N + 1 - i, - 1))}
scores = sapply(rank_total, exponential, N = N)
score1 = sum(scores[1:m])
pt1 = combn(scores, m)
pts = colSums(pt1)
sum(pts <= score1) / choose(N, m)
# Permutation test
pt1 = combn(combined_sample, m)
pts = colSums(pt1)
sum(pts <= sum(g1)) / choose(N, m)
