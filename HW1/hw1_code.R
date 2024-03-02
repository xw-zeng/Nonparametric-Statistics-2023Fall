
# Exercise 3

L = function(p){
  p * 34 - sqrt(p * (1-p) * 34)*qnorm(0.95)
}

U = function(p){
  p * 34 + 1 + sqrt(p * (1-p) * 34)*qnorm(0.95)
}

# Exercise 5


X = c(rep(75.1, 39), 90)
mean(X)
sd(X)
z = (mean(X)-75) / (sd(X) / sqrt(40))
1 - pnorm(z)

Z = function(x40){
  X = c(rep(75.1, 39), x40)
  out = (mean(X)-75) / (sd(X) / sqrt(40))
  return(out)
}

library(ggplot2)

Z_list = c()
for (i in seq(90, 76, -1)){
  Z_list = c(Z_list, Z(i))
}

ggplot() +
  geom_line(aes(y = Z_list, x = seq(90, 76, -1))) +
  theme_light() +
  labs(x = 'the 40th value', y = 'Z_mu')

# Exercise 7

P = c(0.0010, 0.0098, 0.0439, 0.1172, 0.2051, 0.2461)
cumsum(P)

# Exercise 8-1

Z_mu = function(mu){(mu - 75) / (2.5 / sqrt(40))}
Z_B = function(p){(p - 0.5) / sqrt(p * (1 - p) / 40)}

pw_mu = function(mu){1 - pnorm(1.645 - Z_mu(mu))}
pw_B = function(mu){
  p = pnorm((mu - 75) / 2.5)
  1 - pnorm(1.645 * sqrt(0.25 / (p * (1 - p))) - Z_B(p))
}
pw_Diff = function(mu){abs(pw_mu(mu) - pw_B(mu))}

mu_list = seq(75.01, 77, 0.01)

data = data.frame('mu' = mu_list,
                  'CLT' = pw_mu(mu_list),
                  'Bin' = pw_B(mu_list), 
                  'diff' = pw_mu(mu_list) - pw_B(mu_list))

ggplot(data=data) +
  geom_line(aes(y = CLT, x = mu, color='CLT')) +
  geom_line(aes(y = Bin, x = mu, color='Binomial')) +
  theme_light() +
  labs(x = 'mu', y = 'power') +
  guides(color = guide_legend(title = 'Test',
                              title.theme = element_text(size = 9),
                              legend.text = element_text(size = 7))) +
  theme(legend.position = c(0.85, 0.2))

ggplot(data=data) +
  geom_line(aes(y = diff, x = mu)) +
  geom_point(aes(y = diff[94], x = mu[94]), color='red', size=2)+
  theme_light() +
  labs(x = 'mu', y = 'power difference')

optimise(pw_Diff, lower = 75 + 1e-5, upper = 77, maximum = T)

# Exercise 8-2

Z_mu = function(mu){(mu - 75) / (2.5 / sqrt(40))}
Z_B = function(p){(p - 0.5) / sqrt(p * (1 - p) / 40)}

pw_mu = function(mu){1 - pnorm(1.645 - Z_mu(mu))}
pw_B = function(mu){
  p = 1 - 0.5 * exp(- (mu - 75) / sqrt(3.125))
  1 - pnorm(1.645 * sqrt(0.25 / (p * (1 - p))) - Z_B(p))
}
pw_Diff = function(mu){abs(pw_mu(mu) - pw_B(mu))}

mu_list = seq(75.01, 77, 0.01)

data = data.frame('mu' = mu_list,
                  'CLT' = pw_mu(mu_list),
                  'Bin' = pw_B(mu_list), 
                  'Diff' = pw_mu(mu_list) - pw_B(mu_list))

ggplot(data=data) +
  geom_line(aes(y = CLT, x = mu, color='CLT')) +
  geom_line(aes(y = Bin, x = mu, color='Binomial')) +
  theme_light() +
  labs(x = 'mu', y = 'power') +
  guides(color = guide_legend(title = 'Test',
                              title.theme = element_text(size = 9),
                              legend.text = element_text(size = 7))) +
  theme(legend.position = c(0.85, 0.2))

ggplot(data=data) +
  geom_line(aes(y = Diff, x = mu)) +
  geom_point(aes(y = Diff[65], x = mu[65]), color='red', size=2)+
  theme_light() +
  labs(x = 'mu', y = 'power difference')

optimise(pw_Diff, lower = 75 + 1e-5, upper = 77, maximum = T)

