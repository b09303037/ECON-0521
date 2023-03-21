#assign parameters
mu_0 <- 5
mu_1 <- 10
sigma_0 <- sqrt(7)
sigma_1 <- sqrt(2)
sigma_01 <- 3
C <- 4

#gen multi-normaldistribution
library(MASS)
size <- 10000000
mean_vector <- c(0,0)
covariance_matrix <- matrix(c(7,3,3,2),2,2)
distrbution <- mvrnorm(
  n=size,
  mu=mean_vector,
  Sigma=covariance_matrix
)

#epsilon0~(0,7)
#epsilon1~(0,2)
epsilon_0 <- distrbution[,1]
epsilon_1 <- distrbution[,2]

table.data <- data.frame(mu_0, mu_1, sigma_0, sigma_1, sigma_01, epsilon_1, epsilon_0, C)

w_0 = c(mu_0 + epsilon_0)
table.data <- cbind(table.data,w_0)

w_1 = c(mu_1 + epsilon_1)
table.data <- cbind(table.data,w_1)

v = c(epsilon_1 - epsilon_0)
sigma_v = sqrt(var(v))
table.data <- cbind(table.data,v)

I<-replicate(10000000,sample(0:1, 1))
table.data <- cbind(table.data,I)

#--------------------------------------------------data

library(dplyr)
table.data %>%
  group_by(I) %>%
  summarise(w_0 = mean(w_0))

library(dplyr)
table.data %>%
  group_by(I) %>%
  summarise(w_1 = mean(w_1))

library(dplyr)
table.data %>%
  group_by(I) %>%
  summarise(epsilon_0 = mean(epsilon_0))

library(dplyr)
table.data %>%
  group_by(I) %>%
  summarise(epsilon_1 = mean(epsilon_1))

ro <- sigma_01 / (sigma_0 * sigma_1)

z <- (mu_0 - mu_1 + C)/sigma_v

phi_z <- dnorm(z,0,1)

capphi_z <- pnorm(z,0,1)


#RHS
w_o_I <- mu_0 + sigma_0*sigma_1*(ro - sigma_0 / sigma_1)*(phi_z/(1-capphi_z))/sigma_v

w_1_I <- mu_1 + sigma_0*sigma_1*(sigma_1 / sigma_0 - ro)*(phi_z/(1-capphi_z))/sigma_v


