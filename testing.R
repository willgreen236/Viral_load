library(dplyr) 
library(ggplot2)
library(tidyverse)

ct_df <- read.csv("ct_dat_refined.csv") %>% 
  filter(PersonID %in% c(154)) %>%
  mutate(vl = 1.1^(40-CtT1)) %>%
  rename(tau = TestDateIndex) %>%
  mutate(tau = round(tau,1))

ggplot(ct_df, aes(x = TestDateIndex, y=vl)) +
  geom_point() +
  facet_wrap(~PersonID) +
  theme_bw()

params <- c(tau_max = 4/5, v_max = 12, a = 3.3, b = 0.6, offset = 5)

vl <- function(tau, tau_max, v_max, a, b, offset){
  vl_tau <- v_max * (a + b)/(b*exp(-a * (tau-tau_max+offset)) + a*exp(b * (tau-tau_max+offset)))
  return(vl_tau)
}

checking <- data.frame(tau = seq(-5, 10,0.1)) %>%
  mutate(vl_tau = vl(tau, tau_max = 4.5, v_max = 2, a = 3.3, b = 0.6, offset = 5))

ggplot(checking, aes(x=tau, y=vl_tau)) + 
  geom_line() +
  theme_bw()

combined <- left_join(ct_df, checking, by='tau') %>% drop_na(vl_tau)

ggplot(combined, aes(x=tau)) +
  geom_point(aes(y = vl)) +
  geom_line(aes(y = vl_tau))
  

R_squared_calculator <- 
