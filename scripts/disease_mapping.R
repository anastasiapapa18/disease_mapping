# load required libraries
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(readxl)

#Read in data.xlsx
data <- read_excel("data/data.xlsx")
head(data)

#Filter data for age under 45
data_under25 <- data %>% filter(age %in% c("0-24","25-44") )
head(data_under25)

#Indirect Standardization
#rate_g = y_gs/n_gs
rate <- data_under25 %>%
  group_by(age) %>% 
  summarise(Y_d = sum(diabetics), Y_c = sum(chronical_condition), N= sum(beneficaries)) %>%
  mutate(rate_d = Y_d/N, rate_c = Y_c/N)
head(rate)

# E_i = rate_gs * n_gi
data_under25 <- left_join(data_under25,rate,by ="age") %>%
  mutate(Ei_d = rate_d * beneficaries,Ei_c = rate_c *beneficaries)
head(data_under25)

# SMR = sum(y_gi)/sum(E_gi)
result <- data_under25 %>% group_by(municipality) %>% 
  summarize(Y_d = sum(diabetics),Y_c = sum(chronical_condition),E_d=sum(Ei_d),E_c = sum(Ei_c)) %>%
  mutate(SMR_d=Y_d/E_d, SMR_c = Y_c/ E_c ) 
head(result)
