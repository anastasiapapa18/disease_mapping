# load required libraries
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(readxl)
library(INLA)

###### Data Pre-processing #####
#Read in data.xlsx
data <- read_excel("data/data.xlsx")
head(data)

#Read in shape file
municipality.shp <- st_read("data/gemeenten_304_2025.shp")
head(municipality.shp)


#Filter data for age under 45
data_under25 <- data %>% filter(age %in% c("0-24","25-44") )
head(data_under25)

#Indirect Standardization
#rate_g = O_gs/n_gs
rate <- data_under25 %>%
  group_by(age) %>% 
  summarise(O_d = sum(diabetics), O_c = sum(chronical_condition), N= sum(beneficaries)) %>%
  mutate(rate_d = O_d/N, rate_c = O_c/N)
head(rate)

# E_i = rate_gs * n_gi
data_under25 <- left_join(data_under25,rate,by ="age") %>%
  mutate(Ei_d = rate_d * beneficaries,Ei_c = rate_c *beneficaries)
head(data_under25)

# SMR = sum(y_gi)/sum(E_gi)
result <- data_under25 %>% group_by(municipality) %>% 
  summarize(O_d = sum(diabetics),O_c = sum(chronical_condition),E_d=sum(Ei_d),E_c = sum(Ei_c)) %>%
  mutate(SMR_d=O_d/E_d, SMR_c = O_c/ E_c ) 
head(result)

##### Poisson Lognormal Model #####

### Diabetics Data ###
diabetics <- list(O = result$O_d,E= result$E_d,N=nrow(result))

#### specification of model - POSSON LOGNORMAL MODEL
d.formula <- O ~ f(region, model='iid', param=c(2,0.5))

#### Prepare data:
ID <- 1:diabetics$N
d.data <- data.frame(O=diabetics$O, E=diabetics$E,region = ID)
head(d.data)

#### Model Specification

d.model.res <- inla(d.formula, 
                  family="Poisson", 
                  data= d.data,
                  E = E,
                  control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))


#### Produce general summary 

summary(d.model.res)

## Estimates of intercept and precision parameters
d.model.res$summary.fixed
d.model.res$summary.hyperpar

## Derive estimate of variance parameters
marg.variance<-inla.tmarginal(function(x) 1/x,d.model.res$marginals.hyperpar$"Precision for region")
inla.zmarginal(marg.variance)

## Estimates of random effects
d.model.res$summary.random$region

## Estimates of RR
d.model.res$summary.fitted.values

#### Make map
municipality.shp$SMR_d <- d.data$O/d.data$E
municipality.shp$theta_d<-d.model.res$summary.fitted.values$mean

## SMR plot (NOT really needed)
tm_shape(municipality.shp) + 
  tm_polygons(c("SMR_d"), palette = "-RdYlGn",breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)

## RR plot of diabetics
## I have trouble mapping with the following code from lecture
tm_shape(municipality.shp) + 
  tm_polygons(c("theta_d"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)

## this version (v4 of tmap) seems to work better
tm_shape(municipality.shp) + 
  tm_polygons(
    fill = "theta_d", 
    fill.scale = tm_scale_intervals(
      style = "fixed", 
      breaks = c(0, 0.5, 0.6, 0.8, 1.2, 1.5, 2, 2.5),
      values = "-brewer.rd_yl_gn" # v4 uses 'values' instead of 'palette', with the prefix for cols4all
    )
  ) + 
  tm_layout(legend.title.size = 1)


# WAIC & DIC
c(d.model.res$waic$waic,d.model.res$waic$p.eff)

c(d.model.res$dic$dic,d.model.res$dic$p.eff)

### Chronic Condition Data ###
chronic <- list(O = result$O_c,E= result$E_c,N=nrow(result))

#### specification of model - POSSON LOGNORMAL MODEL
c.formula <- O ~ f(region, model='iid', param=c(2,0.5))

#### Prepare data:
ID <- 1:chronic$N
c.data <- data.frame(O=chronic$O, E=chronic$E,region = ID)
head(c.data)

#### Model Specification

c.model.res <- inla(c.formula, 
                    family="Poisson", 
                    data= c.data,
                    E = E,
                    control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))


#### Produce general summary 

summary(c.model.res)

## Estimates of intercept and precision parameters
c.model.res$summary.fixed
c.model.res$summary.hyperpar

## Derive estimate of variance parameters
marg.variance<-inla.tmarginal(function(x) 1/x,c.model.res$marginals.hyperpar$"Precision for region")
inla.zmarginal(marg.variance)

## Estimates of random effects
c.model.res$summary.random$region

## Estimates of RR
c.model.res$summary.fitted.values

#### Make map
municipality.shp$SMR_c <- c.data$O/d.data$E
municipality.shp$theta_c<-c.model.res$summary.fitted.values$mean

#SMR plot (NOT really needed)
tm_shape(municipality.shp) + 
  tm_polygons(c("SMR_c"), palette = "-RdYlGn",breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)

#RR plot of Chronic disease 
## The following code from course does not work well
tm_shape(municipality.shp) + 
  tm_polygons(c("theta_c"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)

## this version (v4 of tmap) seems to work better
tm_shape(municipality.shp) + 
  tm_polygons(
    fill = "theta_c", 
    fill.scale = tm_scale_intervals(
      style = "fixed", 
      breaks = c(0, 0.5, 0.6, 0.8, 1.2, 1.5, 2, 2.5),
      values = "-brewer.rd_yl_gn" # v4 uses 'values' instead of 'palette', with the prefix for cols4all
    )
  ) + 
  tm_layout(legend.title.size = 1)

# WAIC & DIC
c(c.model.res$waic$waic,c.model.res$waic$p.eff)

c(c.model.res$dic$dic,c.model.res$dic$p.eff)

