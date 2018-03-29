library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tb1= read_csv("C:/MathMod/MathMod/eddypro.csv",skip = 1,na =c("","NA","-9999","-9999.0"), comment=c("["))
tb1=tb1[-1,] 
tb1  
tb1$daytime = as.logical(tb1$daytime) 
tb1 = subset(tb1, as.Date(date) >= as.Date("2013-03-01") & as.Date(date) <= as.Date("2013-07-01") & daytime == T)
tb1
tb1=tb1[tb1$DOY > 132 & tb1$DOY < 152,] 
tb1=tb1[tb1$daytime == T,]
glimpse(tb1) 
tb1 = select(tb1, -(roll)) 
tb1 = tb1 %>% mutate_if(is.character, factor) 
names(tb1) = str_replace_all(names(tb1), "[!]","_emph_")
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tb1)
sapply(tb1,is.numeric)
tb1_numeric = tb1 [,sapply (tb1,is.numeric) ]
tb1_non_numeric = tb1[,!sapply(tb1,is.numeric) ]
cor_tb = cor(drop_na(tb1_numeric)) 
cor_tb
cor_tb = cor (drop_na(tb1_numeric))
cor_tb
cor_tb1 = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_tb1
vars = row.names(cor_tb1)[cor_tb1$co2_flux^2 > .2] %>% na.exclude 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
mod=lm(formula, data = tb1) 
anova(mod)  
summary(mod) 
formula1 = as.formula(paste("co2_flux ~ co2_flux + rand_err_co2_flux + co2_molar_density + 
                            T_star_ + un_co2_flux")) 
formula1 
mod2=lm(formula1, data = tb1) 
anova(mod2) 
summary(mod2) 
formula2 = as.formula(paste("co2_flux ~ co2_flux + co2_molar_density + 
                            T_star_ + un_co2_flux")) 
mod3=lm(formula2, data = tb1) 
anova(mod3) 
summary(mod3) 
mod4 = lm(co2_flux ~ (co2_flux + co2_molar_density + 
                        T_star_ + un_co2_flux)^2, data = tb1)
mod4
anova(mod4)
summary(mod4)
mod5 = lm(co2_flux ~ (co2_flux + co2_molar_density + 
                        T_star_ + un_co2_flux)^2 - T_star_:un_co2_flux, data = tb1)  
mod5
anova(mod5)
summary(mod5)
mod6 = lm(co2_flux ~ (co2_flux + co2_molar_density + 
                        T_star_ + un_co2_flux)^2 - T_star_:un_co2_flux -
            co2_molar_density - T_star_ - co2_flux:un_co2_flux , data = tb1)
mod6
anova(mod6)
summary(mod6)
mod7 = lm(co2_flux ~ (co2_flux + co2_molar_density + 
                        T_star_ + un_co2_flux)^2 - T_star_:un_co2_flux -
            co2_molar_density - T_star_ - 
            co2_flux:un_co2_flux - co2_molar_density:T_star_ , data = tb1)
mod7
anova(mod7)
summary(mod7)
