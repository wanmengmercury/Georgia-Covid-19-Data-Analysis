#I combined the economic data, demographic data, and covid-19 data 
socio = read.table("/Users/mercuryliu/Documents/Covid-19 Research/Data/Economic Data/socieconData.csv", 
                   sep=",", header = TRUE)

#check correlation
splom(socio[6:10], pscales = 0)
round(cor(socio[1:6]),2)
colnames(socio)
#preprocess data
socio$incomepercaita <- socio$personal_income/socio$population
socio$employercpercaita <- socio$employer_contri/socio$population
socio$jobspercaita <- socio$totalJobs/socio$population


#Question1: Does percentage of black and African American residents have an effect on
#Covid-19 mortality rate?

# Method(1): Compare the based line model and the model with the demographic variable,
#if the the R squared is higher in the larger model, it means that the percentage of 
#black and African American residents have an effect on Covid-19 mortality rate

#base line model
fm1 <- lm(death_rate ~ hosp_rate + incomepercaita + employercpercaita + jobspercaita, data = socio)
summary(fm1)
#add the variable black_pct
fm2 <- lm(death_rate ~ hosp_rate +incomepercaita + employercpercaita + jobspercaita + black_pct, data = socio)
summary(fm2)

#Results:
#baseline model(adj. r_squared=0.4985): mortality_rate = 115.85414 + 0.25802*hosp_rate -1.10076*income_percapita +
#0.96144employer_contri -56.20942*totalJobs 
#larger model: mortality_rate(adj. r_squared=0.5333) = 87.05537 + 0.24249*hosp_rate -0.63942*personal_income +
#1.55799*employer_contri -91.73177*totalJobs + 1.28018*black_pct

#Conclusion: The variable black_pct has an effect on Covid-19 mortality rate 

# Method(2): Use anova to compare the baseline model and the larger model
#Decision Rule: if the p-value is smaller than 0.05, we claim that the larger model is better
fm1 <- lm(death_rate ~ hosp_rate + incomepercaita + employercpercaita + jobspercaita, data = socio)
fm2 <- lm(death_rate ~ hosp_rate + incomepercaita + employercpercaita + jobspercaita + black_pct, data = socio)
anova(fm1,fm2)

#Results
# The small p-value (0.0005503) shows that fm2 is better. 
# The percentage of black and african american residents do have an effect on the mortality rate. 




