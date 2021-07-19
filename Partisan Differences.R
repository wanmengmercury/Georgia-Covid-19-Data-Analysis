library(dplyr); library(data.table)
#Q: does republican and democrat areas have different vaccinaiton rates?
#what is the relationship between partisan preferences and vaccination rate?
#context: republican meida/politician do not advocate for getting vaccinated
#vs democrat channels follow the Biden Administraiton's policy 
ele = read.table("/Users/mercuryliu/Documents/Covid-19 Research/Data/elec_vax_67.csv",sep=",", header = TRUE)
ele.head()
ele$party <- as.factor(ele$label)
summary(aov(rank(ele$Series_Complete_18PlusPop_Pct) ~ ele$party))
summary(aov(rank(ele$Series_Complete_12PlusPop_Pct) ~ ele$party))
summary(aov(rank(ele$Series_Complete_65PlusPop_Pct) ~ ele$party))
fm1c <- lm( Series_Complete_18PlusPop_Pct ~ rep_pct, data = ele)
summary(fm1c)

#simple LR, replican votes percentage is negatively correlated with 18 and plus population
#vaccination rate
fm1c <- lm(Series_Complete_18PlusPop_Pct ~ rep_pct, data = ele)
summary(fm1c)
fm1c <- lm(Series_Complete_12PlusPop_Pct ~ rep_pct, data = ele)
summary(fm1c)
fm1c <- lm(Series_Complete_65PlusPop_Pct ~ rep_pct, data = ele)
summary(fm1c)
#democrat votes percentage is postitively correlated with 18 and plus population vaccination rate
fm1c <- lm(Series_Complete_18PlusPop_Pct ~ dem_pct, data = ele)
summary(fm1c)
fm1c <- lm(Series_Complete_12PlusPop_Pct ~ dem_pct, data = ele)
summary(fm1c)
fm1c <- lm(Series_Complete_65PlusPop_Pct ~ dem_pct, data = ele)
summary(fm1c)

#rep models:
#Series_Complete_18PlusPop_Pct = 31.767 - 22.103* rep_pct, Adjusted R-squared: 0.09086
#Series_Complete_12PlusPop_Pct = 30.259 - 21.501* rep_pct, Adjusted R-squared: 0.09404
#Series_Complete_65PlusPop_Pct = 50.306 - -35.299* rep_pct, Adjusted R-squared:0.09402
#dem models:
#Series_Complete_18PlusPop_Pct = 10.119 + 21.416 * dem_pct, Adjusted R-squared: 0.08516
#Series_Complete_12PlusPop_Pct = 9.201 + 20.831 * dem_pct, Adjusted R-squared: 0.08813
#Series_Complete_65PlusPop_Pct = 15.706 + 34.280 * dem_pct, Adjusted R-squared:0.08854
#both factors:


fm1c <- lm( Series_Complete_18PlusPop_Pct~ rep_pct + dem_pct, data = ele)
summary(fm1c)
plot(fm1c)
#Series_Complete_18PlusPop_Pct = 1288.2 - 1290.8rep_pct - 1268.7*dem_pct, Adjusted R-squared:  0.2535 

#negative correlation, republican and vaccination rate
cor.test(ele$rep_pct, ele$Series_Complete_18PlusPop_Pct, method="spearman")
cor.test(ele$rep_pct, ele$Series_Complete_12PlusPop_Pct, method="spearman")
cor.test(ele$rep_pct, ele$Series_Complete_65PlusPop_Pct, method="spearman")

#positive correlation 
cor.test(ele$dem_pct, ele$Series_Complete_18PlusPop_Pct, method="spearman")
cor.test(ele$dem_pct, ele$Series_Complete_12PlusPop_Pct, method="spearman")
cor.test(ele$dem_pct, ele$Series_Complete_65PlusPop_Pct, method="spearman")

