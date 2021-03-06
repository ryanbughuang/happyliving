library(DescTools)
library(tidyverse)
library(ggmosaic)

path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',')


agg_health_risk = info[ info$n3_health_risk > 0, c(23:28,282)] 
#只選self_living>0的row ; 22:26=只選self_living相關的風險
basic_info = info[,c(270:272,282:287)] #只選填答者基本資料

agg_health_risk = agg_health_risk %>% gather(key, value, -response) %>% arrange(response)
agg_health_risk = filter(agg_health_risk,agg_health_risk$value == 1) %>% subset(,select = -value)
names(agg_health_risk)[names(agg_health_risk) == "key"] <- "answered_field"
agg_health_risk = merge.data.frame(agg_health_risk,basic_info,by='response')
agg_health_risk[,-1] = sapply(agg_health_risk[,-1],as.factor)



### Moassic ###

ggplot(data = agg_health_risk) +
  geom_mosaic(aes(weight = response, x = product(answered_field,elderly2,gender,livingstatus), fill = elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### GLM ###
# 轉換為glm可以跑的形狀
winney_glm = aggregate(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = agg_health_risk, FUN = length)

# 各種glm model
winney_glm_model = glm(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = winney_glm, family = 'poisson')
addterm(winney_glm_model, ~. + answered_field:(gender + cities + elderly2 + geo + livingstatus), test = 'Chisq')
winney_glm_model2 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus))
addterm(winney_glm_model2, ~. + gender:(elderly2 + geo + livingstatus), test = 'Chisq')
winney_glm_model3 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus))
addterm(winney_glm_model3, ~. + elderly2:(geo + livingstatus), test = 'Chisq')
winney_glm_model4 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus))
addterm(winney_glm_model4, ~. + geo:livingstatus, test = 'Chisq')
winney_glm_model5 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus)
addterm(winney_glm_model5, ~. + answered_field:gender:(cities + elderly2 + geo + livingstatus), test = 'Chisq')
winney_glm_model6 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus)
addterm(winney_glm_model6, ~. + answered_field:cities:(elderly2 + livingstatus), test = 'Chisq')
winney_glm_model7 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus + answered_field:cities:elderly2 + answered_field:cities:livingstatus)
addterm(winney_glm_model7, ~. + answered_field:elderly2:livingstatus, test = 'Chisq')
winney_glm_model8 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus)
addterm(winney_glm_model8, ~. + gender:cities:(elderly2 + geo + livingstatus), test = 'Chisq')
winney_glm_model9 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus + gender:cities:livingstatus)
addterm(winney_glm_model9, ~. + gender:elderly2:( geo + livingstatus), test = 'Chisq')
winney_glm_model10 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus)
addterm(winney_glm_model10, ~. + cities:elderly2:livingstatus, test = 'Chisq')
winney_glm_model11 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:gender:livingstatus + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus + cities:elderly2:livingstatus)
winney_glm_model12 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus + cities:elderly2:livingstatus)
winney_glm_model13 = update(winney_glm_model12, .~. - answered_field:(gender + elderly2 ) )

dropterm(winney_glm_model13, test = 'Chisq')
summary(winney_glm_model12)
summary(winney_glm_model13)
anova_result = anova(winney_glm_model12,test = 'Chisq')
1-pchisq(deviance(winney_glm_model13), df.residual(winney_glm_model13))



### 給琮仁做預測model ###
winney_names <- lapply(winney_glm[,-7], unique) # omit Freq

winney_glm_pm <- predict(winney_glm_model13, expand.grid(winney_names), type='response') # poisson means
all_predict = expand.grid(winney_names)
all_predict$predicted = winney_glm_pm
all_predict = all_predict[all_predict$elderly2 != 0,] #不預測NA
all_predict_wide = spread(all_predict,key = 'answered_field',value = 'predicted')
all_predict_wide$total = apply(all_predict_wide[,c(6:11)],1,sum)
all_predict_wide_pr = cbind(all_predict_wide[,c(1:5)],all_predict_wide[,c(6:11)]/all_predict_wide[,12])


### TP moasic ###
tp_predict = all_predict[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'), ]

tp_wide = all_predict_wide_pr[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'),]
tp_long = gather(tp_wide, key = "answered_field", value = "predicted", t1_health_risk1, t1_health_risk2, t1_health_risk3, t1_health_risk4, t1_health_risk5,t1_health_risk6)
tp_wide %>% as_tibble() %>% gather(answered_field, predicted, t1_health_risk1, t1_health_risk2, t1_health_risk3, t1_health_risk4 )
tp_wide[,c(6:11)] = round(tp_wide[,c(6:11)],2)
write_csv(tp_wide, '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/health_risk.csv')


ggplot(data = tp_long) +
  geom_mosaic(aes(weight = predicted, x = product(gender,elderly2,answered_field,livingstatus), fill=elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





