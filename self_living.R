install.packages('DescTools')
library(DescTools)
path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',')
info$response = c(1:68320)
colnames(info)
agg_self_living = info[ info$n3_self_living > 0, c(68:73,282)] 
#只選self_living>0的row ; 68:73=只選self_living相關的風險
basic_info = info[,c(270:272,282:287)] #只選填答者基本資料
colnames(agg_self_living)
agg_self_living = agg_self_living %>% gather(key, value, -response) %>% arrange(response)
agg_self_living = filter(agg_self_living,agg_self_living$value == 1) %>% subset(,select = -value)
names(agg_self_living)[names(agg_self_living) == "key"] <- "answered_field"
agg_self_living = merge.data.frame(agg_self_living,basic_info,by='response')
agg_self_living[,-1] = sapply(agg_self_living[,-1],as.factor)
agg_self_living %>% glimpse()
colnames(agg_self_living)
attach(agg_self_living)


### Moassic ###
par(mar=c(8.1,4.1,4.1,8.1))
ggplot(data = agg_self_living) +
  geom_mosaic(aes(weight = response, x = product(elderly2,answered_field,livingstatus,gender), fill = elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### GLM ###
# 轉換為glm可以跑的形狀
winney_glm = aggregate(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = agg_self_living, FUN = length)

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

#dropterm(winney_glm_model11, test = 'Chisq')
#summary(winney_glm_model13)
#anova_result = anova(winney_glm_model12,test = 'Chisq')
#1-pchisq(deviance(winney_glm_model12), df.residual(winney_glm_model12))
#PseudoR2(winney_glm_model12)


### 給琮仁做預測model ###
winney_names <- lapply(winney_glm[,-7], unique) # omit Freq

winney_glm_pm <- predict(winney_glm_model12, expand.grid(winney_names), type='response') # poisson means
all_predict = expand.grid(winney_names)
all_predict$predicted = winney_glm_pm
all_predict = all_predict[all_predict$elderly2 != 0,] #不預測NA
all_predict_wide = spread(all_predict,key = 'answered_field',value = 'predicted')
all_predict_wide$total = apply(all_predict_wide[,c(6:11)],1,sum)
all_predict_wide_pr = cbind(all_predict_wide[,c(1:5)],all_predict_wide[,c(6:11)]/all_predict_wide[,12])


### TP moasic ###
tp_predict = all_predict[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'), ]

tp_wide = all_predict_wide_pr[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'),]
tp_long = gather(tp_wide, key = "answered_field", value = "predicted", t9_self_living1, t9_self_living2, t9_self_living3, t9_self_living4, t9_self_living5, t9_self_living6)

colnames(tp_wide)
tp_wide[,c(6:11)] = round(tp_wide[,c(6:11)],2)

mosaicplot(xtabs(predicted~livingstatus+answered_field+gender+elderly2, data=tp_long), color=TRUE, off = 5,dir='h',las = 2)

ggplot(data = tp_long) +
  geom_mosaic(aes(weight = predicted, x = product(gender,elderly2,answered_field,livingstatus), fill=elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



