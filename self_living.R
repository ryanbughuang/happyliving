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

### 給琮仁做預測model ###
winney_names <- lapply(winney_glm[,-7], unique) # omit Freq
winney_glm %>% glimpse()
winney_glm_pm <- predict(winney_glm_model11, expand.grid(winney_names), type='response') # poisson means
a = expand.grid(winney_names)
a$predicted = winney_glm_pm
glimpse(a)
merge.data.frame(winney_glm,a,by=c('answered_field','gender','cities','elderly2','geo','livingstatus')) %>% glimpse()

cbind(expand.grid(n1names[-5]), prob = round(n1_table.pr, 2))

mosaicplot(xtabs(response~elderly2+gender+answered_field, data=agg_self_living), color=TRUE, off = 5,dir='h',las = 2)



### Moassic ###
ggplot(data = agg_self_living) +
  geom_mosaic(aes(weight = response, x = product(elderly2,gender,cities,answered_field), fill=answered_field, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### GLM ###
winney_glm = aggregate(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = agg_self_living, FUN = length)
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
dropterm(winney_glm_model11, test = 'Chisq')

summary(winney_glm_model10)
anova(winney_glm_model11,test = 'Chisq')
1-pchisq(deviance(winney_glm_model10), df.residual(winney_glm_model10))



