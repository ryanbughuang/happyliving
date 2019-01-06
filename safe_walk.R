library(DescTools)
library(tidyverse)
library(ggmosaic)

path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',')


######################
#### reshape data ####
######################
agg_safe_walk = info[ info$n3_safe_walk > 0, c(47:51,282)]
#只選save_walking>0的row ; 47:51=只選save_walking相關的風險
basic_info = info[,c(270:272,282:287)] #只選填答者基本資料
agg_safe_walk = agg_safe_walk %>% gather(key, value, -response) %>% arrange(response)
agg_safe_walk = filter(agg_safe_walk,agg_safe_walk$value == 1) %>% subset(,select = -value)
names(agg_safe_walk)[names(agg_safe_walk) == "key"] <- "answered_field"
agg_safe_walk = merge.data.frame(agg_safe_walk,basic_info,by='response')
agg_safe_walk[,-1] = sapply(agg_safe_walk[,-1],as.factor)
attach(agg_safe_walk)

#######################
#### Sample Moassic####
#######################
ggplot(data = agg_safe_walk) +
  geom_mosaic(aes(weight = response, x = product(answered_field,elderly2,gender,livingstatus), fill = elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######################
##### GLM Model #####
######################
# 轉換為glm可以跑的形狀
winney_glm = aggregate(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = agg_safe_walk, FUN = length)

# 各種glm model
winney_glm_model = glm(response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, data = winney_glm, family = 'poisson')
addterm(winney_glm_model, ~. + answered_field:(gender + cities + elderly2 + geo + livingstatus), test = 'Chisq')
winney_glm_model12 = update(winney_glm_model, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus + cities:elderly2:livingstatus)
# 檢測glm model
dropterm(winney_glm_model12, test = 'Chisq')
summary(winney_glm_model12)
anova_result = anova(winney_glm_model12,test = 'Chisq')
1-pchisq(deviance(winney_glm_model12), df.residual(winney_glm_model12))


##### GLM Prediction #####
winney_names <- lapply(winney_glm[,-7], unique) # omit Freq
winney_glm_pm <- predict(winney_glm_model12, expand.grid(winney_names), type='response') # poisson means
all_predict = expand.grid(winney_names)
all_predict$predicted = winney_glm_pm
all_predict = all_predict[all_predict$elderly2 != 0,] #不預測NA
all_predict_wide = spread(all_predict,key = 'answered_field',value = 'predicted') #長表轉寬
all_predict_wide$total = apply(all_predict_wide[,c(6:10)],1,sum) # 將同x下數量轉成比例
all_predict_wide_pr = cbind(all_predict_wide[,c(1:5)],all_predict_wide[,c(6:10)]/all_predict_wide[,11]) # 將同x下數量轉成比例

#######################################
### Taipei Prediction & Moasic plot ###
#######################################
tp_predict = all_predict[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'), ]

tp_wide = all_predict_wide_pr[ (all_predict_wide_pr$geo == 'north') & (all_predict_wide_pr$cities == '1'),]
tp_long = gather(tp_wide, key = "answered_field", value = "predicted", t5_safe_walk1, t5_safe_walk2, t5_safe_walk3, t5_safe_walk4, t5_safe_walk5)
tp_wide[,c(6:10)] = round(tp_wide[,c(6:10)],2)
write_csv(tp_wide, '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/safe_walking.csv')

# moasic plot for all field
ggplot(data = tp_long) +
  geom_mosaic(aes(weight = predicted, x = product(gender,elderly2,answered_field,livingstatus), fill=elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data = tp_long) +
  geom_mosaic(aes(weight = predicted, x = product(gender,livingstatus,answered_field,elderly2), fill=livingstatus, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# moasic plot for selected field
ggplot(data = tp_long[(tp_long$answered_field == 't5_safe_walk1') | (tp_long$answered_field == 't5_safe_walk4') ,]) +
  geom_mosaic(aes(weight = predicted, x = product(gender,elderly2,answered_field,livingstatus), fill=elderly2, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = tp_long[(tp_long$answered_field == 't5_safe_walk1') | (tp_long$answered_field == 't5_safe_walk4') ,]) +
  geom_mosaic(aes(weight = predicted, x = product(gender,livingstatus,answered_field,elderly2), fill=livingstatus, na.rm=TRUE,offset = 0.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

detach(agg_safe_walk)



