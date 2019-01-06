library(jsonlite)
library(tidyverse)
library(stringr)
#install.packages('devtools')
library(devtools)
#devtools::install_github("haleyjeppson/ggmosaic")
#devtools::install_github("stefanedwards/lemon")
library(ggmosaic)

my.df <- fromJSON("/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/問卷題目.json")
path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',') #fileEncoding='big5' or fileEncoding='utf8'


###################################
#########     新增欄位    #########
###################################

# 將年齡分為長者/非長者
elderly <- function(x){
  if (is.na(x)) {
    return(0);
  }else if (x>9){
    return(1);
  }else{
    return(0);
  }}
info$elderly = sapply(info$age, elderly)

# 將年齡分成四個區間
elderly2 <- function(x){
  if (is.na(x)){
    return(0);
  }else if (x<=6){
    return(1)
  }else if (x<=9){
    return(2)
  }else{
    return(3)
  }
}
info$elderly2 = sapply(info$age, elderly2)

# 將居住地分為都市vs非都市
isCity <- function(x){
  citylist = c(1,2,4,8,14,15)
  if (is.na(x)){
    return(0);
  }else if (x %in% citylist){
    return(1)
  }else return(0)
}
info$city = sapply(info$countycode,isCity)
# 標記六都
sixCity <- data_frame(cities = c(1:6),
                      countycode = c(1,2,4,8,14,15))
# 非六都給0
info <- merge.data.frame(info,sixCity,by='countycode',all.x = TRUE)
info['cities'][is.na(info['cities'])] <- 0

# 分為北、中、南、東、離島
geo <- function(x){
  if(x %in% c(1,2,3,4,5,6,17)){
    return('north')
  }else if (x %in% c(7,8,9,10,11)){
    return('central')
  }else if (x %in% c(12,13,14,15,16)){
    return('south')
  }else if(x %in% c(18,19)){
    return('east')
  }else if(x %in% c(20,21,22)){
    return('island')
  }else if(is.na(x)){
    return('None')
  }
}
county = data.frame(my.df['county']) #json中的countycode - city 對照表
county$geo <-  sapply(county$county.id,geo) #將countycode對照北中南東
#以code對照（field_info的第一行以及county的第一行），將北中南東對應field_info
colnames(info)
info <- merge.data.frame(info,subset(county,select = c(1,3)),by.x = 1,by.y = 1)
info <- as.data.frame(lapply(info, unlist)) #將list轉為vector


###################################
#########    EDA Graph    #########
###################################

# field_info: 大領域選擇＋各種x
field_info = read.table('/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/field_basicinfo.csv', sep = ',',header = TRUE)


#領域v.s age，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(age))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(age))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 是否老年，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(elderly))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(elderly))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 年齡分組，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(elderly2))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(elderly2))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 性別，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(gender))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(gender))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s county，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(countycode))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(countycode))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 六都，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(cities))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(cities))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 地理區，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(geo))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(geo))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 居住型態，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(answered_field), fill = factor(livingstatus))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(answered_field), fill = factor(livingstatus))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))


#################
### GLM Model ###
#################

# reshpae data
big_field = info[,c(13:22,282)] #只選n3 columns
basic_info = info[,c(270:272,282:287)] #只選填答者基本資料
big_field = big_field %>% gather(answered_field, value, -response) %>% arrange(response) #寬表格轉長
big_field = filter(big_field,big_field$value == 1) %>% subset(,select = -value)
big_field = merge.data.frame(big_field,basic_info,by='response') #合併大表格以及基本資料
big_field[,-1] = sapply(big_field[,-1], as.factor) #將x全部轉成factor

# 計數同樣x下 十大領域的累積人數
big_field_glm = aggregate(data=big_field,response ~ answered_field + gender + cities + elderly2 + geo + livingstatus, FUN = length)
names(big_field_glm)[names(big_field_glm) == "response"] <- "response_count"

# 開跑GLM
big_field_glm_model1 = glm(response_count~answered_field +gender+cities+elderly2+geo+livingstatus, data = big_field_glm, family = 'poisson')
addterm(big_field_glm_model1, ~. + answered_field*(gender + cities + elderly2 + geo + livingstatus)^2, test = 'Chisq')
big_field_glm_model2 = update(big_field_glm_model1, .~. + answered_field:(gender + cities + elderly2 + livingstatus)^2 + answered_field:gender:elderly2 + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus + cities:elderly2:livingstatus)
big_field_glm_model3 = update(big_field_glm_model1, .~. + answered_field:(gender + cities + elderly2 + livingstatus) + gender:(elderly2 + geo + livingstatus) + elderly2:(geo + livingstatus) + geo:livingstatus + answered_field:gender:elderly2 + answered_field:cities:elderly2 + answered_field:cities:livingstatus + answered_field:elderly2:livingstatus + gender:cities:livingstatus + gender:elderly2:livingstatus + cities:elderly2:livingstatus)

# 檢測模型
summary(big_field_glm_model2)
summary(big_field_glm_model3)
anova_result = anova(big_field_glm_model3,test = 'Chisq')
1-pchisq(deviance(big_field_glm_model3), df.residual(big_field_glm_model3))

############################################
### Chisq test for elderly and big field ###
############################################
elderly_field = aggregate(response ~ answered_field + elderly, data = field_info, FUN = length)
pairwise.prop.test(xtabs(response~answered_field+elderly,data= elderly_field))



