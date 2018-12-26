library(jsonlite)
library(tidyverse)
library(stringr)
my.df <- fromJSON("/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/問卷題目.json")
path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',',fileEncoding='big5')
final_field <-  info[,12:21] #填答者最終選擇的兩個領域


###################################
#########     新增欄位    #########
###################################


# 把寬表格轉成長表格
basic_info = info[,269:279] #只選填答者基本資料
basic_info = subset(basic_info, select = c(1:3,11)) #只選性別、年齡、住況、住地
basic_info$response = c(1:68323)
final_field <-  info[,12:21]

a = final_field %>% as_tibble()
a %>% glimpse()
field = a %>% mutate(response = row_number()) %>% select(response, everything()) %>%
  gather(key, value, -response) %>% arrange(response)
field <- filter(field,value == 1) %>% subset(,select = -value)

# 將長表格與填答者基本資料合併
field_info <- merge.data.frame(field,basic_info,by='response')

# 將年齡分為長者/非長者
elderly <- function(x){
  if (is.na(x)) {
    return(0);
  }else if (x>9){
    return(1);
  }else{
    return(0);
  }}
field_info$elderly = sapply(field_info$age, elderly)

# 將居住地分為都市vs非都市
isCity <- function(x){
  citylist = c(1,2,4,8,14,15)
  if (is.na(x)){
    return(0);
  }else if (x %in% citylist){
    return(1)
  }else return(0)
}
field_info$city = sapply(field_info$countycode,isCity)
# 標記六都
sixCity <- data_frame(cities = c(1:6),
                      countycode = c(1,2,4,8,14,15))
# 非六都給0
field_info <- merge.data.frame(field_info,sixCity,by='countycode',all.x = TRUE)
field_info['cities'][is.na(field_info['cities'])] <- 0

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
county = data.frame(my.df['county']) #json中的code - city 對照表
county$geo <-  sapply(county$county.id,geo) #將code對照北中南東
#以code對照（field_info的第一行以及county的第一行），將北中南東對應field_info
field_info <- merge.data.frame(field_info,subset(county,select = c(1,3)),by.x = 1,by.y = 1)
field_info <- as.data.frame(lapply(field_info, unlist)) #將list轉為vector

field_info <-  field_info[,c(2,1,3,4,5,6,7,8,9,10)]
field_info <- field_info[order(field_info$response),]
names(field_info)[names(field_info) == "key"] <- "answered_field"

###################################
#########    EDA Graph    #########
###################################

#排序每個領域選的人數並做barchart
final_field_sum <- apply(final_field,MARGIN = 2,FUN = sum) # 計算各領域選擇人數的總和
field_count = data.frame(row.names = NULL,
                         count = final_field_sum,
                         fields = colnames(final_field))

a = ggplot(data = field_count, aes(x = reorder(fields, -count), y = count)) 
a = a + theme(axis.text.x = element_text(angle = 90, hjust = 1))
a = a + geom_bar(stat = "identity")

#領域v.s age，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(key), fill = factor(age))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(key), fill = factor(age))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 是否>65歲，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(key), fill = factor(elderly))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(key), fill = factor(elderly))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 六都，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(key), fill = factor(cities))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(key), fill = factor(cities))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#領域v.s 地理區，第一張絕對人數(position = 'stack')、第二張相對比例(position = 'fill')
ggplot(field_info,aes(factor(key), fill = factor(geo))) + geom_bar(stat = 'count',position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(field_info,aes(factor(key), fill = factor(geo))) + geom_bar(stat = 'count',position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))


