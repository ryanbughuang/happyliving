path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',',fileEncoding='big5')
info$response = c(1:68323)
self_living = info[ info$n3_self_living > 0, c(22:27,282)]
View(self_living)
colnames(basic_info)
basic_info = info[,269:279] #只選填答者基本資料
#apply(basic_info[,c(4:8)],1,'sum') #living status detail是複選^^
basic_info = subset(basic_info, select = c(1:3,11)) #只選性別、年齡、住況、住地


