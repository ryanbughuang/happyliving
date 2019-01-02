path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',',fileEncoding='big5')
colname = (colnames(info))
self_living = info[ info$n3_self_living > 0,]
