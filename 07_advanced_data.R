# practical session introducing the data.table package
# and working with a large data.set


library(data.table)
help(data.table)


# data table basics
# data.table is a wrapper for data.frame, with enhanced features
# a data.table is always a data.frame, with all its capabilities

data(mtcars)

data_mtcars<-data.table(mtcars)

data_mtcars  # better display, but lost rownames!

data_mtcars<-cbind(model=rownames(mtcars),data_mtcars) # still a data.table

data_mtcars[mpg>=20,list(model,mpg,hp)]   # no need to preceed with $ the variable names
data_mtcars[mpg>=20,c('model','mpg','hp')]

# we can make groupped opperation on the fly
data_mtcars[,carb:=as.factor(carb)]
data_2<-data_mtcars[mpg>=20,list(n=.N,mean_mpg=mean(mpg),mean_hp=mean(hp)),by='carb']
data_2

# add a new column

data_mtcars[,new_variable:=seq(1,nrow(mtcars))]
data_mtcars[mpg>=20,new_variable2:='mpg >= 20']

# delete a column
data_mtcars[,new_variable:=NULL]


# merges with data.table

data_3<-data.table(x=runif(6),model=data_mtcars$model[sample(nrow(data_mtcars),6)])

setkeyv(data_mtcars,'model')
setkeyv(data_3,'model')

# inner join
data_3[data_mtcars, nomatch=0]
data_mtcars[data_3, nomatch=0]
data_3[data_mtcars, nomatch=NA]

# left join
data_mtcars[data_3]
data_3[data_mtcars]

# duplicated treatment
data_mtcars[!duplicated(carb)]
data_mtcars[!duplicated(model)]





# https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# 
# 
# https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt



library(readxl)

setwd("C:/Users/sirio/Documents/IE - R Programming")

raw_data<-data.table(read_excel('data/AmesHousing.xlsx'))
# raw_data<-fread('AmesHousing.csv',stringsAsFactors = T)

str(raw_data)


# change all column names eliminating spaces
names(raw_data)<-gsub(" ", "_", names(raw_data), fixed = TRUE)

# set all character columns to factor variables

char_to_factor<-function(var){
  if(is.character(var)){
    var<-as.factor(as.character(var))
  } else {
    return(var)
  }
}

# the traditional way
raw_data<-data.frame(raw_data)
for ( i in 1:ncol(raw_data)){
  if(is.character(raw_data[,i])){
    raw_data[,i]<-as.factor(raw_data[,i])
  }
}

str(raw_data)

# the data.table way
# rerun from line 73 to 80
raw_data[, names(raw_data)[sapply(raw_data,is.character)]:= lapply(.SD, factor), .SDcols=sapply(raw_data,is.character)]

length(unique(raw_data$PID))

ggplot(raw_data,aes(x=`Lot Area`,y=SalePrice,colour=Neighborhood)) + geom_point()

raw_data[,.(mean_price=mean(SalePrice),n=.N),by=Neighborhood][order(-mean_price)]


summary(raw_data$`Yr Sold`)

# eliminate all whitespaces in variable names
names(raw_data)<-gsub(pattern =' ' , replacement ='' , x=names(raw_data))


raw_data[,.(mean_price=mean(SalePrice),n=.N),by=YrSold][order(YrSold)]

raw_data[,.(mean_price=mean(SalePrice),n=.N),by=YearBuilt][order(YearBuilt)]


ggplot(raw_data,aes(x=LotArea,y=SalePrice,colour=factor(YearBuilt))) + geom_point()
ggplot(raw_data,aes(x=YearBuilt,y=SalePrice,colour=-YearBuilt)) + geom_point()


raw_data[,.(mean_price=mean(SalePrice),n=.N),by=ExterQual][order(mean_price)]

raw_data[,.(mean_price=mean(SalePrice),n=.N),by=HeatingQC][order(mean_price)]

raw_data[,.(mean_price=mean(SalePrice),n=.N),by=HeatingQC][order(mean_price)]


with(raw_data,table(HeatingQC,ExterQual))
