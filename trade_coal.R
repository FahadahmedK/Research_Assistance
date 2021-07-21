rm(list = ls())
setwd('D:/Work/Research/Miriam')

library(dplyr)
library(haven)
library(ggplot2)

a <- read.csv('62-66.csv', header = TRUE)
b<- read.csv('67-71.csv', header = TRUE)
c <- read.csv('72-76.csv', header = TRUE)
d <- read.csv('77-82.csv', header = TRUE)
e <- read.csv('83-87.csv', header = TRUE)
f <- read.csv('88-92.csv', header = TRUE)
g  <- read.csv('93-97.csv', header = TRUE)
h <- read.csv('98-02.csv', header = TRUE)
i<- read.csv('03-07.csv', header = TRUE)
j <- read.csv('08-10.csv', header = TRUE)


data <- rbind(a,b,c,d,e,f,g,h,i,j)

data <- data[, c('Year', 'Trade.Flow', 'Reporter', 'Partner', 'Trade.Value..US..')]

colnames(data) <- c('year', 'flow', 'reporter', 'partner', 'volume')
data$reporter <- as.character(data$reporter)
data$partner <- as.character(data$partner)

data$reporter <- replace(data$reporter, data$reporter== 'Fmr Fed. Rep. of Germany', 'Germany' )
data$partner <- replace(data$partner, data$partner== 'Fmr Fed. Rep. of Germany', 'Germany' )
data$partner <- replace(data$partner, data$partner== 'Viet Nam', 'Vietnam' )
data$partner <- replace(data$partner, data$partner== 'USA (before 1981)', 'USA' )

data_world <- data[data$partner == 'World', ]

data <- data[!(data$partner == 'World'),]

data_60 <- data[data$year== 1962| data$year == 1963| data$year ==1964 | data$year == 1965 | data$year == 1966 | data$year == 1967 | data$year == 1968 | data$year == 1969,] 
data_70 <- data[data$year == 1970| data$year == 1971| data$year ==1972|data$year ==1973|data$year ==1974|data$year ==1975|data$year ==1976|data$year ==1977|data$year ==1978|data$year ==1979,] 
data_80 <- data[data$year == 1980|data$year ==1981|data$year ==1982|data$year ==1983|data$year ==1984|data$year ==1985|data$year ==1986|data$year ==1987|data$year ==1988|data$year ==1989,] 
data_90 <- data[data$year == 1990|data$year ==1991|data$year ==1992|data$year ==1993|data$year ==1994|data$year ==1995|data$year ==1996|data$year ==1997|data$year ==1998|data$year ==1999,] 
data_00 <- data[data$year == 2000|data$year ==2001|data$year ==2002|data$year ==2003|data$year ==2004|data$year ==2005|data$year ==2006|data$year ==2007|data$year ==2008|data$year ==2009|data$year ==2010,] 

#max5_60 <- data.frame()
#for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
#  a <- data_60[data_60$reporter == i,] %>% arrange(desc(volume))
#  max5_60<- rbind(max5_60, head(a,5))
#}
#write_dta(max5_60, 'max5_60.dta')



rel_shares <- function(data, decade) {
  final <- data.frame()
  for(i in unique(data$reporter)) {
    x<-data.frame('reporter' = rep(i, length(unique(data[data$reporter == i,]$partner))))
  
    for(j in 1:length(unique(data[data$reporter == i,]$partner))) {
      x$partner[j] <- unique(data[data$reporter==i,]$partner)[j]
      x$total[j] <- sum(data[data$reporter==i & data$partner == x$partner[j],]$volume) 
      #x$share[j] <- round((x[j, 'total'])/sum(x[x$reporter==i,'total']), digits = 4)
    }
    x <- x %>% arrange((desc(total)))
    final <-rbind(final, x)
  }

  for(k in 1:nrow(final)) {
    final$share[k] <- round(final[final$reporter == final$reporter[k] & final$partner == final$partner[k],]$total / sum(final[final$reporter == final$reporter[k],]$total), digits = 4)
  }
  max6 <- data.frame()
  for(l in unique(data$reporter)) {
    max6<- rbind(max6,head(final[final$reporter == l, ],6))
  }
  return(max6)
}

max6_60 <- rel_shares(data_60)
write_dta(max6_60, 'max_60.dta')

max6_70 <- rel_shares(data_70)
write_dta(max6_70, 'max_70.dta')

max6_80 <- rel_shares(data_80)
write_dta(max6_80, 'max_80.dta')

max6_90 <- rel_shares(data_90)
write_dta(max6_90, 'max_90.dta')

max6_00 <- rel_shares(data_00)
write_dta(max6_00, 'max_00.dta')




#sum(data_70[data_70$reporter == 'United Kingdom' & data_70$partner == 'Ireland',]$volume) /sum(data_70[data_70$reporter == 'United Kingdom',]$volume)


#x<-data.frame('reporter' = rep('France', length(unique(data_60[data_60$reporter == 'France',]$partner))))
#for(j in 1:length(unique(data_60[data_60$reporter == i,]$partner))) {
#  x$partner[j] <- unique(data_60$partner)[j]
#  x$total[j] <- sum(data_60[data_60$reporter==i & data_60$partner == x$partner[j],]$volume)
#}


#x<- data.frame('reporter' = 'France')
#x$partner <- 'Belgium-Luxembourg'

#x$total <- sum(data_60[data_60$reporter==x$reporter[1] & data_60$partner ==x$partner[1],]$volume)

#sum(data_60[data_60$reporter == 'France' & data_60$partner == data_60$partner[1],]$volume)/sum(data_60[data_60$reporter == 'France',]$volume)
#head(data_60)

#max5_70 <- data.frame()
#for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
#  a <- data_70[data_60$reporter == i,] %>% arrange(desc(volume))
#  max5_70<- rbind(max5_70, head(a,5))
#}
#write_dta(max5_70, 'max5_70.dta')


#max5_80 <- data.frame()
#for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
#  a <- data_80[data_80$reporter == i,] %>% arrange(desc(volume))
#  max5_80<- rbind(max5_80, head(a,5))
#}
#write_dta(max5_80, 'max5_80.dta')

#max5_90 <- data.frame()
#for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
#  a <- data_90[data_90$reporter == i,] %>% arrange(desc(volume))
#  max5_90<- rbind(max5_90, head(a,5))
#}
#write_dta(max5_90, 'max5_90.csv')

#max5_00 <- data.frame()
#for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
#  a <- data_00[data_00$reporter == i,] %>% arrange(desc(volume))
#  max5_00<- rbind(max5_00, head(a,5))
#}
#write_dta(max5_00, 'max5_00.dta')

#x<- data_60[data_60$reporter == 'United Kingdom' & data_60$year == 1962,]
#max5_60[1,'share'] <- max5_60[1, 'volume'] / sum(data_60[data_60$reporter == max5_60[1,'reporter'] & data_60$year == max5_60[1,'year'],]$volume)


#share <- function(data, maxdata) {
#  for(i in 1:nrow(maxdata)) {
#    maxdata[i, 'share'] <- maxdata[i, 'volume'] / sum(data[data$reporter == maxdata[i,'reporter'] & data$year == maxdata[i,'year'],]$volume)
#    print(maxdata[i, 'volume'] / sum(data[data$reporter == maxdata[i,'reporter'] & data$year == maxdata[i,'year'],]$volume))
#  }
#}

data_west <- data[data$partner == 'Belgium'| data$partner == 'Belgium-Luxembourg' | data$partner =='Netherlands'|data$partner == 'United Kingdom' |data$partner =='Germany' |data$partner =='France'| data$partner == 'Ireland',]
data_us_can <- data[data$partner == 'USA' | data$partner == 'Canada',]
data_col <- data[data$partner == 'Colombia',]
data_soafrica <- data[data$partner == 'South Africa'|data$partner == 'So. African Customs Union' ,]
data_soafrica$partner <- replace(data_soafrica$partner, data_soafrica$partner == 'So. African Customs Union', 'South Africa' )
data_aus <- data[data$partner == 'Australia', ]
data_china <- data[data$partner == 'Australia', ]
data_east <- data[data$partner == 'Poland' |data$partner ==  'Russian Federation' | data$partner == 'Czechia'  | data$partner == 'Czechoslovakia' | data$partner ==  'Fmr USSR'| data$partner == 'Slovakia' | data$partner =='Romania' | data$partner =='Hungary', ]

#WEST 
regional_west <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_west[data_west$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'Western Europe'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_west <- rbind(regional_west, reg)
}


#US_Canada

regional_US_Can <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_us_can[data_us_can$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'US & Canada'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_US_Can <- rbind(regional_US_Can, reg)
}


#Colombia

regional_col <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_col[data_col$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'Colombia'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_col <- rbind(regional_col, reg)
}

#South Africa

regional_SoAfrica <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_soafrica[data_soafrica$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'South Africa'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_SoAfrica <- rbind(regional_SoAfrica, reg)
}

#Australia

regional_aus <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_aus[data_aus$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'Australia'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_aus <- rbind(regional_aus, reg)
}

#China

regional_china <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_china[data_china$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'China'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_china <- rbind(regional_china, reg)
}

#East

regional_east <- data.frame()
for(i in c('France', 'Germany', 'Netherlands', 'United Kingdom')) {
  x <- data_east[data_east$reporter == i, ]
  reg<-data.frame()
  for(j in 1:length(unique(x$year))) {
    reg[j, 'year'] <- unique(x$year)[j]
    reg[j, 'reporter'] <- i
    reg[j, 'region'] <- 'Eastern Europe'
    reg[j, 'trade'] <- sum(x[x$year == unique(x$year)[j],]$volume)
  }
  regional_east <- rbind(regional_east, reg)
}

colnames(data_world)[c(4, 5)] <- c('region', 'trade')
data_world <- subset(data_world, select = -c(2) ) 
data_regional <- rbind(data_world, regional_west, regional_east, regional_US_Can, regional_SoAfrica, regional_west, regional_aus, regional_china, regional_col) %>% arrange(year)
unique(data_regional$region)
data_regional$region <- factor(data_regional$region,
                      levels = c('World', 'Western Europe', 'Eastern Europe',
                                 'US & Canada', 'Australia','China', 'Colombia',
                                 'South Africa'))
ggplot(data_regional[data_regional$reporter == 'France',], aes(x = year, 
                          y= trade, fill = region)) + geom_area() + ggtitle('Coal Imports in France')

ggsave('stacked_france.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'Germany',], aes(x = year, y= trade, fill = region)) + geom_area() + ggtitle('Coal Imports in Germany')
ggsave('stacked_germany.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'Netherlands',], aes(x = year,y= trade, fill = region)) + geom_area() +ggtitle('Coal Imports in Netherlands')
ggsave('stacked_netherlands.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'United Kingdom',], aes(x = year, y= trade, fill = region)) + geom_area() + ggtitle('Coal Imports in United Kingdom')
ggsave('stacked_UK.pdf')
dev.off()


ggplot(data_regional[data_regional$reporter == 'France',], aes(x = year, 
                                                               y= trade, group = region, colour = region)) + geom_line() + ggtitle('Coal Imports in France')
ggsave('lines_france.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'Germany',], aes(x = year, 
                                                               y= trade, group = region, colour = region)) + geom_line() + ggtitle('Coal Imports in Germany')
ggsave('lines_germany.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'Netherlands',], aes(x = year, 
                                                               y= trade, group = region, colour = region)) + geom_line() + ggtitle('Coal Imports in Netherlands')
ggsave('lines_netherlands.pdf')
dev.off()

ggplot(data_regional[data_regional$reporter == 'United Kingdom',], aes(x = year, 
                                                                    y= trade, group = region, colour = region)) + geom_line() + ggtitle('Coal Imports in UK')
ggsave('lines_UK.pdf')
dev.off()