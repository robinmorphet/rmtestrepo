library(ggplot2)
#script to plot calculated origin rents against ward house price data
home<-"C:/Users/Robin/work/"
home <- E:/Arcadia/
#this needs to be changed to the Arcadia data location
# read in price data
priceData<-read.csv(paste(home,"vonThunen/London_Mean_House_Prices.csv",sep=""))
openSpace <- read.csv(paste(home,"vonThunen/London_Mean_House_Prices.csv",sep=""))
house_price_2005 <- priceData$mhp2005
rent_2001 <- priceData$lambdaR
length(house_price_2005)
length(rent_2001)
#modify by trips area and no of dwellings to get equivalent price of land
house_area <-priceData$GLUD_Domestic
origins <- priceData$OR
dwellings <- priceData$dwgs2005

rent_unit_area <- rent_2001*(origins/house_area)
price_unit_area <- house_price_2005* (dwellings/house_area)
ratio <- rent_unit_area /price_unit_area
ratio <- exp(mean(log(ratio)))
#exclude City
 rent_unit_area <- rent_unit_area[2:length(rent_unit_area)]
 price_unit_area <- price_unit_area[2:length(price_unit_area)]
index <- ((rent_unit_area < 4000)+0)* ((ratio*price_unit_area < 4000)+0)
#index <- ((rent_unit_area < 4000)+0)
plot((index*rent_unit_area),(ratio*index*price_unit_area))
dF<-data.frame(index*rent_unit_area,ratio*index*price_unit_area)
cor(dF[,1],dF[,2])
