# Farmers market Project

library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(zeallot)
library(reshape2)
library(ggrepel)
library(forcats)
library(scales)
library(treemapify)
library(stringr)
library(anchors)


# Importing the data, Removing NA's and removing duplicate records
fmar <- read.csv("C:/Users/shash/Desktop/Courses/Computational Visualization/project_farmers/fmarket (1).csv",stringsAsFactors = FALSE,header = TRUE)
fmar <- fmar %>% mutate_all(na_if,"")
fmar <- fmar %>% mutate_all(na_if," ")
fmar <- distinct(fmar)


# Seggregation of data based on region
Northeast<-c("Maine", "Massachusetts", "Rhode Island", "Connecticut", "New Hampshire", "Vermont", "New York", "Pennsylvania", "New Jersey" )
South<-c("Delaware", "Maryland","West Virginia", "Virginia", "Kentucky", "Tennessee", "North Carolina", "South Carolina", "Georgia", "Alabama", "Mississippi", "Arkansas", "Louisiana", "Florida","Oklahoma","Texas","District of Columbia")
Midwest<-c("Wisconsin","Michigan","Illinois","Indiana","Ohio","North Dakota","South Dakota","Nebraska","Kansas","Minnesota","Iowa","Missouri")
West<-c("Idaho","Montana","Wyoming","Nevada","Utah","Colorado","Arizona","New Mexico","Alaska","Washington","Oregon","California","Hawaii")
Unincorporated<-c("Virgin Islands","Puerto Rico")
##This data has been collected from Wikipedia. The region "Unincorporated" hs not been mentioned on the Wikipedia, it is an addition as there is no mention of the two states on Wikipedia.

# loop create a new column and assign the specific regions accordingly to the data
for(i in 1:length(fmar$State)){
  
  if(fmar$State[i] %in% Northeast){
    fmar$region[i]<-"Northeast" 
  }
  else if(fmar$State[i] %in% South){
    fmar$region[i]<-"South" 
  }
  else if(fmar$State[i] %in% Midwest){
    fmar$region[i]<-"Midwest" 
  }
  else if(fmar$State[i] %in% West){
    fmar$region[i]<-"West" 
  }
  else {
    fmar$region[i]<-"Unincorporated" 
  }
}

# Seggregating the goods based on type

#Starting Visualization 
#1.Basic visualization of top 10 goods in available in markets

colnames(fmar)
farmarket<- fmar[,c("Organic","Bakedgoods","Cheese","Crafts","Flowers","Eggs",
                    "Seafood","Herbs","Vegetables","Honey","Jams","Maple","Meat",
                    "Nursery","Nuts","Plants","Poultry","Prepared","Soap","Trees",
                    "Wine","Coffee","Beans","Fruits","Grains","Juices","Mushrooms",
                    "PetFood","Tofu","WildHarvested")]

farm_long_form <- gather(farmarket, key = "Type_goods", value = measurement)

farm_top_10 <- farm_long_form %>%
              filter(measurement=="Y") %>% 
              group_by(Type_goods) %>% drop_na() %>% 
              summarise(farmers_market_total=n()) %>% arrange(desc(farmers_market_total))%>% top_n(10)

ggplot(data=farm_top_10,aes(x=reorder(Type_goods, farmers_market_total), y=farmers_market_total)) +
  geom_bar(stat="identity", fill="steelblue")+theme_minimal()+coord_flip()+theme_classic(base_size = 16)+
  geom_text(aes(label = farmers_market_total))+
  labs(x = "Goods", y = "Number of Farmer's Markets", fill = NULL, title = "Top 10 Goods sold in Farmer's Markets")

ggsave("Bargoods.jpg",dpi=300,width = 7, height = 5 )

#2.Top 10 states with farmers markets
farmarket<- fmar[,c("FMID","State")]
farm_top_10_state <- distinct(farmarket) %>% group_by(State) %>% drop_na() %>% 
  summarise(farmers_market_state=n()) %>% arrange(desc(farmers_market_state))%>% top_n(10)

ggplot(data=farm_top_10_state,aes(x=reorder(State,farmers_market_state), y=farmers_market_state)) +
  geom_bar(stat="identity", fill="steelblue")+theme_minimal()+coord_flip()+theme_classic(base_size = 16)+
  geom_text(aes(label = farmers_market_state))+
  labs(x = "States", y = "Number of Farmer's Markets", fill = NULL, title = "Top 10 States with most Farmer's Markets")

ggsave("Barstates.png",dpi=300,width = 7, height = 5 )

#3.Pie chart used to illustrate Highest mode of payments

farmarket<- fmar[,c("Credit","WIC","WICcash","SFMNP","SNAP")]
farmarket_longform <- gather(farmarket, key = "Cash_Mode", value = Measurement)

farm_payment <- farmarket_longform %>% 
                filter(Measurement=="Y") %>% 
                group_by(Cash_Mode) %>% drop_na() %>% 
                summarise(Total_count=n())


pie <- ggplot(farm_payment, aes(x="", y=Total_count, fill=Cash_Mode)) + 
  geom_bar(stat="identity", width=1) + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Total_count*100/sum(Total_count)), "%")), position = position_stack(vjust = 0.5)) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Share of Payment methods accepeted in Farmers market")

pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, color = "#666666"))

ggsave("pie.jpg",dpi=300,width = 6, height = 5 )

#4. Line chart Year wise distrubution of payment methods based on new market registration
farmarket_year<- fmar[,c("Credit","WIC","WICcash","SFMNP","SNAP","updateTime")]
farmarket_year_clean1 <- farmarket_year[!is.na(farmarket_year$updateTime), ]

# Function used for cleanining the Date and time column to year
date_cleansing <- function(farmarket_year){
  for(i in 1:nrow(farmarket_year)){
    if(str_length(farmarket_year[i,"updateTime"])>4){
      farmarket_year[i,"updateTime"] <- unlist(strsplit(unlist(strsplit(farmarket_year[i,"updateTime"],' '))[1],'/'))[3]
    }else{
      farmarket_year[i,"updateTime"] <- farmarket_year[i,"updateTime"]
    }
  }
  return(farmarket_year)
}

farmarket_year_clean1 <- date_cleansing(farmarket_year_clean1)

farmarket_year_clean1 <- farmarket_year_clean1[,c("Credit","WIC","WICcash","SFMNP","SNAP","updateTime")]%>% 
  drop_na()%>% group_by(Credit,WIC,WICcash,SFMNP,SNAP,updateTime)

farmarket_year_clean1[farmarket_year_clean1=="Y"]<-1
farmarket_year_clean1[farmarket_year_clean1=="N"]<-0

farmarket_year_clean1$Credit <- as.integer(farmarket_year_clean1$Credit)
farmarket_year_clean1$WIC <- as.integer(farmarket_year_clean1$WIC)
farmarket_year_clean1$WICcash <- as.integer(farmarket_year_clean1$WICcash)
farmarket_year_clean1$SFMNP <- as.integer(farmarket_year_clean1$SFMNP)
farmarket_year_clean1$SNAP <- as.integer(farmarket_year_clean1$SNAP)

farmarket_year_clean1<- melt(farmarket_year_clean1)

farmarket_year_clean1 <- farmarket_year_clean1 %>% filter(value==1) %>% 
  filter(updateTime!= 2020) %>%
  group_by(updateTime,variable) %>% drop_na() %>% 
  summarise(Total_count=sum(value))

farmarket_year_clean1 <- setNames(farmarket_year_clean1, c("Years","Type_of_Payment","Total_Count"))

ggplot(farmarket_year_clean1, aes(x=Years, y=Total_Count, group=Type_of_Payment,colour= Type_of_Payment)) +
  geom_line()+ geom_point()+ ggtitle("Line chart for various payment methods over the for years")

ggsave("linechart.jpg",dpi=300,width = 6, height = 5 )


#5.Stacked BAr chart based on regions (need to do)

fm_heat<- fmar[,c("region","Credit","WIC","WICcash","SFMNP","SNAP")] %>%
  drop_na()%>%
  group_by(Credit,WIC,WICcash,region,SFMNP,SNAP)

fm_heat[fm_heat=="Y"]<-1
fm_heat[fm_heat=="N"]<-0

fm_heat$Credit <- as.integer(fm_heat$Credit)
fm_heat$WIC<- as.integer(fm_heat$WIC)
fm_heat$WICcash<- as.integer(fm_heat$WICcash)
fm_heat$SFMNP <- as.integer(fm_heat$SFMNP)
fm_heat$SNAP <- as.integer(fm_heat$SNAP)

fm_heat <- melt(fm_heat)
fm_heat <- setNames(fm_heat, c("Region","Type_of_Payment","Total_count"))

ggplot(data=fm_heat, aes(x=Region, y=Total_count, fill=Type_of_Payment,label = Total_count)) +
  geom_bar(stat="identity") +labs(title = "Payment modes used in Farmer's Market in each region") +
  scale_fill_brewer(palette="Set3")

ggsave("stackbar.jpg",dpi=300,width = 8, height = 5 )


#6.Popular social media platforms
colnames(fmar)
fm_social <- fmar[,c("region","Website","Facebook","Twitter","Youtube","OtherMedia")]

fm_social <- fm_social %>%group_by(region) %>% 
  summarise(Total_count=n()) %>% arrange(desc(Total_count))%>% top_n(10)

ggplot(data=fm_social,aes(x=reorder(region,Total_count), y=Total_count)) +
  geom_bar(stat="identity", fill="sienna2")+theme_minimal()+theme_classic(base_size = 16)+
  geom_text(aes(label = Total_count))+
  labs(x = "Region", y = "Total_count", fill = NULL, title = "Highest social media presence of markets in Regions")

ggsave("barsocial.jpg",dpi=300,width = 8, height = 5 )


#7.Heat Map

products <- colnames(fmar[29:58])

farm_temp <- replace.value(fmar, products, from='N', to=as.integer(0))
farm_temp <- replace.value(fmar, products, from='Y', to=as.integer(1))
farm_temp[,29:58] <- lapply(farm_temp[,29:58], as.numeric)

farm_temp <- farm_temp %>% group_by(State, region) %>% summarise_at(products, sum, na.rm = TRUE)

farm_temp$Condiments <- rowSums(farm_temp[c("Cheese","Crafts","Honey","PetFood","Jams","Soap","Bakedgoods","Coffee")])

farm_temp$MeatProducts <- rowSums(farm_temp[c("Eggs","Seafood","Meat","Poultry","Prepared")])

farm_temp$Plantbased_food <- rowSums(farm_temp[c("Organic","Vegetables","Nuts","Beans","Fruits","Grains","Juices","Mushrooms","Tofu")])

farm_temp$other <- rowSums(farm_temp[c("Plants","Trees","Flowers","Nursery","WildHarvested","Herbs","Wine","Maple")])

farm_temp_1 <- farm_temp %>% group_by(region) %>% dplyr::summarise_at(c("Condiments", "MeatProducts", "Plantbased_food","other"), sum, na.rm = TRUE)

farm_temp_1<-melt(farm_temp_1)

farm_temp_1 <- setNames(farm_temp_1, c("Region","Goods","Total_Count"))

ggplot(farm_temp_1, aes(Goods,Region, fill= Total_Count))+ ggtitle("Heat Map For goods across the Us farmer market regions") + geom_tile()
ggsave("Heatmap.jpg",dpi=300,width = 6, height = 5 )

#8.Tree map

 ggplot2::ggplot(farm_temp_1, ggplot2::aes(area = Total_Count, fill = Goods, label = Region, subgroup = Region)) +
  geom_treemap()+ geom_treemap_subgroup_border() + geom_treemap_subgroup_text(colour = "white", place = "centre",)

 ggsave("Treemap.jpg",dpi=300,width = 6, height = 5 )
