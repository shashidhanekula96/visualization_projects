#Project 2


firearms <- read.csv("C:/Users/shash/nics-firearm-background-checks/data/nics-firearm-background-checks.csv",stringsAsFactors = FALSE)
View(firearms)

#Map graph
firearms_map <- firearms[,c(1,2,3)]
firearms_map<-firearms_map %>%
  separate(month, sep="-", into = c("Year", "Month"))

firearms_new<- firearms_map %>% drop_na() %>% select(state,Year,permit) %>%
  group_by(state,Year) %>% summarise(Total_permit=sum(permit))

View(firearms_new)

firearms_pivot <- dcast(firearms_new, state ~ Year, value.var="Total_permit", fun.aggregate=sum)
write.csv(firearms_pivot,'firearms_pivot.csv')

#2 Firearms 
firearms_map_1 <-firearms[,c(1,2,27)]
firearms_map_1<-firearms_map_1 %>%
  separate(month, sep="-", into = c("Year", "Month"))

firearms_map_1<- firearms_map_1 %>% drop_na() %>% select(state,Year,totals) %>%
  group_by(state,Year) %>% summarise(Total_firearms=sum(totals))

firearms_map_1_pivot <- dcast(firearms_map_1, state ~ Year, value.var="Total_firearms", fun.aggregate=sum)
write.csv(firearms_map_1_pivot,'firearms_map_1_pivot.csv')

#3 Graph 
 firearms_stacked <- firearms[,c(1,2,5,6,7,8,10:15,19:23)]
 firearms_stacked$prepawn <- rowSums(firearms_stacked[c("prepawn_handgun","prepawn_long_gun","prepawn_other")])
 firearms_stacked$redemption <- rowSums(firearms_stacked[c("redemption_handgun","redemption_long_gun","redemption_other")])
 firearms_stacked$rentals <- rowSums(firearms_stacked[c("rentals_handgun","rentals_long_gun")])
 firearms_stacked$private <- rowSums(firearms_stacked[c("private_sale_handgun","private_sale_long_gun","private_sale_other")])
 firearms_stacked$direct_sale <- rowSums(firearms_stacked[c("handgun","long_gun","other","multiple")])
 
 firearms_stacked<-firearms_stacked %>%
   separate(month, sep="-", into = c("Year", "Month"))
 
 View(firearms_stacked)
 firearms_stacked <- firearms_stacked[,c(1,2,3,19:23)]
 firearms_stacked<- firearms_stacked %>% drop_na() %>%
   group_by(state,Year,Month)
 
 firearms_stacked_melt <-melt(firearms_stacked)
 
 firearms_stacked_melt_1 <- firearms_stacked_melt %>%
   group_by(state,variable) %>% summarise(Total_firearms=sum(value))
 firearms_stacked_melt_1 %>% drop_na()
 
 firearms_stacked_melt_1 <- dcast(firearms_stacked_melt_1,state ~ variable, value.var="Total_firearms", fun.aggregate=sum)


  write.csv(firearms_stacked_melt_1,'firearms_heat.csv')

# graph 4 gun sales
firearms_sales <- firearms[,c(1,2,5,6,8)]

firearms_sales<-firearms_sales %>%
  separate(month, sep="-", into = c("Year", "Month"))

firearms_sales$multiple <- with(firearms_sales, multiple * 2)
firearms_sales$totalsales <- rowSums(firearms_sales[c("handgun","long_gun","multiple")])

firearms_sales_1 <- firearms_sales %>%
  group_by(state) %>% summarise(Total_firearms=sum(totalsales))

firearms_sales_2 <- firearms_sales %>%
  group_by(state,handgun,long_gun,multiple) %>% summarise(Total_firearms=sum(totalsales))

write.csv(firearms_sales_2,'firearms_sales_2.csv')
