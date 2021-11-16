# Import packages

install.packages('sf') #Treatment of coordinates
install.packages('lwgeom') # Treatment of coordinates
library('dplyr') # Management of databases
library('tidyverse') # Management of databases
library('sf')
library('lwgeom')
install.packages("ggmap")
library('ggmap')
install.packages("rayshader")
library('rayshader')
library('ggExtra')
install.packages("ggExtra")
library(scales)
install.packages("ggstance")
library(ggstance)
install.packages("ggmap")
library(ggmap)
options("scipen"=100, "digits"=4)
install.packages("ggmap")
library(gridExtra)
install.packages("ggridges")
library(ggridges)
install.packages("viridis")
library(viridis)
install.packages("hexbin")
library(hexbin)
`## DATA WRANGLING

# Set envirorment
setwd('C:/Users/Ricardo Arias/OneDrive - Monash University/FIT5147 - Data Exploration and Visualisation/Project - House pricing/databases')

# Import Properati database
prop <- read.csv(file = 'co_properties.csv') # Import all the database of Colombia 1000x25
prop
# Transform the dataset 
prop_med <- prop %>% filter(prop$l3 == "MedellÃ­n") # Filter out just the records of Medellín. As it is a spanish word R changes the unknow character. 207195x25
prop_med <- prop_med %>% filter(!is.na(lat) & !is.na(lon)) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) # Delete the properties that does not have a coordinate. 90474x24 and change lat and log for a columns of coordinates
prop_med$priceAUD <- case_when(prop_med$currency == 'COP' ~ prop_med$price/2500, 
                                    prop_med$currency == 'USD' ~ prop_med$price/1.3) # Change the currency to AUD 

# There is only one value in that columns, therefore we do not longer need it
unique(prop_med$ad_type)
unique(prop_med$l1)  
unique(prop_med$l2)  
unique(prop_med$l3) 

# There are no values in that columns, therefore we do not longer need it
unique(prop_med$l5) 
unique(prop_med$l6)
unique(prop_med$price_period)

#Comparison between Room and Bedroom. By comparing these columns we understand that shows the same information
unique(prop_med$rooms == prop_med$bedrooms)
sum(is.na(prop_med$bedrooms))/length(prop_med$bedrooms)
sum(is.na(prop_med$rooms))/length(prop_med$rooms)
#We already know that both columns store the same information. But the column bedrooms has more data than rooms
sum(is.na(prop_med$bathrooms))/length(prop_med$bathrooms)

# GRAFICA. 93.4% of the data in surface_total and 91.1% of surface_covered is NA. Therefore it is not useful
sum(is.na(prop_med$surface_total))/length(prop_med$surface_total)
sum(is.na(prop_med$surface_covered))/length(prop_med$surface_covered)

# Translate terms in Spanish
prop$property_type <- case_when(prop$property_type == 'Apartamento' ~ 'Apartment',
                                prop$property_type == 'Local comercial' ~ 'Business Premises', 
                                prop$property_type == 'Casa' ~ 'House', 
                                prop$property_type == 'Oficina' ~ 'Office', 
                                prop$property_type == 'Otro' ~ 'Other', 
                                prop$property_type == 'Lote' ~ 'Lot',
                                prop$property_type == 'Finca' ~ 'Estate',
                                prop$property_type == 'Parqueadero' ~ 'Car Park') # Translate terms 

prop$operation_type <- case_when(prop$operation_type == 'Venta' ~ 'Sell',
                                 prop$operation_type == 'Arriendo' ~ 'Rent', 
                                 prop$operation_type == 'Arriendo temporal' ~ 'Rent') # Translate terms 


#Drop the useless 10 columns. 90474x15
prop_med <- subset(prop_med, select=-c(ad_type, l1, l2, l3, l4, l5, l6, price_period, price, currency, title, description, rooms, surface_total, surface_covered, id, start_date, end_date))

# Import communes dataset
communes <- st_read('Limite_Catastral_de_Comunas_y_Corregimientos/L%C3%ADmite_Catastral_de__Comunas_y_Corregimientos.shp')

#We do not need Object ID and Sector code
communes <- subset(communes, select=-c(OBJECTID, SECTOR))

# Import poverty dataset and transform map to CSR= 4326 format. 407851 x 3
poverty <- st_read('Poverty/ipm_clase_1.shp') %>% st_transform(4326)


# Merging between the 3 dataset in order to create the final data set with all the attributes
prop <- prop_med %>% st_join(communes, join = st_within , left = TRUE, largest = TRUE) # Prop_med and communes
prop <- final_dataset %>% st_join(poverty, join = st_within , left = TRUE, largest = TRUE) # Prop_med, communes and poverty


# Create a class based on the IPM density value
prop$ipm_class <- case_when(prop$ipm > 0 & prop$ipm < 20 ~ "0.1% - 20%", 
                            prop$ipm >= 20 & prop$ipm < 40 ~ "20.1% - 40%",
                            prop$ipm >= 40 & prop$ipm < 60 ~ "40.1% - 60%",
                            prop$ipm >= 60 & prop$ipm < 80 ~ "60.1% - 80%",
                            prop$ipm >= 80 ~ "Greater than 80%",
                            prop$ipm == 0 ~ "0%")


boxplot(prop$price[prop$operation_type == "Sell"], ylab="Price of Sell")
prop1 <- prop[(prop$operation_type == 'Sell' & prop$price < 424000) | (prop$operation_type == 'Rent' & prop$price < 1968),]
all.na <- apply(prop1, 1, function(x){all(is.na(x))})
prop1 <- prop1[!all.na,]

write.csv(prop,"prop_medellin_no_outliers.csv", row.names = FALSE)

# Import the database with all the merging. So we do not have to do all again
prop <- read.csv(file = 'prop_medellin_no_outliers.csv') # Import all the database of Colombia 1000x25
communes <- st_read('Limite_Catastral_de_Comunas_y_Corregimientos/L%C3%ADmite_Catastral_de__Comunas_y_Corregimientos.shp')
communes <- subset(communes, select=-c(OBJECTID, SECTOR))
poverty <- st_read('Poverty/ipm_clase_1.shp') %>% st_transform(4326)


# Data Checking
# Number of properties per year
prop_month <- prop %>% count(format(as.Date(prop$date, format = '%d/%m/%y'),"%m"))
names(prop_month) <- c('month', 'properties')

# Plot 1 - Number of properties per month
ggplot(data=prop_month, aes(x=month, y=properties)) + geom_bar(stat="identity", width=0.8) + 
  geom_text(aes(label=properties), vjust=1.6, color="white", size=3) + 
  theme_minimal() + ggtitle("Properties published by month") 

# Plot 2 - Bedrooms vs. Bathrooms
p <- ggplot(prop, aes(x=bedrooms, y=bathrooms)) + geom_point() + theme_minimal() + ggtitle("Bedrooms vs. Bathrooms")
ggMarginal(p, type="boxplot")

# Plot 3
prop_type <- prop[prop$property_type != "Car Park",] %>% group_by(property_type) %>% count() %>% ungroup() %>% 
  mutate(per=`n`/sum(`n`)) 
names(prop_type) <- c('type', 'qty', 'perc')
prop_type$perc <- 

piepercent<- paste(prop_type$type, "-",prop_type$perc)

pie(prop_type$qty, labels = piepercent, main = "Distribution of Type of Property",col = rainbow(7))+
legend("topright", prop_type$type, cex = 0.8, fill = rainbow(7))


# Plot 4
# Number of properties per year
operation <- prop %>% group_by(operation_type) %>% count() %>% ungroup() %>% 
  mutate(per=`n`/[prop$operation_type == "Rent",])sum(`n`)) 
names(operation) <- c('type', 'qty', 'perc')
operation$perc <- scales::percent(operation$perc)

piepercent<- paste(operation$type, "-",operation$perc)

pie(operation$qty, labels = piepercent, main = "Distribution of Type of Operation",col = rainbow(7))+
  legend("topright", operation$type, cex = 0.8, fill = rainbow(7))


# Plot 5

p1 <- ggplot(prop[prop$operation_type == "Rent",]) + geom_density(aes(x=price)) + 
  xlab("Rent Price") + ylab("Density") + ggtitle("Density Plot of Price of Rent (AUD)") +
  theme_minimal() + geom_vline(xintercept = median(prop[prop$operation_type == "Rent",]$price), linetype=3)

# Plot 6
p2 <- ggplot(prop[prop$operation_type == "Sell",]) + geom_density(aes(x=price)) + 
  xlab("Sell Price") + ylab("Density") + ggtitle("Density Plotlot of Price of Sell (AUD)") +
  theme_minimal() + geom_vline(xintercept = median(prop[prop$operation_type == "Sell",]$price), linetype=3)

grid.arrange(p1, p2)


# Number of properties per commune
prop_commmune <- prop %>% count(prop$commune_name)
names(prop_commmune) <- c('commune', 'properties')
prop_commmune <- prop_commmune[!is.na(prop_commmune$commune),]
prop_commmune <- prop_commmune[order(prop_commmune$properties),]

# Plot 7 - Number of properties per commune
ggplot(data=prop_commmune, aes(x=properties, y=reorder(commune, properties))) + geom_bar(stat="identity", width=0.8) + 
  ggtitle("Properties published per Commune")+geom_text(aes(label=properties), hjust=-0.1) + xlim(0, 22000) + ylab("Communes")+theme_minimal()

# Plot 8 
ggplot(prop, aes(x = ipm, y = -0.5)) + geom_boxploth(aes(fill = ipm)) + geom_density(aes(x = ipm), inherit.aes = FALSE) + 
  ylab("MPI") + ggtitle("Distribution of Multidimensional Poverty Index in Medellin, Colombia") + theme_minimal()

## Plot 8
prop_ipm <- prop %>% count(prop$ipm_class)
names(prop_ipm) <- c('ipm_class', 'properties')
prop_ipm <- prop_ipm[!is.na(prop_ipm$ipm_class),]
prop_ipm <- prop_ipm[order(prop_ipm$properties),]

ggplot(data=prop_ipm, aes(y=properties, x=ipm_class)) + geom_bar(stat="identity", width=0.8) + 
  ggtitle("Properties published per IPM Class")+geom_text(aes(label=properties), vjust=-0.3) + ylim(0, 43000) + xlab("IPM Class")+theme_minimal()


# Plot 9
sf_map <- ggmap(get_map(c(-75.6827,6.1071,-75.4718,6.3763), source = "stamen"))
sf_map + ggtitle("Medellin, Colombia")
x1 <- prop[prop$operation_type == "Sell", c("long", "lat", "price")]
names(x1) <- c("long", "lat", "selling_price")
s1 <- sf_map +  geom_point(aes(x = long, y = lat, color=selling_price), data  = x1) + ggtitle("Location of the properties by price") + theme_void()
s1



# Plot 10!!!!!
install.packages('geosphere')
library(geosphere)
nc_shp <- st_read('Limite_Catastral_de_Comunas_y_Corregimientos/L%C3%ADmite_Catastral_de__Comunas_y_Corregimientos.shp')
sf_map <- ggmap(get_map(c(-75.74,6.1471,-75.4718,6.3763), source = "stamen", maptype = 'terrain-background'))

sf_map + p

p <- ggplot(data=nc_shp) + geom_sf() + geom_sf_label(aes(label=NOMBRE), size = 2.2) 

  geom_sf(data = nc_shp, inherit.aes = FALSE) + theme_void()

centroid(nc_shp$SHAPEAREA)


#Plot 11

x2 <- prop[prop$operation_type == "Rent", c("long", "lat", "price")]
names(x2) <- c("long", "lat", "renting_price")
s2 <- sf_map +  geom_point(aes(x = long, y = lat, color=renting_price), data  = x2 ) + theme_void()
s2

grid.arrange(s1, s2, ncol=2)

# Plot 9
sf_map <- ggmap(get_map(c(-75.6827,6.1071,-75.4718,6.3763), source = "stamen"))
sf_map + ggtitle("Medellin, Colombia")
i1 <- prop[(prop$ipm > 0 & prop$ipm < 40), c("long", "lat", "ipm")]
names(i1) <- c("long", "lat", "ipm")
r1 <- sf_map +  geom_point(aes(x = long, y = lat, color=ipm), data  = i1) + ggtitle("Location of the properties by IPM") + theme_void()
r1


# Plot 10
# Number of properties per year
ipm_class_ <- prop[prop$ipm_class != 'Not Applicable',] %>% group_by(ipm_class) 
names(ipm_class_) <- c('ipm_class', 'properties')

ggplot(data=ipm_class_, aes(x=properties, y=reorder(ipm_class, properties))) + geom_bar(stat="identity", width=0.8) + 
  ggtitle("Properties published per IPM Class")+geom_text(aes(label=properties), hjust=-0.1) + xlim(0, 45000) + ylab("IPM Class")



## Communes Shape File
com_info <- prop %>% group_by(commune_name, operation_type, property_type) %>% 
  summarise(mean_price=mean(price), mean_ipm=mean(ipm), bed=mean(bedrooms, na.rm = T), bath=mean(bathrooms, na.rm=T), prop=n())
com_shp <- merge(communes, com_info, by.x="NOMBRE", by.y="commune_name")

# Plot 11
sell1 <- com_shp[com_shp$operation_type == "Sell",] %>% group_by(NOMBRE) %>% summarise(mean_price=round(mean(mean_price), digits=2))
ggplot(data=sell1) + 
  geom_sf(aes(fill=mean_price)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  geom_sf_label(aes(label=NOMBRE), size = 2.3) + ggtitle("Distribution of Mean Price of Sell by Commune") + theme_void()

rent1 <- com_shp[com_shp$operation_type == "Rent",] %>% group_by(NOMBRE) %>% summarise(mean_price=round(mean(mean_price), digits=2))
ggplot(data=rent1) + 
  geom_sf(aes(fill=mean_price)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  geom_sf_label(aes(label=NOMBRE), size = 2.3) + ggtitle("Distribution of Mean Price of Rent by Commune") + theme_void()

## Plot 12
unique(com_shp[com_shp$operation_type=="Sell" & com_shp$property_type=="Office",]$mean_price)
ggplot(data=com_shp[com_shp$operation_type=="Sell",]) + geom_sf(aes(fill=mean_price)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="black") + 
  ggtitle("Distribution of Mean Price of Sell by Commune and Type of Property") + facet_wrap(~property_type) + theme_void()

ggplot(data=com_shp[com_shp$operation_type=="Rent",]) + geom_sf(aes(fill=mean_price)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  ggtitle("Distribution of Mean Price of Rent by Commune and Type of Property") + facet_wrap(~property_type) + theme_void()


# Plot 13
ggplot(data=com_shp) + 
  geom_sf(aes(fill=prop)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  geom_sf_label(aes(label=NOMBRE), size = 2.2) + ggtitle("Property Availability by Commune and Operation Type") + facet_wrap(~operation_type)

ggplot(data=com_shp) + 
  geom_sf(aes(fill=prop)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  ggtitle("Property Availability by Commune and Property Type") + facet_wrap(~property_type)
                                                      
# Plot 13
ipm1 <- com_shp %>% group_by(NOMBRE) %>% summarise(mean_ipm=round(mean(mean_ipm), digits=2))
ggplot(data=ipm1) + geom_sf(aes(fill=mean_ipm)) + scale_fill_continuous(low ="thistle2", high="darkred", guide="colorbar", na.value="white") + 
  geom_sf_label(aes(label=NOMBRE), size = 2.2) + ggtitle("IPM by Commune") + theme_void()


a <- prop[,c("commune_name","property_type","operation_type","price", "ipm", "ipm_class")]
a1 <- a[a$operation_type == "Sell",]
a2 <- a[a$operation_type == "Rent",]
ggplot(data=a[(a$ipm < 100 & a$ipm > 0 & a$price > 10 ),], aes(y=ipm, x=price)) + geom_jitter() + geom_smooth(method="lm") 

# Hexbin chart with default option
c1 <- ggplot(a1, aes(x=ipm, y=price)) +
  geom_jitter() + geom_smooth(method="lm") + ylab("price of Sell") +
  theme_minimal() + ggtitle("Scatter Plot of Price of Sell vs. IPM")

c2 <- ggplot(a2, aes(x=ipm, y=price)) +
  geom_jitter() + geom_smooth(method="lm") + ylab("price of Rent") +
theme_minimal() + ggtitle("Scatter Plot of Price of Rent vs. IPM")

grid.arrange(c1, c2, ncol=2)

# Ridges


# Hexbin chart with default option
b1 <- ggplot(a1, aes(x=ipm, y=price)) +
  geom_bin2d()  + ylab("price of Sell") +
  theme_minimal() + ggtitle("Density 2D plot of Sell Price and IPM")

b2 <- ggplot(a2, aes(x=ipm, y=price)) +
  geom_bin2d()  +  ylab("price of Rent") +
  theme_minimal() + ggtitle("Density 2D plot of Rent Price and IPM")

grid.arrange(b1, b2)

# Ridges

prop[prop$operation_type == "Rent",] %>%
  mutate(class = ipm_class) %>%
  ggplot( aes(y=class, x=price,  fill=class)) +
  geom_density_ridges(alpha=0.6, bandwidth=8) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Price of Rent") +
  ylab("Density") + ggtitle("Ridge Plot of Price of Rent by IPM Class ")



prop[prop$operation_type == "Sell",] %>%
  mutate(class = ipm_class) %>%
  ggplot( aes(y=class, x=price,  fill=class)) +
  geom_density_ridges(alpha=0.6, bandwidth=8) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Price of Sell") +
  ylab("Density") + ggtitle("Ridge Plot of Price of Sell by IPM Class ")



## Blocks
block_info <- prop %>% group_by(commune_name, operation_type, property_type) %>% 
  summarise(mean_price=mean(price), mean_ipm=mean(ipm), bed=mean(bedrooms, na.rm = T), bath=mean(bathrooms, na.rm=T), prop=n())
com_shp <- merge(communes, com_info, by.x="NOMBRE", by.y="commune_name")


prop <- prop %>% st_join(poverty, join = st_within , left = TRUE, largest = TRUE) # Prop_med, communes and poverty



