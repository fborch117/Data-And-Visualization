#RISI Pre-Interview Assessment 
#Name: Foster Borch Date: 8/25/2021

##First setting my working directory just in case I have to go access other files saved here in the future.
##Additionally installing and loading packages.

setwd('N:/R/Work/RISI/risi_analyst_exercise')

# install.packages('snakecase')

library(snakecase)
library(stringi)
library(tools)

#Downloading "Rural Atlas" Data:
## Set Values For url and file destination. User should update file_destination to place of preference.If writing for a general group,
##I might use the Windows desktop in the future as default as to reduce possibility of conflicts if the code was for general use.

down_url<-'https://www.ers.usda.gov/webdocs/DataFiles/82701/Rural_Atlas_Update23.zip?v=1869'
down_path<-file.path(getwd(),"/data.zip")
download.file(down_url,down_path, mode="wb")

#Load Sheets Into R
##I will also unzip the file into a new folder.

zip_file<-'N:/R/Work/RISI/risi_analyst_exercise/data.zip'
unzip_dir<-"N:/R/Work/RISI/risi_analyst_exercise"
unzip(zip_file,exdir=unzip_dir)

file.remove("N:/R/Work/RISI/risi_analyst_exercise/Release23_June2021/RuralAtlasData23.xlsx") ##Instructional xlsx that I didn't want interfering. 

#Removing White Space from file names so I can use them in a loop and as dataframe names.

folder_dir<-"N:/R/Work/RISI/risi_analyst_exercise/Release23_June2021"

files_space<-list.files(folder_dir,pattern=' ',full.names=TRUE)

files_no_space<-stri_replace_all_fixed(files_space,pattern=' ','')

# or gsub.Arguments seem more intuitive for stringi. files_no_space<-gsub('\\s','',files_space)

#Pasting new file names into folder without spaces and removing old files

file.copy(from=files_space, to=files_no_space)

file.remove(files_space)

#Creating a list of all csv files within the unzipped folder

csv_list<-list.files(folder_dir,pattern="*.csv")
csv_list_short<-sub('\\.csv$','',csv_list)
dir_list<-list.files(folder_dir,full.names=TRUE)

#Creating a loop to load each csv into R instead of doing the same process six times.

for(i in 1:length(csv_list)){
  assign(csv_list[i], read.csv(file.path(folder_dir,'/',csv_list[i])))
}

##Creating individual column names
##Creating a new dataframe so that I can easily amend with cleaned data using rbind.

csv_colnames<-scan(text=colnames(CountyClassifications.csv),what='',sep='.')
# CountyClassifications.csv[csv_colnames] <- NA
CountyClassifications<-data.frame()

##Extracting data from each row and entering it into each column. Creating a base case so that the column names match.

csv_rowdata<-CountyClassifications.csv[1:1,1]
csv_rownames<-scan(text=csv_rowdata,what='',sep='\t')
csv_row<-as.data.frame(do.call(cbind,as.list(csv_rownames)))
colnames(csv_row)<-colnames(CountyClassifications)

## For loop that iterates through each of the 3225 rows. It selects the text from each row, separates it into
## individual parts, turns it into a data frame, and then binds it to an originally empty data frame row by row. 

for(i in 1:3225){
  csv_rowdata<-CountyClassifications.csv[i:i,1]
  csv_rownames<-scan(text=csv_rowdata,what='',sep='\t',quote='')
  csv_row<-as.data.frame(do.call(cbind,as.list(csv_rownames)))
  CountyClassifications<-rbind(CountyClassifications,csv_row)
  }

##Colnames got messy after rbind
colnames(CountyClassifications)<-csv_colnames

#Extracting fips codes to create separate state and county fips columns

fips_data<-CountyClassifications[1:3225,1]  
state_fips<-substr(fips_data,start=1,stop=2)
county_fips<-substr(fips_data,start=3,stop=5)
state_df<-as.data.frame(do.call(rbind,as.list(state_fips)))
county_df<-as.data.frame(do.call(rbind,as.list(county_fips)))
CountyClassifications<-cbind(CountyClassifications,state_df)
CountyClassifications<-cbind(CountyClassifications,county_df)
CountyClassifications$FIPStxt<-NULL

names(CountyClassifications)[45]<-'state_fips'
names(CountyClassifications)[46]<-'county_fips'

#Cleaning variables to snakecase

colnames(CountyClassifications)<-to_snake_case(colnames(CountyClassifications))
colnames(Income.csv)<-to_snake_case(colnames(Income.csv))
colnames(Jobs.csv)<-to_snake_case(colnames(Jobs.csv))
colnames(People.csv)<-to_snake_case(colnames(People.csv))
colnames(VariableNameLookup.csv)<-to_snake_case(colnames(VariableNameLookup.csv))
colnames(Veterans.csv)<-to_snake_case(colnames(Veterans.csv))

CountyClassifications.csv<-CountyClassifications

#Creating a new folder and new file names in snake case for cleaned data

clean_dir<-"N:/R/Work/RISI/risi_analyst_exercise/clean_data"
dir.create(clean_dir) #creating folder

files_snake<-to_snake_case(csv_list_short) #Creating file names in snake case to later write to csv
files_snake_dir<-paste0(folder_dir,'/',files_snake,'.csv') #creating directory variable for writing to csv

write.csv(CountyClassifications.csv,file=paste0(clean_dir,'/',files_snake[1],'.csv'),row.names=FALSE)
write.csv(Income.csv,file=paste0(clean_dir,'/',files_snake[2],'.csv'),row.names=FALSE)
write.csv(Jobs.csv,file=paste0(clean_dir,'/',files_snake[3],'.csv'),row.names=FALSE)
write.csv(People.csv,file=paste0(clean_dir,'/',files_snake[4],'.csv'),row.names=FALSE)
write.csv(VariableNameLookup.csv,file=paste0(clean_dir,'/',files_snake[5],'.csv'),row.names=FALSE)
write.csv(Veterans.csv,file=paste0(clean_dir,'/',files_snake[6],'.csv'),row.names=FALSE)


#-------------------------------------------------------------------------------------


#Downloading Cartographic Boundary Files and Unzipping

down_url2<-'https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip'
down_path2<-file.path(getwd(),"/spatial_data.zip")
download.file(down_url2,down_path2, mode="wb")

zip_file2<-'N:/R/Work/RISI/risi_analyst_exercise/spatial_data.zip'
unzip_dir2<-'N:/R/Work/RISI/risi_analyst_exercise/spatial_data'
unzip(zip_file2,exdir=unzip_dir2)

#Installing and loading packages for mapmaking. Rtools is installed with R compiler libraries.

# install.packages('ggplot2')
# install.packages('broom')
# install.packages('rgdal')
# install.packages('sf')
# install.packages('viridis')
# install.packages('tigris')
# install.packages('gridExtra')
# install.packages('biscale')
# install.packages('cowplot')
library(ggplot2)
library(broom)
library(rgdal)
library(tidyverse)
library(sf)
library(dplyr)
library(viridis)
library(tigris)
library(gridExtra)
library(biscale)
library(cowplot)


#Time to get the shapefile/s into R

setwd('N:/R/Work/RISI/risi_analyst_exercise/spatial_data')

r_shp<-st_read(paste0(getwd(),'/','cb_2018_us_county_500k.shp'))

#Subsetting data that will be used to dictate what is shown in the ggplot
#New England States VT, NH, ME, MA, RI, CT | 50, 33, 23, 25, 44, 09
#Nonmetro counties only

fips<-c('50', '33', '23', '25', '44', '09')
NE_fips<-filter(CountyClassifications, state_fips %in% fips & nonmetro_2013==1)
NE_fips$fips_comb<-paste(NE_fips$state_fips,NE_fips$county_fips,sep='')
NE_fips_comb<-NE_fips$fips_comb

##learning a bit about the shapefile

 r_shp
 st_geometry_type(r_shp)
 st_crs(r_shp)
 st_bbox(r_shp)

#Plotting the shapefile filtered by rural and ne counties
 
r_shp$fips<-paste(r_shp$STATEFP,r_shp$COUNTYFP,sep='')
NE_fips_comb
r_shp<-filter(r_shp, fips %in% NE_fips_comb)
# plot(r_shp)

#Subsetting data from rural atlas to create more interesting maps

for (i in 1:3278){
  if (nchar(Income.csv[i:i,1])<5) {
    Income.csv[i:i,1]<-paste0('0',Income.csv[i:i,1]) #Adding leading zeros to those that are missing it
  }
}

names(Income.csv)[1]<-'fips' #Gathering the entire fips column
income_sub<-filter(Income.csv, fips %in% NE_fips_comb) #gathering only the rows within the New England rural fips parameters

for (i in 1:3278){
  if (nchar(Jobs.csv[i:i,1])<5) {
    Jobs.csv[i:i,1]<-paste0('0',Jobs.csv[i:i,1]) #Adding leading zeros to those that are missing it
  }
}

names(Jobs.csv)[1]<-'fips' #Gathering the entire fips column
jobs_sub<-filter(Jobs.csv, fips %in% NE_fips_comb) #gathering only the rows within the New England rural fips parameters

r_shp<-merge(r_shp,jobs_sub, by.x='fips', by.y='fips')
r_shp<-merge(r_shp,income_sub, by.x='fips', by.y='fips')

# r_shp<-cbind(r_shp,jobs_sub) #Binding new information to the shapefile
# r_shp<-cbind(r_shp,income_sub)

# theme_set(
#   theme_minimal() +
#     theme(legend.position = "right")
# )

# r_shp<-readOGR(
#   dsn=file.path('N:/R/Work/RISI/Downloads/spatial_data/cb_2018_us_county_500k.shp'),
#   stringsAsFactors = F
# )

#Two maps showing different variables as the fill option.
#Looking at the differences between rate of unemployment and poverty rate. 


plot1<-ggplot() + geom_sf(data=r_shp,color='white', mapping=aes(fill=unemp_rate_2020))+
  scale_fill_viridis(discrete=FALSE,option='E', name='Unemployment Rate')+
  labs(title='Unemployment Rate in Rural New England Counties in 2020')
  
plot2<-ggplot() + geom_sf(data=r_shp,color='white', mapping=aes(fill=poverty_rate_acs))+
  scale_fill_viridis(discrete=FALSE,option='E', name='Poverty Rate')+
  labs(title='Poverty Rate in Rural New England Counties')

grid.arrange(plot1,plot2,ncol=2) #arranging in a grid to compare

#Creating bivariate choropleth map

bi_data<-bi_class(r_shp, x=unemp_rate_2020, y=poverty_rate_acs, style='quantile', dim=3) #two variable definition

plot3<-ggplot()+geom_sf(data=bi_data, mapping=aes(fill=bi_class), color='black', size=0.1, show.legend=FALSE) +
  bi_scale_fill(pal='DkCyan',dim=3)+
  labs(title = 'Unemployment and Poverty in Rural New England', subtitle = 'Dark Cyan Palette')+bi_theme() #Don't want default legend. Creating map.

legend<-bi_legend(pal='DkCyan',
                  dim=3,
                  xlab='Higher % Unemployment',
                  ylab='Higher Rate of Poverty',
                  size=6) #Creating bivariate legend

plot3_final<-ggdraw()+
  draw_plot(plot3,0,0,1,1)+
  draw_plot(legend,0.1,.65,0.2,0.2) #Combining legend and map

plot3_final #final map! 

#Saving map as pdf to repository

setwd('N:/R/Work/RISI/risi_analyst_exercise')
pdf( {{'unemployment_poverty_rural_ne.pdf'}}, 
     width = par('din')[1],
     height = par('din')[2])
print(plot3_final)

dev.off()
