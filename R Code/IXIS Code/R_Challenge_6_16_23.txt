#IXIS Data Science Challenge
#Name: Foster Borch
#Date: 6/15/2023

#My aim with this challenge and with coding in general is to create something replicable. The last thing I want is for my code to only run on one system.
#Preventing errors, both user and developer, is one of my biggest aims when writing. I hope that this is evident in the following work.

#Setting a working directory to access files I have stored in my folder. The user shall choose.
setwd(
  choose.dir()
)

#Oh tidyverse, where would I be without you?
library(
  tidyverse
)

#Making a nice list of CSVs in the folder. This would come in handy for larger quantities of files so I can load them all in at once.  
csv_list<-
  list.files(getwd(),
             pattern="*.csv"
  )

#A fun loop that will iterate depending on the number of CSVs in the working directory. Creates data frames filled with CSV data with CSV file names.
for(
  i in 1:length(csv_list)
  )
  {
  assign(
    csv_list[i],
    read.csv(
      file.path(getwd(),
                '/',
                csv_list[i]
      )
    )
  )
  }

#Renaming our data frames because I like things to be organized.
Session_Counts<-
  data.frame(
    DataAnalyst_Ecom_data_sessionCounts.csv
  )

Adds_To_Cart<-
  data.frame(
    DataAnalyst_Ecom_data_addsToCart.csv
  )

#Continuing the organizational theme, it's time to get these cumbersome data frames out of the environment.
rm(
  DataAnalyst_Ecom_data_addsToCart.csv,
  DataAnalyst_Ecom_data_sessionCounts.csv
)

#Taking a closer look at the data
Session_Counts%>%
  group_by(
    dim_browser
  )%>%
  summarize(
    n()
  )%>%
  print(
    n=Inf
  )

#Having noticed there are quite a few "(not set)" and "error" browser types, I want to see what the metrics look like for these specifically.
#Running this code will show that there were 0 transactions or QTY associated with these browsers. After doing some research on (not set) browsers,
#I'm leaning towards possible bot activity. I will filter it out of the data for now. 
Session_Counts%>%
  filter(
    dim_browser=="(not set)" | dim_browser=="error"
  )%>%
  group_by(
    dim_browser
  )%>%
  select(
    dim_browser,
    sessions,
    transactions,
    QTY
  )%>%
  summarize(
    Sessions=sum(sessions),
    Transactions=sum(transactions),
    QTY=sum(QTY)
  )

#Removing rows with "(not set)" and "error" browsers.
Session_Counts<-
  Session_Counts%>%
  filter(
    dim_browser!="(not set)" & dim_browser != "error"
  )

#Reusing code to check that they are gone. 
Session_Counts%>%
  filter(
    dim_browser=="(not set)" | dim_browser=="error"
  )%>%
  group_by(
    dim_browser
  )%>%
  summarize(
    n()
  )%>%
  print(
    n=Inf
  )

#Format date column as a date.
Session_Counts$dim_date<-
  as.Date(
    Session_Counts$dim_date,
    format="%m/%d/%y"
  )

#Creating a month column to make aggregation easier on myself.
Session_Counts$Month<-
  format(
    as.Date(
      Session_Counts$dim_date,
      format="%y-%m-%d"
    ),
    "%m"
  )

#Creating ECR column based on the defined formula (Transactions/Sessions). ECR represents a relationship and cannot be traditionally summed. Each
#addition of sessions and transactions affects the ECR. This column addition is purely for informational purposes. 
Session_Counts$ECR<-
  Session_Counts$transactions/Session_Counts$sessions

#In two cases, an "Int" was divided by a zero, resulting in "inf" values. Replacing those with "NA".
Session_Counts[
  sapply(
    Session_Counts,
    is.infinite
  )
]<-
  NA

#Replacing "NA" values with 0. I don't want to drop this data as it is important to the bigger picture. 
Session_Counts[
  sapply(
    Session_Counts,
    is.na
  )
]<-
  0

#Creating a data frame with specific columns. Makes my life easier when aggregating.
Session_Counts_Selection<-
  Session_Counts%>%
  select(
    4:6
  )

#Aggregating the recently created data frame. It provides the user with a nice view of the breakdown of desktop, mobile, and tablet use over each month
#of the year. It includes averages of session counts, transactions, QTY, and ECR.
aggregated<-
  aggregate(
    Session_Counts_Selection,
    by=list(
      Session_Counts$dim_deviceCategory,
      Session_Counts$Month
    ),
    sum
  )

aggregated<-
  aggregated%>%
  rename(
    "Device"="Group.1",
    "Month"="Group.2"
  )

#Creating ECR column for the aggregated data
aggregated$ECR<-
  aggregated$transactions/aggregated$sessions

#Viewing our nice data frame
aggregated

#Sheet 2 - Month over Month Comparison

#Creating a filtered data frame that only contains data from May and June of 2013 (most recent months in the data)
Session_Counts_ByRecentMonths<-
  Session_Counts%>%
  filter(
    dim_date>="2013-05-01"
  )

#Renaming the month column for easier comparison between the two data frames.
Adds_To_Cart<-
  Adds_To_Cart%>%
  rename(
    "Month"="dim_month"
  )

#Changing month column to numeric. Removes leading zero and allows for comparison between two data frames.
Session_Counts_ByRecentMonths$Month<-
  as.numeric(
    Session_Counts_ByRecentMonths$Month
  )

#Now that conditions are favorable, the data frames are merged by month. 
Recent_Month_Metrics<-
  merge(
    Adds_To_Cart,
    Session_Counts_ByRecentMonths,
    "Month"
  )

#Performing a range of simple analyses on the data. Means and sums seemed like the most interesting ones to me. 
Recent_Month_Metrics_Diff<-
  Recent_Month_Metrics%>%
  group_by(
    Month
  )%>%
  select(
    Month,
    sessions,
    transactions,
    QTY,
    ECR,
    addsToCart
  )%>%
  summarize(
    AvgSessions=mean(sessions),
    TotalSessions=sum(sessions),
    AvgTransactions=mean(transactions),
    TotalTransactions=sum(transactions),
    AvgQTY=mean(QTY),
    TotalQTY=sum(QTY),
    AvgECR=mean(ECR),
    MonthlongECR=sum(transactions)/sum(sessions),
    TotalAdds=mean(addsToCart)
  )

#I want to mutate data and find absolute and relative differences between the two months. I could do this manually for each column.
#Instead, I will define functions that I can use in conjunction with across(). This will help mutate at scale. 

#Function takes an input, and then subtracts the "lag"/previous case entry from it. First result will be "NA".Thiss returns total/absolute difference.
fun_abs<-
  function(data)
{
  result<-
    (
      data-lag(data)
    )
}

#Function takes the absolute difference and divides by the "lag"/previous case entry from it. First result will be "NA". This gives relative/% difference.
fun_rel<-
  function(data)
{
  result<-
    (
      (data-lag(data))/lag(data)
    )
}

#Running the mutate using across(). Calling both functions with pre-set naming schemes. 
Recent_Month_Metrics_Diff<-
  Recent_Month_Metrics_Diff%>%
  mutate(
    across(
      !Month,
      funs(
        abs_diff=fun_abs(.),
        rel_diff=fun_rel(.)
      )
    )
  )

#Replacing "NA" values with zero.
Recent_Month_Metrics_Diff[
  sapply(
    Recent_Month_Metrics_Diff,
    is.na
  )
]<-
  0

#Looking at the data, I am instantly curious why avg ECR decreased but month long ECR increased.
#The relative differences tell a story that the total transactions increased by a greater percentage than the total sessions between months 5 & 6.
#So then why is avg ECR lower? It has to do with the way I calculated it. I took the averages of the ECR values, which disregards the number of 
#transactions and sessions. Possibly a weighted ECR value could be interesting to view.
Recent_Month_Metrics_Diff

#Now it's time to export these data frames to an xlsx with multiple sheets. This will save to the wd().
library(
  openxlsx
)

#Using openxlsx, I was able to easily create an xlsx with two sheets, each filled with a unique dataframe.

write.xlsx(
  aggregated,
  file="aggregated_metrics.xlsx",
  sheetName="sheet1",
  rownames=FALSE
)

wb<-
  loadWorkbook(
    "aggregated_metrics.xlsx"
  )

addWorksheet(
  wb,
  "sheet2"
)

writeData(
  wb,
  "sheet2",
  Recent_Month_Metrics_Diff
)

saveWorkbook(
  wb,
  "aggregated_metrics.xlsx",
  overwrite = TRUE
)
