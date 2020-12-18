# install.packages("data.table")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("skimr")
# install.packages("knitr")
# install.packages("treemap")
# install.packages("installr")
# 
# library(installr)
# 
# updateR()

#Getting Libraries
library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)
library(skimr)
library(knitr)
library(treemap)

# import raw data file using read_excel function & trim white space
retail <- read_excel("Online Retail.xlsx", 
                     trim_ws = TRUE)

#Inspecting Data using skim
retail %>%  skim()
#skim(retail) ~ retail %>% skim() this is called pipe operator


#Get rows with invoice no starting with C and give total number of these rows
retail %>% 
  filter(grepl("C", retail$InvoiceNo)) %>% 
  summarise(Total = n())

#Now remove these rows and make that the new retail table
retail  <- retail %>% 
  filter(!grepl("C", retail$InvoiceNo))

retail %>% 
  filter(Quantity <= 0) %>%
  group_by(Description, UnitPrice) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) %>%
  ungroup()

#Keep only quantities greater than 0 
retail  <- retail %>% 
  filter(Quantity > 0)


#working with product codes
retail$StockCode
#blog writer checked for weird stock codes which were not numbers
stc <- c('AMAZONFEE', 'BANK CHARGES', 'C2', 'DCGSSBOY',     'DCGSSGIRL', 'DOT', 'gift_0001_', 'PADS', 'POST')

paste(stc, collapse="|")

retail %>%  
  filter(grepl(paste(stc, collapse="|"), StockCode))  %>% 
  group_by(StockCode, Description) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

#removing all rows with such stock codes
retail <- filter(retail, 
                 !grepl(paste(stc, collapse="|"), StockCode))

#Working with descriptions
retail$Description
retail$Description %>% table()

# Additional adjustment codes to remove
descr <- c( "check", "check?", "?", "??", "damaged", "found", 
            "adjustment", "Amazon", "AMAZON", "amazon adjust", 
            "Amazon Adjustment", "amazon sales", "Found", "FOUND",
            "found box", "Found by jackie ","Found in w/hse","dotcom", 
            "dotcom adjust", "allocate stock for dotcom orders ta", "FBA", "Dotcomgiftshop Gift Voucher £100.00", "on cargo order",
            "wrongly sold (22719) barcode", "wrongly marked 23343",
            "dotcomstock", "rcvd be air temp fix for dotcom sit", 
            "Manual", "John Lewis", "had been put aside", 
            "for online retail orders", "taig adjust", "amazon", 
            "incorrectly credited C550456 see 47", "returned", 
            "wrongly coded 20713", "came coded as 20713", 
            "add stock to allocate online orders", "Adjust bad debt", 
            "alan hodge cant mamage this section", "website fixed",
            "did  a credit  and did not tick ret", "michel oops",
            "incorrectly credited C550456 see 47", "mailout", "test",
            "Sale error",  "Lighthouse Trading zero invc incorr", "SAMPLES",
            "Marked as 23343", "wrongly coded 23343","Adjustment", 
            "rcvd be air temp fix for dotcom sit", "Had been put aside." )

#working with thw %in% operator
tbl
fav <- "Pig"
fav %in% tbl$animal
tbl$animal %in% fav
tbl %>% filter(fav %in% animal)

#removing rows which have description which we dont want
retail <- retail %>% 
  filter(!(Description %in% descr))

#working with is.na function

a <- c(1,2,3,4,5,NA,6)
a %>% is.na

sum(is.na(retail$Description))
retail <- retail %>% 
  filter(!is.na(Description))

#Working with apply functions
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 1, mean)
a_m1

cars %>% View()

#lapply and sapply
dt <- cars
lmn_cars <- lapply(dt, sum)
smn_cars <- sapply(dt, min)
lmn_cars
smn_cars

#subsetting data
retail[1,]
retail[1:5,]
retail[c(1,3,5),]

retail[1:5,1]
retail[1:5,c("InvoiceNo","Description")]
retail[1:5,c(1,3)]

#Working with "unique" and "length" function
a <- c("ant","ant","ant","pig","pig","genda")
a %>% length()
a %>% unique()
a %>% unique() %>% length()
length(unique(a))

sapply(retail[ ,c('InvoiceNo','CustomerID')], 
       function(x) length(unique(x)))

#Using ggplot
retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)

a <- c("Ant","Bat","Cow","Pig","Pig","Genda","Genda","Pig")
b <- c(1,1,1,1,2,1,1,3)
tbl <- cbind(a,b) %>% as.data.frame()
colnames(tbl) <- c("animal","price")
tbl %>% group_by(price) %>% summarise(count=n())
tbl %>% group_by(animal) %>% summarise(count=n())

#Using duplicated function on tbl

tbl <- tbl %>% 
  mutate(InNo_Desc = paste(animal, price, sep = ' ')) 

tbl <- tbl [tbl$InNo_Desc %>% duplicated() %>% `!`,]
tbl

# install.packages('recommenderlab')
# library(recommenderlab)



ratings_matrix <- tbl %>%
  select(animal,price) %>%
  mutate(rating = 1) %>%
  spread(animal, rating, fill = 0) %>%
  select(-price) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")

ratings_matrix
tbl

retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)


retail %>% 
  ggplot(aes(hour(hms(Time)))) + 
  geom_histogram(stat = "count",fill = "#E69F00", colour = "red") +
  labs(x = "Hour of Day", y = "") +
  theme_grey(base_size = 12)
