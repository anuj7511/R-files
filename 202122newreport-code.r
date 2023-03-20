library(knitr)
library(rmarkdown)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(officer)
library(openxlsx)
library(dplyr)
library(tidyr)
library(flextable)
library(writexl)
library(kableExtra)
require(gdtools)
library(officer)
library(magrittr)

imIndia202021 <- importdf
exIndia202021 <- read_excel("/Users/parth/Desktop/TradeReport_Development/Exports202122.xlsx")

# gsub replacement operations, replacing any /n etc
trim <- function (x) gsub("^\\s+|\\s+$", "", x)




formatnew <- function(x,pgwidth = 6){
  ft<- qflextable(x)
  ft <- theme_box(ft)
  ft <- bg(ft, bg = "#34ABA2", part = "header")
  ft <- set_table_properties(ft, width = .5, layout = "autofit")
  ft <- color(ft, color = "white", part = "header")
  ft <- font(ft, fontname = "Cambria")
  ft <- align(ft, align = "center", part = "header")
  
  return(ft)
}

formatnew1 <- function(x,pgwidth = 6){
  ft<- qflextable(x)
  ft <- theme_box(ft)
  ft <- bg(ft, bg = "#34ABA2", part = "header")
  ft <- set_table_properties(ft, width = .5, layout = "autofit")
  ft <- color(ft, color = "white", part = "header")
  ft <- font(ft, fontname = "Cambria")
  ft <- align(ft, align = "center", part = "header")
  ft <- footnote( ft, i = NULL, j = NULL,value = "USD Million",ref_symbols = NULL,part = "footer")
  return(ft)
}

nosector <- "[No significant sector]"
nocom <- "[No significant commodities]"
######################################################################################################################################################################
imIndia201920 <- read_excel("Imports202122.xlsx")
exIndia201920 <- read_excel("Exports202122.xlsx")



imIndia201920$Y202021[is.na(imIndia201920$Y202021)] <- 0
exIndia201920$Y202021[is.na(exIndia201920$Y202021)] <- 0
imIndia201920$Y202122[is.na(imIndia201920$Y202122)] <- 0
exIndia201920$Y202122[is.na(exIndia201920$Y202122)] <- 0


totalimports <- sum(imIndia201920$Y202122)
totalexports <- sum(exIndia201920$Y202122)

imIndia201920$country <- str_to_title(imIndia201920$country)
exIndia201920$country <- str_to_title(exIndia201920$country)
imcountry$Country <- str_to_title(imcountry$Country)
excountry$Country <- str_to_title(excountry$Country)

imIndia202021 <- imIndia201920
exIndia202021 <- exIndia201920


country1 <- "Jordan"
countriesList <- unique(excountry$Country)

countriesListdf <- data.frame(matrix(unlist(countriesList), nrow=length(countriesList), byrow=TRUE))
write_xlsx(countriesListdf, "countriesListdf.xlsx")

hsc2d

colnames(imIndia202021) <- c( "hsc2d","country", "code",    "Y202021", "Y202122" ,"hsc4d"  , "hsc6d" ,  "Section" ,"Sector" )
colnames(exIndia202021) <- c( "hsc2d","country", "code",    "Y202021", "Y202122" ,"hsc4d"  , "hsc6d" ,  "Section" ,"Sector" )

print(countriesList)

write.csv(data,"C:\\Users\\...YOUR PATH...\\population.csv")

rm(a4)

datafr <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

datafr <- within(datafr, rm(imIndia202021))

datafr %>% purrr::list_modify("a" = NULL)

datafr[imIndia202021] <- NULL
datafr <- datafr[names(datafr) != "exIndia202021"] 
print(datafr)
print(countriesList)
######################################################################################################################################################################
country1 <- "Russia"


i <- 156
i <- i+1
print(countriesList)
## Loop 
while (i < 240) {
  print("in loop")
  country1 <- countriesList[i]
  
  rm(ces)
  rm(importmsspos)
  rm(importmssneg)
  rm(importms8pos)
  rm(importms8neg)
  rm(exportmsspos)
  rm(exportmssneg)
  rm(exportms8pos)
  rm(exportms8neg)
  rm(imIndia202021cds)
  rm(imIndia202021cd2)
  rm(imIndia202021cd8)
  rm(exIndia202021cds)
  rm(exIndia202021cd2)
  rm(exIndia202021cd8)
  rm(cs)
  rm(c2)
  rm(c8)
  rm(es)
  rm(e2)
  rm(e8)
  rm(imtsannexure)
  rm(imtcannexure)
  rm(extsannexure)
  rm(extcannexure)
  rm(immsannexure)
  rm(exmsannexure)
  rm(imdsannexure)
  rm(imdcannexure)
  rm(exdsannexure)
  rm(exdcannexure)
  
  imIndia202021filter<-filter(imIndia202021,imIndia202021$country == country1)
  imIndia202021fe<-filter(exIndia202021,exIndia202021$country == country1)
  imc<-filter(imcountry,imcountry$Country == str_to_title(country1))
  exc<-filter(excountry,excountry$Country == str_to_title(country1))
  
  colnames(exc)
  imcountrysum <- imc$`2021-2022`[1]
  excountrysum <- exc$`2021-2022`[1]
  imcountryg <- as.numeric(imc$`%Growth`[1])
  excountryg <- as.numeric(exc$`%Growth`[1])

  imIndia202021filter$Y202122[is.na(imIndia202021filter$Y202122)] <- 0
  imIndia202021filter$Y202021[is.na(imIndia202021filter$Y202021)] <- 0
  imIndia202021fe$Y202122[is.na(imIndia202021fe$Y202122)] <- 0
  imIndia202021fe$Y202021[is.na(imIndia202021fe$Y202021)] <- 0
  try1 <- imIndia202021filter[imIndia202021filter[,'Y202122']>0,]
  imlen8 <- nrow(try1)
  try1 <- imIndia202021fe[imIndia202021fe[,'Y202122']>0,]
  exlen8 <- nrow(try1)
  
  rm()
  
  ## TRADE SNAPSHOT
  
  #Imports
  
  keeps <- c("Sector", "Y202122")
  imIndia202021fk <- imIndia202021filter[keeps]
  imIndia202021fk <- aggregate(Y202122 ~ Sector, imIndia202021fk, sum)
  imtsannexure <- imIndia202021fk
  imtsannexure <- imtsannexure[imtsannexure[,'Y202122']>0,]
  imlens <- nrow(imtsannexure)
  imtsannexure <- imtsannexure[with(imtsannexure, order(-Y202122)),]
  imtsannexure <- tibble::rowid_to_column(imtsannexure, "ID")
  colnames(imtsannexure) <- c("S.No.","Sector","Imports (2021-22)")
  cs <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(5)
  #cs <- merge(x= cs, y = codes2, by.x= 'hsc2d',by.y = 'HSCode', all.x = TRUE)
  colnames(cs) <- c("Sector","Imports (2021-22)")
  cs<-cs[order(-cs$`Imports (2021-22)`),]
  row.names(cs) <- NULL
  cs <- cbind(Rank = rownames(cs), cs)
  rownames(cs) <- 1:nrow(cs)
  cs$Sector <- str_to_title(cs$Sector)
  
  keeps <- c("code", "Y202122")
  imIndia202021fk <- imIndia202021filter[keeps]
  c8 <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(10)
  c8$code <- trim(c8$code)
  c8 <- merge(x = c8, y = codes8, by= 'code', all.x = TRUE)
  colnames(c8) <- c("HSCode","Imports (2021-22)","Commodity")
  c8<-c8[order(-c8$`Imports (2021-22)`),]
  row.names(c8) <- NULL
  c8 <- cbind(Rank = rownames(c8), c8)
  rownames(c8) <- 1:nrow(c8)
  
  keeps <- c("hsc2d", "country", "Y202122")
  imIndia202021fk <- imIndia202021filter[keeps]
  imIndia202021fk <- aggregate(Y202122 ~ hsc2d, imIndia202021fk, sum)
  imtcannexure <- imIndia202021fk
  imtcannexure <- merge(x= imtcannexure, y = codes2, by.x= 'hsc2d',by.y = 'HSCode', all.x = TRUE)
  imtcannexure <- imtcannexure[imtcannexure[,'Y202122']>0,]
  imlenc <- nrow(imtcannexure)
  imtcannexure <- imtcannexure[with(imtcannexure, order(-Y202122)),]
  imtcannexure <- tibble::rowid_to_column(imtcannexure, "ID")
  colnames(imtcannexure) <- c("S.No.","Chapter Code","Imports (2021-22)","Chapter Description")
  c2 <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(5)
  c2$hsc2d <- trim(c2$hsc2d)
  c2 <- merge(x= c2, y = codes2, by.x= 'hsc2d',by.y = 'HSCode', all.x = TRUE)
  colnames(c2) <- c("Chapter Code","Imports (2021-22)","Chapter Description")
  c2<-c2[order(-c2$`Imports (2021-22)`),]
  row.names(c2) <- NULL
  c2 <- cbind(Rank = rownames(c2), c2)
  rownames(c2) <- 1:nrow(c2)
  
  drops <- c("Imports (2021-22)")
  ces <- cs[ , !(names(cs) %in% drops)]
  colnames(ces) <- c("Rank","India's Top Importing Sectors")
  
  #Exports
  
  keeps <- c("Sector", "Y202122")
  imIndia202021fk <- imIndia202021fe[keeps]
  imIndia202021fk <- aggregate(Y202122 ~ Sector, imIndia202021fk, sum)
  extsannexure <- imIndia202021fk
  extsannexure <- extsannexure[extsannexure[,'Y202122']>0,]
  exlens <- nrow(extsannexure)
  extsannexure <- extsannexure[with(extsannexure, order(-Y202122)),]
  extsannexure <- tibble::rowid_to_column(extsannexure, "ID")
  colnames(extsannexure) <- c("S.No.","Sector","Exports (2021-22)")
  
  es <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(5)
  colnames(es) <- c("Sector","Exports (2021-22)")
  es<-es[order(-es$`Exports (2021-22)`),]
  row.names(es) <- NULL
  es <- cbind(Rank = rownames(es), es)
  rownames(es) <- 1:nrow(es)
  es$Sector <- str_to_title(es$Sector)
  
  keeps <- c("code", "Y202122")
  imIndia202021fk <- imIndia202021fe[keeps]
  e8 <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(10)
  e8$code <- trim(e8$code)
  e8 <- merge(x = e8, y = codes8, by= 'code', all.x = TRUE)
  colnames(e8) <- c("HSCode","Exports (2021-22)","Commodity")
  e8<-e8[order(-e8$`Exports (2021-22)`),]
  row.names(e8) <- NULL
  e8 <- cbind(Rank = rownames(e8), e8)
  rownames(e8) <- 1:nrow(e8)
  
  keeps <- c("hsc2d", "country", "Y202122")
  imIndia202021fk <- imIndia202021fe[keeps]
  imIndia202021fk <- aggregate(Y202122 ~ hsc2d, imIndia202021fk, sum)
  extcannexure <- imIndia202021fk
  extcannexure <- merge(x= extcannexure, y = codes2, by.x= 'hsc2d',by.y = 'HSCode', all.x = TRUE)
  extcannexure <- extcannexure[extcannexure[,'Y202122']>0,]
  exlenc <- nrow(extcannexure)
  extcannexure <- extcannexure[with(extcannexure, order(-Y202122)),]
  extcannexure <- tibble::rowid_to_column(extcannexure, "ID")
  colnames(extcannexure) <- c("S.No.","Chapter Code","Exports (2021-22)","Chapter Description")
  
  e2 <- imIndia202021fk[with(imIndia202021fk, order(-Y202122)),] %>% top_n(5)
  e2$hsc2d <- trim(e2$hsc2d)
  e2 <- merge(x= e2, y = codes2, by.x= 'hsc2d',by.y = 'HSCode', all.x = TRUE)
  colnames(e2) <- c("Chapter Code","Exports (2021-22)","Chapter Description")
  e2<-e2[order(-e2$`Exports (2021-22)`),]
  row.names(e2) <- NULL
  e2 <- cbind(Rank = rownames(e2), e2)
  rownames(e2) <- 1:nrow(e2)
  
  drops <- c("Exports (2021-22)")
  ees <- es[ , !(names(es) %in% drops)]
  colnames(ees) <- c("Rank","India's Top Exporting Sectors")
  
  ces <- merge(x= ces, y = ees, by = 'Rank', all.x = TRUE)
  
  
  #dependence products
  
  imcountryd <- (imcountrysum/totalimports)*100
  excountryd <- (excountrysum/totalexports)*100
  
  ### Imports
  keeps <- c("code", "Y202122")
  imIndia202021f <- imIndia202021[keeps]
  imIndia202021f8 <- aggregate(Y202122 ~ code, imIndia202021f, sum)
  colnames(imIndia202021f8) <- c("code", "Y202122sum8")
  keeps <- c("hsc2d", "Y202122")
  imIndia202021f <- imIndia202021[keeps]
  imIndia202021f2 <- aggregate(Y202122 ~ hsc2d, imIndia202021f, sum)
  colnames(imIndia202021f2) <- c("hsc2d", "Y202122sum2")
  keeps <- c("Sector", "Y202122")
  imIndia202021f <- imIndia202021[keeps]
  imIndia202021fs <- aggregate(Y202122 ~ Sector, imIndia202021f, sum)
  colnames(imIndia202021fs) <- c("Sector", "Y202122sums")
  
  
  
  keeps <- c("code", "Y202122")
  imIndia202021f <- imIndia202021filter[keeps]
  imIndia202021fc8 <- aggregate(Y202122 ~ code, imIndia202021f, sum)
  colnames(imIndia202021fc8) <- c("code", "Y202122")
  keeps <- c("hsc2d", "Y202122")
  imIndia202021f <- imIndia202021filter[keeps]
  imIndia202021fc2 <- aggregate(Y202122 ~ hsc2d, imIndia202021f, sum)
  colnames(imIndia202021fc2) <- c("hsc2d", "Y202122")
  keeps <- c("Sector", "Y202122")
  imIndia202021f <- imIndia202021filter[keeps]
  imIndia202021fcs <- aggregate(Y202122 ~ Sector, imIndia202021f, sum)
  colnames(imIndia202021fcs) <- c("Sector", "Y202122")
  
  
  
  imIndia202021fc8 <- merge(x= imIndia202021fc8, y = imIndia202021f8, by = 'code', all.x = TRUE)
  imIndia202021fc2 <- merge(x= imIndia202021fc2, y = imIndia202021f2, by = 'hsc2d', all.x = TRUE)
  imIndia202021fcs <- merge(x= imIndia202021fcs, y = imIndia202021fs, by = 'Sector', all.x = TRUE)
  
  imIndia202021fc8$per <- (imIndia202021fc8$Y202122/imIndia202021fc8$Y202122sum8)*100
  imIndia202021fc2$per <- (imIndia202021fc2$Y202122/imIndia202021fc2$Y202122sum2)*100
  imIndia202021fcs$per <- (imIndia202021fcs$Y202122/imIndia202021fcs$Y202122sums)*100
  imIndia202021fc8 <-imIndia202021fc8 %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fc2 <-imIndia202021fc2 %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fcs <-imIndia202021fcs %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fc2 <- merge(x = imIndia202021fc2, y = codes2, by.x= 'hsc2d',by.y= 'HSCode', all.x = TRUE)
  imIndia202021fc8$code <- trim(imIndia202021fc8$code)
  imIndia202021fc8 <- merge(x = imIndia202021fc8, y = codes8, by= 'code', all.x = TRUE)
  
  imIndia202021fc8 <- imIndia202021fc8[imIndia202021fc8[,'Y202122']>2,]
  imIndia202021fc2 <- imIndia202021fc2[imIndia202021fc2[,'Y202122']>5,]
  imIndia202021fcs <- imIndia202021fcs[imIndia202021fcs[,'Y202122']>10,]
  
  
  #sector
  aname<- str_c("Imports from " ,country1 ," (2020-21)", sep='')
  imdsannexure <- imIndia202021fcs
  imdsannexure <- imdsannexure[with(imdsannexure, order(-per)),]
  imdsannexure <- tibble::rowid_to_column(imdsannexure, "ID")
  colnames(imdsannexure) <- c("S.No.","Sector",aname,"Total Imports","Dependence Percentage (%)")
  
  imIndia202021cds <- imIndia202021fcs[imIndia202021fcs[,'per']>2,]
  
  colnames(imIndia202021cds) <- c("Sector",aname,"Total Imports","Dependence Percentage (%)")
  imIndia202021cds<-imIndia202021cds[order(-imIndia202021cds$`Dependence Percentage (%)`),]
  row.names(imIndia202021cds) <- NULL
  imIndia202021cds <- cbind(Rank = rownames(imIndia202021cds), imIndia202021cds)
  try(rownames(imIndia202021cds) <- 1:nrow(imIndia202021cds))
  imIndia202021cds$Sector <- str_to_title(imIndia202021cds$Sector)
  
  if(nrow(imIndia202021cds) < 3){
    
    imIndia202021cds <- imIndia202021fcs[with(imIndia202021fcs, order(-per)),] %>% top_n(3)
    aname<- str_c("Imports from " ,country1 ,"(2021-22)", sep='')
    colnames(imIndia202021cds) <- c("Sector",aname,"Total Imports","Dependence Percentage (%)")
    imIndia202021cds<-imIndia202021cds[order(-imIndia202021cds$`Dependence Percentage`),]
    row.names(imIndia202021cds) <- NULL
    imIndia202021cds <- cbind(Rank = rownames(imIndia202021cds), imIndia202021cds)
    try(rownames(imIndia202021cds) <- 1:nrow(imIndia202021cds))
    imIndia202021cds$Sector <- str_to_title(imIndia202021cds$Sector)
  }
  
  #chapter
  imdcannexure <- imIndia202021fc2
  imdcannexure <- imdcannexure[with(imdcannexure, order(-per)),]
  imdcannexure <- tibble::rowid_to_column(imdcannexure, "ID")
  colnames(imdcannexure) <- c("S.No.","Chapter Code",aname,"Total Imports","Dependence Percentage (%)","Chapter Description")
  
  imIndia202021cd2 <- imIndia202021fc2[imIndia202021fc2[,'per']>5,]
  aname<- str_c("Imports from " ,country1 ," (2021-22)", sep='')
  colnames(imIndia202021cd2) <- c("Chapter Code",aname,"Total Imports","Dependence Percentage (%)","Chapter Description")
  imIndia202021cd2<-imIndia202021cd2[order(-imIndia202021cd2$`Dependence Percentage`),]
  row.names(imIndia202021cd2) <- NULL
  imIndia202021cd2 <- cbind(Rank = rownames(imIndia202021cd2), imIndia202021cd2)
  try(rownames(imIndia202021cd2) <- 1:nrow(imIndia202021cd2))
  imIndia202021cd2$`Chapter Code` <- str_to_title(imIndia202021cd2$`Chapter Code`)
  
  if(nrow(imIndia202021cd2) < 5){
    
    imIndia202021cd2 <- imIndia202021fc2[with(imIndia202021fc2, order(-per)),] %>% top_n(5,per)
    aname<- str_c("Imports from " ,country1 ,"(2021-22)", sep='')
    colnames(imIndia202021cd2) <- c("Chapter Code",aname,"Total Imports","Dependence Percentage (%)","Chapter Description")
    imIndia202021cd2<-imIndia202021cd2[order(-imIndia202021cd2$`Dependence Percentage`),]
    row.names(imIndia202021cd2) <- NULL
    imIndia202021cd2 <- cbind(Rank = rownames(imIndia202021cd2), imIndia202021cd2)
    try(rownames(imIndia202021cd2) <- 1:nrow(imIndia202021cd2))
    imIndia202021cd2$`Chapter Code` <- str_to_title(imIndia202021cd2$`Chapter Code`)
  }
  
  
  # commodity
  
  imIndia202021fc8[is.na(imIndia202021fc8)] <- 0
  imIndia202021cd8 <- imIndia202021fc8[imIndia202021fc8[,'per']>50,]
  aname<- str_c("Imports from " ,country1 ," (2021-22)", sep='')
  colnames(imIndia202021cd8) <- c("Commodity",aname,"Total Imports","Dependence Percentage (%)","Description")
  imIndia202021cd8<-imIndia202021cd8[order(-imIndia202021cd8$`Dependence Percentage`),]
  row.names(imIndia202021cd8) <- NULL
  imIndia202021cd8 <- cbind(Rank = rownames(imIndia202021cd8), imIndia202021cd8)
  try(rownames(imIndia202021cd8) <- 1:nrow(imIndia202021cd8))
  imIndia202021cd8$Commodity <- str_to_title(imIndia202021cd8$Commodity)
  
  if(nrow(imIndia202021cd8) < 5){
    
    imIndia202021cd8 <- imIndia202021fc8[with(imIndia202021fc8, order(-per)),] %>% top_n(5,per)
    aname<- str_c("Imports from " ,country1 ,"(2021-22)", sep='')
    colnames(imIndia202021cd8) <- c("Commodity",aname,"Total Imports","Dependence Percentage (%)","Description")
    imIndia202021cd8<-imIndia202021cd8[order(-imIndia202021cd8$`Dependence Percentage`),]
    row.names(imIndia202021cd8) <- NULL
    imIndia202021cd8 <- cbind(Rank = rownames(imIndia202021cd8), imIndia202021cd8)
    try(rownames(imIndia202021cd8) <- 1:nrow(imIndia202021cd8))
    imIndia202021cd8$Commodity <- str_to_title(imIndia202021cd8$Commodity)
  }
  
  
  
  ############ Exports
  
  keeps <- c("code", "Y202122")
  imIndia202021f <- exIndia202021[keeps]
  imIndia202021f8 <- aggregate(Y202122 ~ code, imIndia202021f, sum)
  colnames(imIndia202021f8) <- c("code", "Y202122sum8")
  keeps <- c("hsc2d", "Y202122")
  imIndia202021f <- exIndia202021[keeps]
  imIndia202021f2 <- aggregate(Y202122 ~ hsc2d, imIndia202021f, sum)
  colnames(imIndia202021f2) <- c("hsc2d", "Y202122sum2")
  keeps <- c("Sector", "Y202122")
  imIndia202021f <- exIndia202021[keeps]
  imIndia202021fs <- aggregate(Y202122 ~ Sector, imIndia202021f, sum)
  colnames(imIndia202021fs) <- c("Sector", "Y202122sums")
  
  keeps <- c("code", "Y202122")
  imIndia202021f <- imIndia202021fe[keeps]
  imIndia202021fc8 <- aggregate(Y202122 ~ code, imIndia202021f, sum)
  colnames(imIndia202021fc8) <- c("code", "Y202122")
  keeps <- c("hsc2d", "Y202122")
  imIndia202021f <- imIndia202021fe[keeps]
  imIndia202021fc2 <- aggregate(Y202122 ~ hsc2d, imIndia202021f, sum)
  colnames(imIndia202021fc2) <- c("hsc2d", "Y202122")
  keeps <- c("Sector", "Y202122")
  imIndia202021f <- imIndia202021fe[keeps]
  imIndia202021fcs <- aggregate(Y202122 ~ Sector, imIndia202021f, sum)
  colnames(imIndia202021fcs) <- c("Sector", "Y202122")
  
  imIndia202021fc8 <- merge(x= imIndia202021fc8, y = imIndia202021f8, by = 'code', all.x = TRUE)
  imIndia202021fc2 <- merge(x= imIndia202021fc2, y = imIndia202021f2, by = 'hsc2d', all.x = TRUE)
  imIndia202021fcs <- merge(x= imIndia202021fcs, y = imIndia202021fs, by = 'Sector', all.x = TRUE)
  
  imIndia202021fc8$per <- (imIndia202021fc8$Y202122/imIndia202021fc8$Y202122sum8)*100
  imIndia202021fc2$per <- (imIndia202021fc2$Y202122/imIndia202021fc2$Y202122sum2)*100
  imIndia202021fcs$per <- (imIndia202021fcs$Y202122/imIndia202021fcs$Y202122sums)*100
  imIndia202021fc8 <-imIndia202021fc8 %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fc2 <-imIndia202021fc2 %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fcs <-imIndia202021fcs %>% mutate_at(vars(per), funs(round(., 2)))
  imIndia202021fc2 <- merge(x = imIndia202021fc2, y = codes2, by.x= 'hsc2d',by.y= 'HSCode', all.x = TRUE)
  imIndia202021fc8$code <- trim(imIndia202021fc8$code)
  imIndia202021fc8 <- merge(x = imIndia202021fc8, y = codes8, by= 'code', all.x = TRUE)
  
  imIndia202021fc8 <- imIndia202021fc8[imIndia202021fc8[,'Y202122']>2,]
  imIndia202021fc2 <- imIndia202021fc2[imIndia202021fc2[,'Y202122']>5,]
  imIndia202021fcs <- imIndia202021fcs[imIndia202021fcs[,'Y202122']>10,]
  
  
  #sector
  aname<- str_c("Exports to " ,country1 ," (2021-22)", sep='')
  exdsannexure <- imIndia202021fcs
  exdsannexure <- exdsannexure[with(exdsannexure, order(-per)),]
  exdsannexure <- tibble::rowid_to_column(exdsannexure, "ID")
  colnames(exdsannexure) <- c("S.No.","Sector",aname,"Total Exports","Dependence Percentage (%)")
  
  exIndia202021cds <- imIndia202021fcs[imIndia202021fcs[,'per']>2,]
  
  colnames(exIndia202021cds) <- c("Sector",aname,"Total Exports","Dependence Percentage (%)")
  exIndia202021cds<-exIndia202021cds[order(-exIndia202021cds$`Dependence Percentage`),]
  row.names(exIndia202021cds) <- NULL
  exIndia202021cds <- cbind(Rank = rownames(exIndia202021cds), exIndia202021cds)
  try(rownames(exIndia202021cds) <- 1:nrow(exIndia202021cds))
  exIndia202021cds$Sector <- str_to_title(exIndia202021cds$Sector)
  
  if(nrow(exIndia202021cds) < 3){
    
    exIndia202021cds <- imIndia202021fcs[with(imIndia202021fcs, order(-per)),] %>% top_n(3)
    aname<- str_c("Exports to " ,country1 ,"(2021-22)", sep='')
    colnames(exIndia202021cds) <- c("Sector",aname,"Total Exports","Dependence Percentage (%)")
    exIndia202021cds<-exIndia202021cds[order(-exIndia202021cds$`Dependence Percentage`),]
    row.names(exIndia202021cds) <- NULL
    exIndia202021cds <- cbind(Rank = rownames(exIndia202021cds), exIndia202021cds)
    try(rownames(exIndia202021cds) <- 1:nrow(exIndia202021cds))
    exIndia202021cds$Sector <- str_to_title(exIndia202021cds$Sector)
  }
  
  #chapter
  exdcannexure <- imIndia202021fc2
  exdcannexure <- exdcannexure[with(exdcannexure, order(-per)),]
  exdcannexure <- tibble::rowid_to_column(exdcannexure, "ID")
  colnames(exdcannexure) <- c("S.No.","Chapter Code",aname,"Total Exports","Dependence Percentage (%)", "Chapter Description")
  
  exIndia202021cd2 <- imIndia202021fc2[imIndia202021fc2[,'per']>5,]
  aname<- str_c("Exports to " ,country1 ," (2021-22)", sep='')
  colnames(exIndia202021cd2) <- c("Chapter Code",aname,"Total Exports","Dependence Percentage (%)","Chapter Description")
  exIndia202021cd2<-exIndia202021cd2[order(-exIndia202021cd2$`Dependence Percentage`),]
  row.names(exIndia202021cd2) <- NULL
  exIndia202021cd2 <- cbind(Rank = rownames(exIndia202021cd2), exIndia202021cd2)
  try(rownames(exIndia202021cd2) <- 1:nrow(exIndia202021cd2))
  exIndia202021cd2$`Chapter Code` <- str_to_title(exIndia202021cd2$`Chapter Code`)
  
  if(nrow(exIndia202021cd2) < 5){
    
    exIndia202021cd2 <- imIndia202021fc2[with(imIndia202021fc2, order(-per)),] %>% top_n(5,per)
    aname<- str_c("Exports to " ,country1 ,"(2021-22)", sep='')
    colnames(exIndia202021cd2) <- c("Chapter Code",aname,"Total Exports","Dependence Percentage (%)","Chapter Description")
    exIndia202021cd2<-exIndia202021cd2[order(-exIndia202021cd2$`Dependence Percentage`),]
    row.names(exIndia202021cd2) <- NULL
    exIndia202021cd2 <- cbind(Rank = rownames(exIndia202021cd2), exIndia202021cd2)
    try(rownames(exIndia202021cd2) <- 1:nrow(exIndia202021cd2))
    exIndia202021cd2$`Chapter Code` <- str_to_title(exIndia202021cd2$`Chapter Code`)
  }
  
  
  # commodity
  
  imIndia202021fc8[is.na(imIndia202021fc8)] <- 0
  exIndia202021cd8 <- imIndia202021fc8[imIndia202021fc8[,'per']>50,]
  aname<- str_c("Exports to " ,country1 ," (2021-22)", sep='')
  colnames(exIndia202021cd8) <- c("Commodity",aname,"Total Exports","Dependence Percentage (%)","Commodity Description")
  exIndia202021cd8<-exIndia202021cd8[order(-exIndia202021cd8$`Dependence Percentage`),]
  row.names(exIndia202021cd8) <- NULL
  exIndia202021cd8 <- cbind(Rank = rownames(exIndia202021cd8), exIndia202021cd8)
  try(rownames(exIndia202021cd8) <- 1:nrow(exIndia202021cd8))
  exIndia202021cd8$Commodity <- str_to_title(exIndia202021cd8$Commodity)
  
  if(nrow(exIndia202021cd8) < 5){
    
    exIndia202021cd8 <- imIndia202021fc8[with(imIndia202021fc8, order(-per)),] %>% top_n(5,per)
    aname<- str_c("Exports to " ,country1 ,"(2021-22)", sep='')
    colnames(exIndia202021cd8) <- c("Commodity",aname,"Total Exports","Dependence Percentage (%)","Commodity Description")
    exIndia202021cd8<-exIndia202021cd8[order(-exIndia202021cd8$`Dependence Percentage`),]
    row.names(exIndia202021cd8) <- NULL
    exIndia202021cd8 <- cbind(Rank = rownames(exIndia202021cd8), exIndia202021cd8)
    try(rownames(exIndia202021cd8) <- 1:nrow(exIndia202021cd8))
    exIndia202021cd8$`Commodity Description` <- str_to_title(exIndia202021cd8$`Commodity Description`)
  }
  
  
  ############## Movers and Shakers
  
  ## Imports
  
  keeps <- c("code", "Y202122","Y202021")
  imIndia202021f <- imIndia202021filter[keeps]
  imIndia202021fcm8 <- aggregate(.~ code, imIndia202021f, sum)
  
  x<- sum(imIndia202021fcm8$Y202122)
  y <- length(imIndia202021fcm8$Y202122)
  imIndia202021fcm8$per <- ((imIndia202021fcm8$Y202122 - imIndia202021fcm8$Y202021)/imIndia202021fcm8$Y202021)*100
  imIndia202021fcm8$code <- trim(imIndia202021fcm8$code)
  imIndia202021fcm8 <- merge(x = imIndia202021fcm8, y = codes8, by= 'code', all.x = TRUE)
  imIndia202021fcm8<- imIndia202021fcm8[!is.na(imIndia202021fcm8$Commodity), ]
  imIndia202021fcm8 <-imIndia202021fcm8 %>% mutate_at(vars(per), funs(round(., 2)))
  
  imIndia202021fcm8<-filter(imIndia202021fcm8,imIndia202021fcm8$Y202122 >= (x/y)*10)
  imIndia202021fcm8<-filter(imIndia202021fcm8,imIndia202021fcm8$Y202021 >= (x/y)*10 )
  imIndia202021fcm8pos<-filter(imIndia202021fcm8,imIndia202021fcm8$per > 0)
  imIndia202021fcm8neg<-filter(imIndia202021fcm8,imIndia202021fcm8$per < 0)
  imIndia202021fcm8pos <- imIndia202021fcm8pos[with(imIndia202021fcm8pos, order(-per)),] %>% head(3)
  imIndia202021fcm8neg <- imIndia202021fcm8neg[with(imIndia202021fcm8neg, order(per)),] %>% head(3)
  
  
  colnames(imIndia202021fcm8pos) <- c("HSCode","Imports 2021-22","Imports 2020-21","Percentage Change (%)","Commodity")
  row.names(imIndia202021fcm8pos) <- NULL
  imIndia202021fcm8pos <- cbind(Rank = rownames(imIndia202021fcm8pos), imIndia202021fcm8pos)
  try(rownames(imIndia202021fcm8pos) <- 1:nrow(imIndia202021fcm8pos))
  
  
  importms8pos <- imIndia202021fcm8pos
  
  colnames(imIndia202021fcm8neg) <- c("HSCode","Imports 2021-22","Imports 2020-21","Percentage Change (%)","Commodity")
  row.names(imIndia202021fcm8neg) <- NULL
  imIndia202021fcm8neg <- cbind(Rank = rownames(imIndia202021fcm8neg), imIndia202021fcm8neg)
  try(rownames(imIndia202021fcm8neg) <- 1:nrow(imIndia202021fcm8neg))
  
  importms8neg <- imIndia202021fcm8neg
  
  #### Sector
  
  keeps <- c("Sector", "Y202122","Y202021")
  imIndia202021f <- imIndia202021filter[keeps]
  imIndia202021fcms <- aggregate(.~ Sector, imIndia202021f, sum)
  
  x <- sum(imIndia202021fcms$Y202122)
  y <- length(imIndia202021fcms$Y202122)
  imIndia202021fcms$per <- ((imIndia202021fcms$Y202122 - imIndia202021fcms$Y202021)/imIndia202021fcms$Y202021)*100
  imIndia202021fcms <-imIndia202021fcms %>% mutate_at(vars(per), funs(round(., 2)))
  immsannexure <- imIndia202021fcms
  immsannexure <- immsannexure[with(immsannexure, order(-per)),]
  immsannexure <- tibble::rowid_to_column(immsannexure, "ID")
  colnames(immsannexure) <- c("S.No.","Sector","Imports 2021-22","Imports 2020-21","Percentage Change (%)")
  
  imIndia202021fcms<-filter(imIndia202021fcms,imIndia202021fcms$Y202122 >= (x/y)/10)
  imIndia202021fcmspos<-filter(imIndia202021fcms,imIndia202021fcms$per > 0)
  imIndia202021fcmsneg<-filter(imIndia202021fcms,imIndia202021fcms$per < 0)
  imIndia202021fcmspos <- imIndia202021fcmspos[with(imIndia202021fcmspos, order(-per)),] %>% top_n(5)
  imIndia202021fcmsneg <- imIndia202021fcmsneg[with(imIndia202021fcmsneg, order(per)),] %>% top_n(-5)
  
  colnames(imIndia202021fcmspos) <- c("Sector","Imports 2021-22","Imports 2020-21","Percentage Change (%)")
  row.names(imIndia202021fcmspos) <- NULL
  imIndia202021fcmspos <- cbind(Rank = rownames(imIndia202021fcmspos), imIndia202021fcmspos)
  try(rownames(imIndia202021fcmspos) <- 1:nrow(imIndia202021fcmspos))
  
  importmsspos <- imIndia202021fcmspos
  
  importmsspos[1,2]
  
  colnames(imIndia202021fcmsneg) <- c("Sector","Imports 2021-22","Imports 2020-21","Percentage Change (%)")
  row.names(imIndia202021fcmsneg) <- NULL
  imIndia202021fcmsneg <- cbind(Rank = rownames(imIndia202021fcmsneg), imIndia202021fcmsneg)
  try(rownames(imIndia202021fcmsneg) <- 1:nrow(imIndia202021fcmsneg))
  
  importmssneg <- imIndia202021fcmsneg
  
  
  
  ### Export
  
  keeps <- c("code", "Y202122","Y202021")
  exIndia202021f <- imIndia202021fe[keeps]
  imIndia202021fcm8 <- aggregate(.~ code, exIndia202021f, sum)
  
  x<- sum(imIndia202021fcm8$Y202122)
  y <- length(imIndia202021fcm8$Y202122)
  imIndia202021fcm8$per <- ((imIndia202021fcm8$Y202122 - imIndia202021fcm8$Y202021)/imIndia202021fcm8$Y202021)*100
  imIndia202021fcm8$code <- trim(imIndia202021fcm8$code)
  imIndia202021fcm8 <- merge(x = imIndia202021fcm8, y = codes8, by= 'code', all.x = TRUE)
  imIndia202021fcm8<- imIndia202021fcm8[!is.na(imIndia202021fcm8$Commodity), ]
  imIndia202021fcm8 <-imIndia202021fcm8 %>% mutate_at(vars(per), funs(round(., 2)))
  
  imIndia202021fcm8<-filter(imIndia202021fcm8,imIndia202021fcm8$Y202122 >= (x/y)*10)
  imIndia202021fcm8<-filter(imIndia202021fcm8,imIndia202021fcm8$Y202021 > 0 )
  imIndia202021fcm8pos<-filter(imIndia202021fcm8,imIndia202021fcm8$per > 0)
  imIndia202021fcm8neg<-filter(imIndia202021fcm8,imIndia202021fcm8$per < 0)
  imIndia202021fcm8pos <- imIndia202021fcm8[with(imIndia202021fcm8, order(-per)),] %>% head(3)
  imIndia202021fcm8neg <- imIndia202021fcm8[with(imIndia202021fcm8, order(per)),] %>% head(3)
  
  
  colnames(imIndia202021fcm8pos) <- c("HSCode","Exports 2021-22","Exports 2020-21","Percentage Change (%)","Commodity")
  row.names(imIndia202021fcm8pos) <- NULL
  imIndia202021fcm8pos <- cbind(Rank = rownames(imIndia202021fcm8pos), imIndia202021fcm8pos)
  try(rownames(imIndia202021fcm8pos) <- 1:nrow(imIndia202021fcm8pos))
  
  exportms8pos <- imIndia202021fcm8pos
  
  colnames(imIndia202021fcm8neg) <- c("HSCode","Exports 2021-22","Exports 2020-21","Percentage Change (%)","Commodity")
  row.names(imIndia202021fcm8neg) <- NULL
  imIndia202021fcm8neg <- cbind(Rank = rownames(imIndia202021fcm8neg), imIndia202021fcm8neg)
  try(rownames(imIndia202021fcm8neg) <- 1:nrow(imIndia202021fcm8neg))
  
  exportms8neg <- imIndia202021fcm8neg
  
  #### Sector
  
  keeps <- c("Sector", "Y202122","Y202021")
  imIndia202021f <- imIndia202021fe[keeps]
  imIndia202021fcms <- aggregate(.~ Sector, imIndia202021f, sum)
  
  x <- sum(imIndia202021fcms$Y202122)
  y <- length(imIndia202021fcms$Y202122)
  imIndia202021fcms$per <- ((imIndia202021fcms$Y202122 - imIndia202021fcms$Y202021)/imIndia202021fcms$Y202021)*100
  imIndia202021fcms <-imIndia202021fcms %>% mutate_at(vars(per), funs(round(., 2)))
  exmsannexure <- imIndia202021fcms
  exmsannexure <- exmsannexure[with(exmsannexure, order(-per)),]
  exmsannexure <- tibble::rowid_to_column(exmsannexure, "ID")
  colnames(exmsannexure) <- c("S.No.","Sector","Exports 2021-22","Exports 2020-21","Percentage Change (%)")
  
  imIndia202021fcms<-filter(imIndia202021fcms,imIndia202021fcms$Y202122 >= (x/y)/10)
  imIndia202021fcmspos<-filter(imIndia202021fcms,imIndia202021fcms$per > 0)
  imIndia202021fcmsneg<-filter(imIndia202021fcms,imIndia202021fcms$per < 0)
  imIndia202021fcmspos <- imIndia202021fcmspos[with(imIndia202021fcmspos, order(-per)),] %>% top_n(5)
  imIndia202021fcmsneg <- imIndia202021fcmsneg[with(imIndia202021fcmsneg, order(per)),] %>% top_n(-5)
  
  colnames(imIndia202021fcmspos) <- c("Sector","Exports 2021-22","Exports 2020-21","Percentage Change (%)")
  row.names(imIndia202021fcmspos) <- NULL
  imIndia202021fcmspos <- cbind(Rank = rownames(imIndia202021fcmspos), imIndia202021fcmspos)
  try(rownames(imIndia202021fcmspos) <- 1:nrow(imIndia202021fcmspos))
  
  exportmsspos <- imIndia202021fcmspos
  
  colnames(imIndia202021fcmsneg) <- c("Sector","Exports 2021-22","Exports 2020-21","Percentage Change (%)")
  row.names(imIndia202021fcmsneg) <- NULL
  imIndia202021fcmsneg <- cbind(Rank = rownames(imIndia202021fcmsneg), imIndia202021fcmsneg)
  try(rownames(imIndia202021fcmsneg) <- 1:nrow(imIndia202021fcmsneg))
  
  exportmssneg <- imIndia202021fcmsneg
  
  ########## Rank changers
  
  
  keeps <- c("code", "Y202122")
  imIndia202021r <- imIndia202021filter[keeps]
  imIndia202021r <- aggregate(.~ code, imIndia202021r, sum)
  imIndia202021r <- imIndia202021r[with(imIndia202021r, order(-Y202122)),]
  row.names(imIndia202021r) <- NULL
  imIndia202021r <- cbind(Rank_202122 = rownames(imIndia202021r), imIndia202021r)
  try(rownames(imIndia202021r) <- 1:nrow(imIndia202021r))
  im8rank202122 <- imIndia202021r
  
  keeps <- c("code", "Y202021")
  imIndia202021r <- imIndia202021filter[keeps]
  imIndia202021r <- aggregate(.~ code, imIndia202021r, sum)
  imIndia202021r <- imIndia202021r[with(imIndia202021r, order(-Y202021)),]
  row.names(imIndia202021r) <- NULL
  imIndia202021r <- cbind(Rank_202021 = rownames(imIndia202021r), imIndia202021r)
  im8rank202021 <- imIndia202021r
  
  suppressMessages( rmarkdown::render(input="20220916_Country_Report.Rmd", output_format = "word_document", output_file = paste0("Trade_Report_",country1,".docx") ) )
  
  i <- i+1
}

dir.create(country1)
suppressMessages( rmarkdown::render(input="20220916_Country_Report.Rmd", output_format = "word_document", output_file = paste0("Trade_Report_new",country1,".docx"), output_dir = country1 ) )

nosector <- "[No significant sector]"
cat ("String 1 is : ", str1)
cat(nosector)



