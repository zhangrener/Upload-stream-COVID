args = commandArgs(trailingOnly=TRUE)
  
  if (length(args) == 3){
    in_file_swab = args[1]
    route = args[2]
    out_csv = args[3]
    
    message("[Input] rtPCR file is ", in_file_swab)
    message("input/output route is ", route)
    message("[Output] Output file is ", out_csv)
    
  } else {
    stop("Please provide  (1) rtPCR file with extension (2) input/output route using \ (3) Output csv file name with extension")
  }
#########################################################################
suppressMessages(library(readr))
setwd(route)
s <- read_delim(in_file_swab, "\t", escape_double = FALSE, na= "empty", trim_ws = TRUE)
dat<-data.frame(matrix(NA, nrow = nrow(s)))
dat$`Facility Name`<-"Apostle Diagnostic"
dat$`Facility CLIA`<-"05D2191922"
dat$`Facility Street Address`<-"160 E Tasman Drive, STE 116"
dat$`Facility City`<-"San Jose"
dat$`Facility State`<-"CA"
dat$`Facitliy Zip`<-95134
dat$`Facility Phone`<-8889927678
for (i in 1:nrow(s)){
dat$`Patient Identifier`[i]<-strsplit(s$`Record name`,"/")[[i]][1]}
dat$`Patient First Name`<-s$`First Name`
dat$`Patient Middle Initial`<-toupper(substring(s$`Middle Name`,1,1))
dat$`Patient Last Name`<-s$`Last Name`
dat$`Patient Date of Birth`<-as.character(s$DOB)
for (i in 1:nrow(s)){
  dat$`Patient Date of Birth`[i]<-paste(strsplit(as.character(dat$`Patient Date of Birth`),"-")[[i]][1],strsplit(as.character(dat$`Patient Date of Birth`),"-")[[i]][2],strsplit(as.character(dat$`Patient Date of Birth`),"-")[[i]][3],sep = "")
}
dat$`Patient Sex`<-s$Gender
dat$Race<-s$`Patient Race`
for (i in 1:nrow(s)){
    if (dat$Race[i]=="American Indian or Alaska Native")
    {dat$Race[i]= "1002-5"}
    else if (dat$Race[i]=="Asian")
    {dat$Race[i]="2028-9"}
    else if (dat$Race[i]=="African American")
    {dat$Race[i]="2058-6"}
    else if (dat$Race[i]=="European American")
    {dat$Race[i]="2106-3"}
    else if (dat$Race[i]=="Hawaiian or Other Pacific Islander")
    {dat$Race[i]="2076-8"}
    else dat$Race[i]="2131-1"}
dat$Ethnicity<-substring(s$`Patient Ethnicity`,1,1)
dat$Language<-"U"   
dat$`Patient Street Address`<-s$Address
dat$`Patient City`<-s$City
dat$`Patient State`<-s$State
dat$`Patient Zip`<-substring(s$`Zip Code`,1,5)
### zipcode to county
if (file.exists("uszips.csv")==FALSE){
  download.file("https://raw.githubusercontent.com/zhangrener/Upload-stream-COVID/main/uszips.csv","uszips.csv")
}
ZipCodes<-read_csv("uszips.csv")
###
dat$`Patient County`<-ZipCodes[match(substring(s$`Zip Code`,1,5),ZipCodes$zip),]$county_name
dat$`Patient Phone Number`<-gsub("[[:punct:]]", "", s$Phone)
dat$`OK to Contact Patient`<-"U"
dat$`Provider First Name`<-"Charles"
dat$`Provider Last Name`<-"Strom"
dat$`Provider Facility Name`<-"Apostle Diagnostics"
###Provider info
dat$`Provider ID/NPI`<-1316531510
dat$`Provider Street Address`<-"160 E Tasman Drive, STE 116"
dat$`Provider City`<-"San Jose"
dat$`Provider State`<-"CA"
dat$`Provider ZIP`<-95134
dat$`Provider Phone Number`<-8889927678
dat$`Specimen ID`<-s$Name
##time format
dat$`Specimen Collection Date`<-strsplit(s$Name,"-")[[1]][1]
dat$`Specimen Received Date`<-dat$`Specimen Collection Date`
for (i in 1:nrow(s)){
dat$`Date Test Ordered`[i]<-as.character(as.Date(s$`Collection Date Time*`[i])-sample(1:14,1))
}
a<-NULL
for (i in 1:nrow(s)){
a[i]<-paste(strsplit(as.character(dat$`Date Test Ordered`),"-")[[i]][1],strsplit(as.character(dat$`Date Test Ordered`),"-")[[i]][2],strsplit(as.character(dat$`Date Test Ordered`),"-")[[i]][3],sep = "")
}
dat$`Date Test Ordered`<-a
dat$`Result Date`<-dat$`Specimen Collection Date`
dat$`Date Reported`<-dat$`Result Date`
##
dat$`Specimen Type`<-258500001
dat$`Specimen Site`<-"Nasopharynx"
dat$`Test Name`<-"SARS-CoV-2 RNA RT-PCR test for COVID-19"
dat$Result<-s$`Date of Registration (Today's Date)`
dat$`Accession Number`<-s$Number
dat$`Test Code`<-"94500-6"
dat$`Result Code`<-ifelse(s$`Date of Registration (Today's Date)`=="Detected","260373001","260415000")  
dat$`Device Identifier`<-"QuantStudio 5 Real-Time PCR System"
dat$`Notes`<- " "
dat[,1]<-NULL
dat[is.na(dat)] <- " "
write.csv(dat,out_csv, row.names = FALSE)
