library(readr)
brands <- read_csv("POIN_MASTER_010319.csv", 
                   col_types = cols(zip = col_character()))
df <- read_csv("mosaic_byZip.csv")

#the latitude and longitude of the US boundary
West = -124.327629
East = -66.885666
North =  49.384359
South = 25.837377

#the loaction in the US mainland(may include some zip codes from MEXICO)
brands = brands[(brands$latitude>= South&brands$latitude<=North&brands$longitude>= West&brands$longitude<=East),]
brands$zip=gsub('-','',brands$zip)

#the US zip codes in 3 formats
#4 digits zip code (1013)
four = which(nchar(brands$zip) == 4)
#5 digits zip code (14620)
five = which(nchar(brands$zip) == 5)
#9 digits zip code (010130001) with None or at most one 0 at the beginning
nine = which(nchar(brands$zip) == 9) 

#select the US zip codes (4,5 or 9 digits)
cleaner = brands[c(four,five,nine),]
#the zip code with 9 digits and start with single 0 should be the US zip codes, convert it to 4 digits to match other data set
cleaner[grep("^0",cleaner$zip),]$zip = substr(cleaner[grep("^0",cleaner$zip),]$zip,2,5)

#convert the zip code with 9 digits and begin with not 0 to 5 digits zip code
cleaner[which(nchar(cleaner$zip) == 9),]$zip = substr(cleaner[which(nchar(cleaner$zip) == 9),]$zip,1,5)
#the zip codes contain character will be changed to NAs
cleaner$zip = as.numeric(cleaner$zip)
#(0)1000 is not the USA zip code
cleaner = cleaner[cleaner$zip >=1001,]
#remove the NAs and get the final location data
location = cleaner[which(!is.na(cleaner$zip)),]
#remove the NAs and get the final location data
location = cleaner[which(!is.na(cleaner$zip)),]
summary(location$zip)

#extract CVS stores
CVS = location[location$gitext == 'GEOCVS',]

#add CVS column to household features, 0 is no store, 1 is having stores
df$CVS = df$zip %in% CVS$zip
df[df$CVS == TRUE,214] <- 1
df[df$CVS == FALSE,214] <- 0
summary(df$CVS)

#extract Walgreens stores
Walgreens = location[location$gitext == 'GEOWALG',]

#add walgreens column to household features, 0 is no store, 1 is having stores
df_wal = df[,-214]
df_wal$walgreens = df_wal$zip %in% Walgreens$zip
df_wal[df_wal$walgreens == TRUE,214] <- 1
df_wal[df_wal$walgreens == FALSE,214] <- 0
summary(df_wal$walgreens)

