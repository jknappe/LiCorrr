library("tidyverse")

#---------------------------------------------
# read LiCor data files 
#---------------------------------------------

# find all .81x LiCor data files in /data folder
licorRawData =
  list.files(pattern = '\\.81x$',
             recursive = TRUE) %>%
  # then import data
  lapply(.,
         function(x)
           read_delim(
             x,
             delim = "\t",
             col_names = c(
               "Type", "Etime", "Date",
               "Tcham", "Pressure", "H2O", "CO2", "Cdry", "Tbench", 
               "T1", "T2", "T3", "T4", "V1", "V2", "V3", "V4",
               "LATITUDE", "LONGITUDE", "STATUS", "SPEED", "COURSE",
               "RH", "Tboard", "Vin",
               "CO2ABS", "H2OABS",
               "Hour", "DOY",
               "RAWCO2", "RAWCO2REF", "RAWH2O", "RAWH2OREF",
               "Annotation"
             )
           ) 
        ) %>%
  # then parse files to tipple
  bind_rows(.)

#---------------------------------------------
# extract flux data from LiCor data file 
#---------------------------------------------

# read licorRawData
fluxData = 
  licorRawData %>%
  # then filter for rows with measured data (== type 1 data)
  filter(Type == 1 & Etime >= 0) %>%
  # then select columns
  select(Date, Etime, Cdry, Tcham, Pressure, RH, V2, V3 ) %>%
  # then calculate obsID
  mutate(startObservation = ifelse(Etime == 0, TRUE, FALSE),
         obsID = cumsum(startObservation)) %>%
  select(-startObservation)
  

#---------------------------------------------
# extract metadata from LiCor data file 
#---------------------------------------------

# define rows to select and their names (in alphabetical order!)
rowSelect = c("File Name:", "Obs#:", "Port#:")
rowNames = c("sourceFile", "obsNumber", "portNumber:")


# read licorRawData
metaData =
  licorRawData %>%
  # then select meta data columns
  select(Type, Etime) %>%
  # then filter for defined rows
  filter(Type %in% rowSelect) %>%
  # then calculate obsID
  mutate(obsID = ceiling((row_number()) / length(rowSelect))) %>%
  # then spread data
  spread(Type, Etime)

# rename columns
names(metaData) = c("obsID", rowNames)

# clean global environment
rm(rowNames, rowSelect)


#---------------------------------------------
# join flux data and meta data 
#---------------------------------------------

# read licorRawData
licorData = 
  fluxData %>%
  # then join by obsID
  left_join(metaData, 
            by = "obsID")


#---------------------------------------------
# clean global environment 
#---------------------------------------------

rm(licorRawData, fluxData, metaData)


