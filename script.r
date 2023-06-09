library(gsheet)
library(jsonlite)


# gsheeturl <- 'https://docs.google.com/spreadsheets/d/1AKlUnrm6CXb1Tf7xN2KGu1B6BB0OISqbeq88xFdgbos/edit#gid=996990613'
# 
# df <- gsheet2tbl(
#   url = gsheeturl,
#   sheetid = 'homicides'
# )

df <- read.csv('download-pbp-homicides/pbp-homicides.csv')

data <- list(
  homicides = list()
)

for (i in 1:nrow(df)) {
  h <- df[i,]
  
  l <- list()
  
  l$crimeLocationLatitude <- h$Latitude
  l$crimeSceneAddress <- h$Street
  
  l$coverage <- list(
    headline = h$Headline,
    link = h$Link
  )
  
  l$isUCRReportable <- NA
  l$wasArrestMade <- NA

  l$crimeDate <- c(
    list(
      date = h$Date,
      formatted = h$formatted_date
    )
  )
  
  l$crimeLocationLongitude <- h$Longitude
  l$suspects <- NA
  
  l$geojson <- list(
    geometry = list(
      type = 'Point',
      coordinates = c(
        l$crimeLocationLongitude,
        l$crimeLocationLatitude
      )
    ),
    type = 'Feature',
    properties = list(
      crimeSceneAddress = l$crimeSceneAddress,
      id = h$ID,
      victim = paste0(h$`First name`,' ',h$`Last name`),
      crimeDate = l$crimeDate$formatted
    )
  )
  
  l$wereChargesFiled <- NA
  
  l$victim <- list(
    bio = NA,
    popupOverride = NA,
    photo = NA,
    birthDate = NA,
    gender = ifelse(
      test = is.na(h$Sex),
      yes = 'unknown',
      no = tolower(h$Sex)
    ),
    race = ifelse(
      test = is.na(h$Race),
      yes = 'unknown',
      no = tolower(h$Race)
    ),
    ageAtDeath = list(
      raw = ifelse(
        test = is.na(h$Age),
        yes = 'unknown',
        no = h$Age
      ),
      formatted = paste0(h$Age,' years')
    ),
    lastName = h$`Last name`,
    fullName = l$geojson$properties$victim,
    id = h$ID
  )
  
  l$homicideAction <- ifelse(
    test = is.na(h$How),
    yes = 'unknown',
    no = tolower(h$How)
  )
  l$id <- h$ID
  l$publicDisplay <- T
  
  data$homicides[[i]] <- l
}

ljson <- toJSON(
  x = data,
  pretty = T,
  na = 'null',
  auto_unbox = T
)

ljsonmin <- toJSON(
  x = data,
  pretty = F,
  na = 'null',
  auto_unbox = T
)

write(
  x = ljson,
  file = 'pbp-homicides.json'
)

write(
  x = ljsonmin,
  file = 'pbp-homicides-min.json'
)



