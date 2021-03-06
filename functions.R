# Function get a link for daily download of all data
get_link<- function(start_date = Sys.Date()-7, end_date= Sys.Date()-1){
  start_date_formatted<-format(as.Date(start_date),"%d%b%Y")
  end_date_formatted<- format(as.Date(end_date), "%d%b%y")
  url<-paste0("https://www.essencefl.com/florida_5_1_19/servlet/PlainDataDetailsServlet?ccddFreeText=all&initTemp=all&initPulseOx=all&year=all&endDate=",end_date_formatted,"&medianIncomeGroup=all&ageCDCILI=all&censusRaceBlackPercGroup=all&medicalGrouping=all&ccddCategory=all&diagnosisType=all&geography=alachua&percentParam=noPercent&patientClass=all&predomHispanic=all&ageTenYear=all&geographySystem=hospitalregion&agedistribute=all&month=all&datasource=va_hosp&dispositionCategory=all&censusRaceAsianPercGroup=all&detector=probrepswitch&censusRaceOtherPercGroup=all&startDate=",start_date_formatted,"&predomRace=all&week=all&dom=all&censusRaceAmerIndPercGroup=all&dow=all&hospitalGrouping=all&censusRaceHawaiianPercGroup=all&doy=all&timeResolution=daily&patientLoc=all&timeInterval=all&clinicalImpression=all&agerange=all&dischargeDiagnosis=all&medicalGroupingSystem=essencesyndromes&sex=all&userId=1064&censusRaceWhitePercGroup=all&medicalSubGrouping=all&hospFacilityType=all&aqtTarget=datadetails&age=all&quarter=all")
  message("you just copied the link for all data to the clip board")
  
  # cat(url)
  # browseURL(url)
  writeClipboard(url, format = 1)
  
}

# Function to get a link just for visits of interest
get_voi <- function(start_date = Sys.Date()-7, end_date= Sys.Date()-1){
  start_date_formatted<-format(as.Date(start_date),"%d%b%Y")
  end_date_formatted<- format(as.Date(end_date), "%d%b%y")
  url <- paste0('https://www.essencefl.com/florida_5_1_19/servlet/PlainDataDetailsServlet?ccddFreeText=all&initTemp=all&initPulseOx=all&year=all&endDate=', end_date_formatted, '&medianIncomeGroup=all&ageCDCILI=all&censusRaceBlackPercGroup=all&medicalGrouping=recordsofinterest&ccddCategory=all&diagnosisType=all&geography=all&percentParam=noPercent&patientClass=all&predomHispanic=all&ageTenYear=all&geographySystem=hospital&agedistribute=all&month=all&datasource=va_hosp&dispositionCategory=all&censusRaceAsianPercGroup=all&detector=probrepswitch&censusRaceOtherPercGroup=all&startDate=', start_date_formatted, '&predomRace=all&week=all&dom=all&censusRaceAmerIndPercGroup=all&dow=all&hospitalGrouping=all&censusRaceHawaiianPercGroup=all&doy=all&timeResolution=daily&patientLoc=alachua&timeInterval=all&clinicalImpression=all&agerange=all&dischargeDiagnosis=all&medicalGroupingSystem=essencesyndromes&sex=all&userId=1064&censusRaceWhitePercGroup=all&medicalSubGrouping=all&hospFacilityType=all&aqtTarget=datadetails&age=all&quarter=all')
  message('you just copied the link for visits of interest to the clip board')
  writeClipboard(url, format = 1)
}

# Function to get a link just for ILI
get_ili <- function(start_date = "2011-01-01", end_date= Sys.Date()-1){
  start_date_formatted<-format(as.Date(start_date),"%d%b%Y")
  end_date_formatted<- format(as.Date(end_date), "%d%b%y")
  url <- paste0("https://www.essencefl.com/florida_5_1_19/servlet/PlainDataDetailsServlet?ccddFreeText=all&initTemp=all&initPulseOx=all&year=all&endDate=", end_date_formatted ,"&medianIncomeGroup=all&ageCDCILI=all&censusRaceBlackPercGroup=all&medicalGrouping=ili&ccddCategory=all&diagnosisType=all&geography=alachua&percentParam=noPercent&patientClass=all&predomHispanic=all&ageTenYear=all&geographySystem=hospitalregion&agedistribute=all&month=all&datasource=va_hosp&dispositionCategory=all&censusRaceAsianPercGroup=all&detector=probrepswitch&censusRaceOtherPercGroup=all&startDate=", start_date_formatted ,"&predomRace=all&week=all&dom=all&censusRaceAmerIndPercGroup=all&dow=all&hospitalGrouping=all&censusRaceHawaiianPercGroup=all&doy=all&timeResolution=daily&patientLoc=all&timeInterval=all&clinicalImpression=all&agerange=all&dischargeDiagnosis=all&medicalGroupingSystem=essencesyndromes&sex=all&userId=1064&censusRaceWhitePercGroup=all&medicalSubGrouping=all&hospFacilityType=all&aqtTarget=datadetails&age=all&quarter=all")
  writeClipboard(url, format = 1)
  message("you just copied the link for ili to the clip board")
}

# Devin's awesome map robot
make_map <- function(syndrome, color_palette = 'Purples'){
  # First, get all the data for the syndrome
  temp <- df[grepl(tolower(syndrome), tolower(df$Category_flat)),]

  # Get number of visits by zipcode
  temp_zip_code <- temp %>%
    group_by(zip_code = as.numeric(as.character(Zipcode))) %>%
    summarise(visits = n(),
              yesterday_visits = length(CCDD[Date == yesterday])) %>%
    mutate(visits = ifelse(is.na(visits), 0, visits),
           yesterday_visits = ifelse(is.na(yesterday_visits), 0, yesterday_visits))
  
  # Bring number of visits into our map data
  zip_map_temp <- zip_map # first just copying zip_map
  zip_map_temp@data <- left_join(zip_map_temp@data, temp_zip_code)
  
  # Plot with the coloring being a function of the number of visits
  
  # Define a color palette
  cols <- colorRampPalette(brewer.pal(9, color_palette))(max(zip_map_temp$visits, na.rm = TRUE) + 1)
  # Define another color palette just for yesterday
  cols_yesterday <- colorRampPalette(brewer.pal(9, color_palette))(max(zip_map_temp$yesterday_visits, na.rm = TRUE) +1)
  
  
  # to pick a palette, run display.brewer.all()
  
  par(mar = c(0,0,1,0))
  par(mfrow = c(1,2))
  # Plot map
  plot(zip_map_temp, col= cols[zip_map_temp$visits + 1], border = 'black')
  
  # Add number of visits
  text(coordinates(zip_map_temp), labels = zip_map_temp$visits, col = "darkgrey")
  
  # Add actual zip code
  text(coordinates(zip_map_temp), labels = zip_map_temp$zip_code, pos = 1, cex= 0.5)
  
  # Title
  title(main = paste(syndrome, "over last 7 days"))
  
  # Plot map for yesterday
  plot(zip_map_temp, col= cols_yesterday[zip_map_temp$yesterday_visits + 1], border = "black")
  
  # Add number of visits
  text(coordinates(zip_map_temp), labels = zip_map_temp$yesterday_visits, col = "darkgrey")
  
  # Add actual zip code
  text(coordinates(zip_map_temp), labels = zip_map_temp$zip_code, pos = 1, cex= 0.5)
  
  # Title
  title(main = paste(syndrome, "Yesterday"))
  
  
  par(mfrow = c(1,2))
  par(mar = c(5,4,2,2))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

