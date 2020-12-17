library(data.table)
library(zoo)
library(XML)
library(lubridate)
#
# Instructions on use:
# 
# 1. Workout on your S22i, wearing your HRM.  I do not start a Garmin activity on my watch.
# 2. Download both the CSV and the TCX files from your iFit account
# 3. Load functions below.
# 4. Run `example_usage` command, taking care to specify the directory where both CSV and TCX files are located.
# 5. Upload the new (??_better.tcx) file to Garmin Connect as a new activity.
# 6. ...
# 7. Profit!
#
#
example_usage <- function() {
  a <- meld_iFit_S22i_CSV_TCX_to_better_TCX(chr.file = '2020_10_24_16_10_Santa_Maria_de_Vilalba_Ride,_Barcelona,_Spain')
  a <- meld_iFit_S22i_CSV_TCX_to_better_TCX(chr.file = '2020_10_24_16_10_Lupine_Trail_Steady-State_Ride,_Crested_Butte,_Colorado')
  a <- meld_iFit_S22i_CSV_TCX_to_better_TCX(chr.file = '2020_10_26_11_10_Arakawa_River_Endurance_Ride,_Tokyo,_Japan')
  a <- meld_iFit_S22i_CSV_TCX_to_better_TCX(chr.file = '2020_10_27_11_10_Hill_Repeats')
  
}
#
# This function is intended to parse the TCX file format that comes from iFit.
# It assumes that there is only one lap represented, which has been true for 
# all of the workouts that I have done.
parseIFit <- function(chr.path = '~/Downloads/', chr.fileName = NA) {
  fl <- paste0(chr.path, chr.fileName)
  doc <- xmlParse(fl)
  tmp <- xmlToList(doc)$Activities$Activity$Lap$Track
  dt.temp <- rbindlist(lapply(tmp, function(rw){
    nm <- names(rw)
    if(any(nm == 'Position')) {
      p <- which(nm == 'Position')
      if(p == length(nm)) {
        nm <- c(nm[1:(p-1)], names(rw$Position))
      } else {
        nm <- c(nm[1:(p-1)], names(rw$Position), nm[(p+1):length(nm)])
      }
    }
    if(any(nm == 'HeartRateBpm')) nm <- c(nm[1:which(nm == "HeartRateBpm")], "HeartRateWorthless", nm[(which(nm == "HeartRateBpm")+1):length(nm)])
    vec <- as.vector(unlist(rw))
    if(length(vec) > 0) {
      dt <- data.table(t(vec))
      setnames(dt, nm)
    } else {
      dt <- data.table()
    }
    return(dt)
  }), fill = TRUE)
  dt.temp[, source :=  substr(chr.fileName, 1, nchar(chr.fileName) - 4) ]
  if(any(names(dt.temp) == "HeartRateWorthless")) dt.temp[, HeartRateWorthless := NULL]
  return(dt.temp)
}



# This function is designed to take both a CSV and TCX file from iFit (circa October 2020) and 
# turn it into a better TCX file that can be uploaded to Garmin that will include power.
# There are some limitations:
# 1. It assumes the existing structures of the iFit CSV and TCX files are unchanged (and it doesn't do any good checking that this is so)
# 2. It assumes the CSV will have every second represented, but the TCX might not.
# 3. It assumes other stuff, too.
#
meld_iFit_S22i_CSV_TCX_to_better_TCX <- function(chr.path = '~/Downloads/', chr.file = NA) {
  # If you don't have a filename, then fail
  if(is.na(chr.file)) error("Don't have a file to process")
  # If you don't have both the TCX and the CSV, then fail
  if(length(list.files(path = chr.path, pattern = paste0(chr.file, '.*'))) < 2) error("Don't have both input files")
  
  # Import the TCX file
  tcx <- parseIFit(chr.path = chr.path, chr.fileName = paste0(chr.file, '.tcx'))
  tcx[, `:=` (DistanceMeters = as.numeric(DistanceMeters), RPM = as.numeric(Cadence), Calories = as.numeric(Calories), HR = as.numeric(HeartRateBpm))]
  tcx[, fromStartSec := as.numeric(difftime(gsub("T", " ", Time), gsub("T", " ", min(tcx$Time)), units = "secs"))]
  setkey(tcx, fromStartSec)
  
  # Import the CSV file
  csv <- data.table(read.csv2(file = paste0(chr.path, chr.file, '.csv'), header = TRUE, skip = 2, sep = ','))
  csv[, `:=` (Miles = as.numeric(Miles), MPH = as.numeric(MPH), HR = as.numeric(HR), Incline = as.numeric(Incline))]
  csv[, meanPower := rollmean(Watts, 1200, fill = NA, align = "right")]
  csv[, fromStartSec := as.numeric(substr(Time, 1, 2)) * 60 + as.numeric(substr(Time, 4, 5))]
  setkey(csv, fromStartSec)
  
  # roll merge
  dt <- tcx[csv, roll = TRUE]
  
  # Create base variables for the TCX output file
  chr.static.header <- '<?xml version="1.0" encoding="UTF-8"?>
            <TrainingCenterDatabase
              xsi:schemaLocation="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd"
              xmlns:ns5="http://www.garmin.com/xmlschemas/ActivityGoals/v1"
              xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2"
              xmlns:ns2="http://www.garmin.com/xmlschemas/UserProfile/v2"
              xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns4="http://www.garmin.com/xmlschemas/ProfileExtension/v1">
              <Activities>
                <Activity Sport="Biking">'
  chr.static.footer <- '        </Track>
                </Lap>
                <Creator xsi:type="Device_t">
                  <Name>New S22i</Name>
                </Creator>
              </Activity>
            </Activities>
          </TrainingCenterDatabase>'
  
  # Pull together summary data for the ride / lap to put into the TCX
  ride_duration <- max(dt$i.Time)
  start_time <- min(dt$Time)
  mn.HR <- ifelse(all(dt$i.HR == 0), 155, mean(dt[i.HR != 0]$i.HR))
  mx.HR <- ifelse(all(dt$i.HR == 0), 175, max(dt[HR < 200]$i.HR))
  mn.RPM <- ifelse(all(dt$i.RPM == 0), 85, mean(dt[i.RPM != 0]$i.RPM))
  chr.dynamic.header <- paste0('      <Id>', start_time, '</Id>
      <Lap StartTime="', start_time, '">
        <TotalTimeSeconds>', as.numeric(substr(ride_duration, 1, 2)) * 60 + as.numeric(substr(ride_duration, 4, 5)), '</TotalTimeSeconds>
        <DistanceMeters>', max(dt$Miles) * 1609.34, '</DistanceMeters>
        <MaximumSpeed>', max(dt$MPH) * 1609.34 / 3600, '</MaximumSpeed>
        <Calories>', round(max(dt$Calories), 0), '</Calories>
        <AverageHeartRateBpm>
          <Value>', round(mn.HR, 0), '</Value>
        </AverageHeartRateBpm>
        <MaximumHeartRateBpm>
          <Value>',mx.HR, '</Value>
        </MaximumHeartRateBpm>
        <Intensity>Active</Intensity>
        <Cadence>', round(mn.RPM,0), '</Cadence>
        <TriggerMethod>Manual</TriggerMethod>
        <Track>')
  
  # Construct a variable of each Trackpoint in the merged data.table
  # separate lines for readability
  dt[, fullTime := paste0(gsub(" ", "T", as.character(lubridate::ymd_hms(start_time) - ms(ride_duration) + ms(i.Time))), ".000Z") ]
  dt[, trackpoint := paste0("<Trackpoint>\n  <Time>", fullTime, "</Time>\n")]
  dt[!is.na(AltitudeMeters), trackpoint := paste0(trackpoint, "  <AltitudeMeters>", AltitudeMeters, "</AltitudeMeters>\n")]
  dt[, trackpoint := paste0(trackpoint, "  <DistanceMeters>", Miles * 1609.34, "</DistanceMeters>\n")]
  dt[i.HR != 0, trackpoint := paste0(trackpoint, "  <HeartRateBpm><Value>",round(i.HR, 0), "</Value></HeartRateBpm>\n")]
  dt[, trackpoint := paste0(trackpoint, "  <Cadence>",i.RPM, "</Cadence>\n")]
  dt[, trackpoint := paste0(trackpoint, "  <Extensions><ns3:TPX>\n    <ns3:Speed>", MPH * 1609.34 / 3600, "</ns3:Speed>\n")]
  dt[, trackpoint := paste0(trackpoint, "    <ns3:Watts>", Watts,"</ns3:Watts>\n  </ns3:TPX></Extensions>\n")]
  dt[, trackpoint := paste0(trackpoint, "</Trackpoint>")]
  
  # Create output variable and write it to a file
  output <- paste(c(chr.static.header, chr.dynamic.header, dt$trackpoint, chr.static.footer), collapse = "\n")
  output.file <- paste0(chr.path, chr.file, '_better.tcx')
  write(output, file = output.file)
  return(output.file)
}

