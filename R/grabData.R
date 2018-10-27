#' Grab Data for a NWIS station from the USGS and NWM
#'
#' @param stationID staion id 
#' @param retro.path path to retro_nwm netcdf file
#' @param units default is cms, alternativly use cfs
#' @param filter if filter is TRUE use filtered USGS meta data
#' @return a data.frame
#' @export
#' @author Mike Johnson

grabData = function(stationID = NULL, retro.path = NULL, units = 'cms', filter = TRUE){
  
    if(is.null(retro.path)) { stop("Enter path to `retro_nwm` as `retro/path`")}
  
    if(!(units %in% c('cms', 'cfs'))) { stop("units must be `cms` or `cfs`")}
    
    convert = 35.314666212661
  
    nc = ncdf4::nc_open(retro.path, suppress_dimvals = TRUE)
    
    if(filter){ meta = lowflows::usgs_filter } else { meta = lowflows::usgs_meta }
    
    nwis.index = which(meta$siteID == stationID)
    
    findex = ncdf4::ncvar_get(nc, varid = 'feature_id')
    
    index = which(findex == meta$COMID[nwis.index])
    
    if(length(index) > 0){
    
    nwm = ncdf4::ncvar_get(nc, varid = 'streamflow',  start= c(index,1), count = c(1, nc$dim$time$len))
    
    df = data.frame(stationID = rep(stationID, 9131),
                    Date      = seq.Date(as.Date('1993-01-01'), as.Date("2017-12-31"),1),
                    comid     = rep(meta$COMID[nwis.index], 9131),
                    nwm       = dailyAvg(nwm, every = 24),
                    season    = rep(NA, 9131))
    
    df$month = as.numeric(format(df$Date, "%m"))

      for(i in 1:NROW(df)){
        if(any(df$month[i] ==  c(1,2,12))){ df$season[i] = 'winter'}
        if(any(df$month[i] ==  c(3:5))){    df$season[i] = 'spring'}
        if(any(df$month[i] ==  c(6:8))){    df$season[i] = 'summer'}
        if(any(df$month[i] ==  c(9:11))){   df$season[i] = 'fall'}
      }

    
    if(units == 'cfs') { df$nwm = df$nwm * convert}
    
    nwis = dataRetrieval::readNWISdv(stationID, parameterCd = '00060')
    
    tmp = nwis[nwis$Date >= as.Date('1993-01-01'),]
    tmp = tmp[tmp$Date <= as.Date('2017-12-31'),]
    
    min.date = min(tmp$Date)
    max.date = max(tmp$Date)
    
    complete = length(seq.Date(min.date, max.date, 1)) == NROW(tmp)
    
    if(complete){
    
    if(dim(tmp)[1] == 0) { message("This station does not have observations between 1993-01-01 and 2017-12-31")
      return(NULL)}
    
    df = df[df$Date >= min.date,]
    df = df[df$Date <= max.date,]
    
    if(units == 'cms'){ df[['nwis']] = tmp$X_00060_00003 /convert } else { df[['nwis']] = tmp$X_00060_00003 }
    
    df[['units']] = units
    
    ncdf4::nc_close(nc)
    return(df)
    } else { message("Complete Timeseries not available")
      return(NULL)}
} else { message("Index not found") }

}

