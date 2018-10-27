x = expand.grid(c('tot', "high", "low"), c("", "fall", "winter", "spring", "summer"), c("pbais", "nse", "br2", "rmse"))

fin = data.frame(matrix(vector(mode = 'numeric'), 0, 66, dimnames=list(c(), c("siteID", "huc8", "lat", "lon", "fdc.high", "fdc.low", gsub("..", ".", do.call(sprintf, c(x, '%s.%s.%s')), fixed = T)))),
                 stringsAsFactors=F)

system.time({
    for( i in 101:NROW(usgs_filter)){
    df = grabData(stationID = usgs_filter$siteID[i], retro.path = "/Users/mikejohnson/Downloads/nwm_retro.nc")  
    
    if(all(!is.null(df), !is.na(df$nwm), !is.na(df$nwis))){
      fdc_nwis = fdc(df, "nwis" )
      fdc.tops = fdc_curve(fdc_nwis, "nwis", c(.1, .9))
      top10 = df[df$nwis >= fdc.tops[1,2],]
      bottom10 = df[df$nwis <= fdc.tops[2,2],]
       
      fin[NROW(fin) + 1,] = c(
       usgs_filter$siteID[i],
       usgs_filter$huc8[i],
       usgs_filter$lat[i], 
       usgs_filter$lon[i], 
       round(fdc.tops[1,2], 2),
       round(fdc.tops[2,2], 2),
       get_summary(df),
       get_summary(top10),
       get_summary(bottom10))
    }
    
    message(paste0("Finsihed ", i , " of ", NROW(usgs_filter) ))
    }
  
  fin[,3:66] = sapply(fin[,3:66], as.numeric)
  
})


save(fin, file = '/Users/mikejohnson/Documents/GitHub/lowflows/data/first100.rda', compress = 'xz')
