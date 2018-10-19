#' @title Calculate Flow Duration Curve from Timeseries
#' @description  Given a data.frame and column of timeseries data, generate new column of flow excedence proabilites
#' @param df a data.frame
#' @param col_name The name of the column containing the streamflow values 
#' @return a data.frame
#' @export
#' @author Mike Johnson

fdc = function(df, col_name = NULL) {
    tmp = rank(-df[,col_name], na.last="keep")
    df[, paste0(col_name,".fdc")] <- tmp / (sum(!is.na(df[,col_name])) + 1)
  
    return(df)
}


#' @title Calculate Flow Duration Spline
#' @description Given a data.frame and column of excedence proabilites, generate new column of flow excedence proabilites
#' @param df a data.frame
#' @param col_name The name of the column containing the streamflow values 
#' @param prob Default NULL. If not NULL, function retruns data.frame of values at input excedence probabilites
#' @return a curve equation or a data.frame
#' @export
#' @author Mike Johnson

fdc_curve = function(df, col_name = NULL, prob = NULL) {
  fit = splinefun(df[,paste0(col_name,".fdc")], df[,col_name], method='natural')
  
  if(is.null(prob)){ return(fit) } else { return(data.frame(prob = prob, val = fit(prob))) }
}

#' @title Plot Flow Duration Curve
#' @description Given a data.frame (that has already been processed by \code{fdc}) and column of streamflow timeseries, generate a plot
#' @param df a data.frame
#' @param col_name The name of the column containing the streamflow values 
#' @param prob Default NULL. Input exedence probabilites (0-1) will be highlighted on plot
#' @return a plot
#' @export
#' @author Mike Johnson

fdc_plot = function(df, col_name = NULL, prob = NULL) {
  
  fit = fdc_curve(df, col_name = col_name)
  
  plot(c(df[,paste0(col_name,".fdc")], 1.2), c(df[,col_name],NA), log='y', 
       xlab="Probability of Exceedance", ylab=c("Flow (log scale)"), 
       main=c(paste0("Flow Duration Curve: ", " n = ", sum(!is.na(df[,col_name])))), bty = 'l', pch = 16, cex = .6)
  
  curve(fit(x), col='red', lwd=2, lty=1, add=T)
  
  if (!is.null(prob)) {
      abline(v=prob,h = fit(prob),col='grey')
      points(x = prob, y = fit(prob), col = 'navy', cex= 2, pch = 16 )
      text(prob,fit(prob), labels=paste0("P = ",prob*100,"%, Q = ",round(fit(prob),2)), adj=c(-0.05,-1), cex=0.9)
    }
}


