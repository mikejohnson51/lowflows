#' @title Summarize model vs observe flows
#' @description This function sumarizes a data frame produced by grabData or one that has a modeled and observered timeseries and a column of seasons
#' @param df a data.frame with a minumium of thress columns eg modeled, observed, and season
#' @param mod.name column name of modeled data
#' @param obs.name column name of observed data
#' @return a vector of statistics
#' @export 
#' @author Mike Johnson

get_summary = function(df, mod.name = 'nwm', obs.name = 'nwis'){
  
  get_stat = function(mod, sim){
    
    if(all(length(mod) > 0, length(sim) > 0, length(mod) == length(sim))){
      
      return(c(
        round(hydroGOF::NSE(mod, sim),   2),
        round(hydroGOF::pbias(mod, sim), 2),
        round(hydroGOF::br2(mod, sim),   2),
        round(hydroGOF::rmse(mod, sim),  2)
      ))
    } else { return(rep(NA, 4))}
  }
  
  return(c(
    get_stat(df[,mod.name], df[,obs.name]),
    get_stat(df[df$season == 'fall', mod.name],   df[df$season == 'fall',   obs.name]),
    get_stat(df[df$season == 'winter', mod.name], df[df$season == 'winter', obs.name]),
    get_stat(df[df$season == 'spring', mod.name], df[df$season == 'spring', obs.name]),
    get_stat(df[df$season == 'summer', mod.name], df[df$season == 'summer', obs.name])
  ))
}

hydroGOF::NSE