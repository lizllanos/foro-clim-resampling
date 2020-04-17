# Script to download daily data for prec (PRECTOT, T2M_MIN, T2M_MAX,ALLSKY_SFC_SW_DWN ) for a singlepoint
#  Link to download NASA Power  https://power.larc.nasa.gov/docs/v1/ 

# Created by: Diego Fernando Agudelo (d.agudelo@cgiar.org) and Lizeth Llanos (l.llanos@cgiar.org)
# Date: April 2020


## Packages
suppressMessages(if(!require(rjson)){install.packages('rjson'); library(rjson)} else {library(rjson)})
suppressMessages(if(!require(stringr)){install.packages('stringr'); library(stringr)} else {library(stringr)})

## functions
download_nasa <- function(dir,start,end,lat,lon){
  
  url=paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=T2M_MIN,T2M_MAX,PRECTOT,ALLSKY_SFC_SW_DWN&startDate=",start,"&endDate=",end,"&userCommunity=SSE&tempAverage=DAILY&outputList=CSV&lat=",lat,"&lon=",lon)
  
  temp <- tempfile()
  download.file(url,temp)
  json_data = fromJSON(file=temp)
  
  data_raw=json_data$features[[1]]$properties$parameter %>% lapply(.,unlist) %>% lapply(.,cbind) %>% do.call("cbind",.) 
   
  years=as.numeric(substr(rownames(data_raw),1,4))
  months=as.numeric(substr(rownames(data_raw),5,6))
  days=as.numeric(substr(rownames(data_raw),7,8))
  
  data_final=data.frame(day=days,month=months,year=years,precip=data_raw[,2],tmax=data_raw[,3],tmin=data_raw[,4])#,srad=data_raw[,1])

  if(sum(data_final$precip==-999)>0){
    pos=which(data_final$precip==-999)
    data_final$precip[pos]= round(mean(data_final$precip[-pos]),2)
  }
  if(sum(data_final$tmax==-999)>0){
    pos=which(data_final$tmax==-999)
    data_final$tmax[pos]= round(mean(data_final$tmax[-pos]),2)
  }
  if(sum(data_final$tmin==-999)>0){
    pos=which(data_final$tmin==-999)
    data_final$tmin[pos]= round(mean(data_final$tmin[-pos]),2)
  }
  # if(sum(data_final$srad==-999)>0){
  #   pos=which(data_final$srad==-999)
  #   data_final$srad[pos]= round(mean(data_final$srad[-pos]),2)
  # }
  
  write.csv(data_final,paste0(main_dir,"data_nasa.csv"),row.names=F)
  return(cat("Succesfully donwloaded data"))
}

if(isTRUE(nasa_power) & !file.exists(paste0(directorio_datos,"data_nasa.csv"))){
  main_dir  <- directorio_datos                   ### Directorio de trabajo 
  start_date<- "19830701"  ### Fecha inicial de descarga(año-mes-día)
  end_date  <- str_replace_all(Sys.Date()-31,pattern = "-","")### Fecha final de descarga(año-mes-día)
}
  ## usage

# main_dir  <- "D:/resampling"                    ### Directorio de trabajo 
# start_date<- "19830701"                         ### Fecha inicial de descarga(año-mes-día)
# end_date  <- str_replace_all(Sys.Date()-31,pattern = "-","")      ### Fecha final de descarga(año-mes-día)
# latitud   <- 4.6                                ### Latitud del sitio de interés(en grados decimales)
# longitud  <- -76                                ### Longitud del sitio de interés(en grados decimales)


