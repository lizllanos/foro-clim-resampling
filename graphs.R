# Graficos remuestreo -----------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(plotly)

tll = theme(panel.grid.major = element_blank(),panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line    = element_line(color='lightgrey'),
            axis.ticks =element_line(color='grey'),
            axis.text.x = element_text( vjust = 0),
            text = element_text(size=15))


to_monthly <- function(data, ...){
  data %>% 
    group_by(year, month) %>%
    summarise(prec = sum(prec, ...), 
              tmin = mean(tmin, ...), 
              tmax = mean(tmax, ...) 
              #            srad = mean(srad), 
              #            rhum = mean(rhum),
              #            wvel = mean(wvel)
    ) %>% #write.csv("climate_data_monthly.csv")
    ungroup() 
}

quarterly_data <- function(data_ini,trim_ini){
  #data_ini=data_historic;trim_ini=pronostico[1,1]
  data = to_monthly(data_ini)[,1:3]
  season = factor(c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ'),
                  c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ'))
  ini_m = c(12,1:11)
  
  pos_trim = which(season %in% trim_ini)
  sy_month = ini_m[pos_trim]             
  names_months=0
  data_out=list()
  year_out=list()
  l=(sy_month)+(0:7)
  if(sum(l>12)>0)l[which(l>12)]=l[which(l>12)]-12
  if(sum(l<1)>0)l[which(l<1)]=l[which(l<1)]+12
  
  i=1
  
  pos_ini=which(data$month==l[i])
  pos_data=sort(c(pos_ini,pos_ini+1,pos_ini+2))
  data_out[[i]]=data.frame(na.omit(aggregate(data[pos_data,-1:-2],by=list(sort(rep(1:(length(pos_ini)),3))),sum))[,-1])
  names(data_out[[i]])=colnames(data)[-1:-2]
  year_out[[i]] =as.data.frame(data[pos_ini+1,1][1:dim(data_out[[i]])[1],])
  data_out[[i]]$season=season[pos_trim]
  
  i=4
  
  pos_ini=which(data$month==l[i])
  pos_data=sort(c(pos_ini,pos_ini+1,pos_ini+2))
  data_out[[2]]=data.frame(na.omit(aggregate(data[pos_data,-1:-2],by=list(sort(rep(1:(length(pos_ini)),3))),sum))[,-1])
  names(data_out[[2]])=colnames(data)[-1:-2]
  year_out[[2]] =as.data.frame(data[pos_ini+1,1][1:dim(data_out[[2]])[1],])
  data_out[[2]]$season=season[pos_trim+3]
  
  
  all_output=do.call("rbind",data_out)
  all_output$year= do.call("rbind",year_out)[,1]
  
  return(all_output)
}

plot_prob <- function(pronostico, id_label = NULL){
  
  pronostico= pronostico %>% mutate(Type = factor(Type, c("above", "normal", "below")),
                                    Season = factor(Season, c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ'))) 
  
  ggplotly(ggplot(pronostico,aes(x = Season, y = Prob, fill = Type)) + 
             geom_col(position = "dodge", color="gray") +
             tll +
             scale_fill_manual(values = c(above = "blue", normal = "lightgreen", below = "red")) +
             labs(title = "Prediccion Climática Estacional", 
                  
                  x = "Trimestre",
                  y = "Probabilidad (%)"))
  
  
}

plot_clima_hist <- function(data_historic, id_label = NULL){
  
  #Set Names and labels  
  var_name = c("rain", "prec", "srad", "tmin", "tmax", "rhum", "wvel")
  var_label = paste(var_name, c('(mm)', '(mm)', '(MJ/m2d)', '(C)', '(C)', '(%)', '(m/s)'))
  names(var_label) <- var_name
  
  #Convert historic data to monthly data  
  monthly_data <- to_monthly(data_historic, na.rm = T) 
  
  
  data_plot = monthly_data %>%
    dplyr::select(-c(year)) %>%
    pivot_longer(cols = -c(month), names_to = "var", values_to = "value") 
  
  
  ggplotly(ggplot(data_plot,aes(month, value, fill= var, group = month)) +
             geom_boxplot() +
             scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
             facet_grid(var ~ ., scales = "free", labeller = labeller(var = var_label)) +
             theme_bw()  +
             theme(legend.position='none',
                   panel.grid.minor = element_blank(),
                   strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
                   strip.text = element_text(face = "bold")) +
             labs(title = "Climatología Histórica",
                  subtitle = id_label,
                  x = " ",
                  y =  NULL) +tll+
             scale_fill_manual(values = c(prec = "#619CFF", tmax = "orange1", tmin = "gold2"))+
             scale_color_manual(values = c(prec = "#619CFF", tmax = "orange1", tmin = "gold2")))
  
}

plot_clima_trim <- function(data_historic, resampling_y,trim_ini,id_label = NULL){
  
  data_trim = quarterly_data(data_ini=data_historic,trim_ini=pronostico[1,1])
  data_trim$Clase = "Histórico"
  
  
  data_plot_clim = data_trim %>%
    dplyr::select(-c(year)) %>%
    pivot_longer(cols = -c(season,Clase), names_to = "var", values_to = "value") 
  
  
   resampling_y$condtion = factor(resampling_y$condtion, c("above", "normal", "below"))
  resampling_y$Clase = "Escenarios remuestreo"
  
  data_plot_res = resampling_y %>%
    dplyr::select(-c(year,id,order,condtion)) %>%
    pivot_longer(cols = -c(season,Clase), names_to = "var", values_to = "value") 
  
  all_to_plot = bind_rows(data_plot_clim,data_plot_res)
  all_to_plot$season = factor(all_to_plot$season,
                               c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ'))
  
  all_to_plot$Clase = factor(all_to_plot$Clase, levels = c("Histórico","Escenarios remuestreo"))
  
  p = ggplot(all_to_plot,aes(season, value, fill= Clase)) +
             geom_boxplot() +
             #scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
             
             theme_bw()  +
             theme(legend.position='bottom',
                   panel.grid.minor = element_blank(),
                   strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
                   strip.text = element_text(face = "bold")) +
             labs(title = "Comparación trimestral Histórico vs Escenarios",
                  
                  x = "Trimestre",
                  y =  NULL) +tll
  
  ggplotly(p)%>%layout(boxmode = "group")
  }

plot_years <- function(resampling_y){
  ggplot (resampling_y, aes(as.character(year)))+ geom_bar()+facet_grid(~season)
  
  
 y_1 = resampling_y %>% filter(season==unique(resampling_y$season)[1]) %>% 
    count(season, year) %>%
    group_by(season) %>%          # now required with changes to dplyr::count()
    mutate(prop = prop.table(n)) %>% arrange(desc(n)) %>% slice(1:10)
  
y_2 =  resampling_y %>% filter(season==unique(resampling_y$season)[2]) %>% 
    count(season, year) %>%
    group_by(season) %>%          # now required with changes to dplyr::count()
    mutate(prop = prop.table(n)) %>% arrange(desc(n))%>% slice(1:10) 
  
 
a =ggplot(y_1, aes(x=reorder(as.character(year), -n), y=n))+
  geom_bar(stat='identity',alpha=0.5)+tll +xlab("")+ylab("Frecuencia")+
  facet_grid(~season)+coord_flip()
  


 b=   ggplot(y_2, aes(x=reorder(as.character(year), -n), y=n))+
    geom_bar(stat='identity',alpha=0.5)+tll +xlab("")+ylab("Frecuencia ")+
   facet_grid(~season)+coord_flip()
 grid.arrange(a, b, nrow = 1, top="Top 10 de los años más frecuentes ")
    
    }


