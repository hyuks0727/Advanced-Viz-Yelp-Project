library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(plyr)
library(dplyr)
library(ggmap)

library(stringr)
library(plotly)
library(datasets)
library(grDevices)
library(htmlwidgets)
#state_geo <- rjson::fromJSON(file=file('gz_2010_us_040_00_500k.json'))

json_file<-'yelp_academic_dataset_business.json'

result<-jsonlite::stream_in(file(json_file))
head(result,1)
colnames(result)

result$state<-as.factor(result$state)
std_state=data.frame(state=state.abb,name=state.name)

df=aggregate(stars~state,data=result,FUN=mean)
df=filter(df,state%in%state.abb)
df=join(df,std_state,by='state')


df2=aggregate(review_count~state,data=result,FUN=mean)
df2=filter(df2,state%in%state.abb)
df2=join(df2,std_state,by='state')


hour_compute<-function(period){
  if(is.na(period)){
    return(0)
  }else{
    start=str_split(period,'-')[[1]][1]
    end=str_split(period,'-')[[1]][2]
    start_time=as.numeric(str_split(start,':')[[1]][1])+as.numeric(str_split(start,':')[[1]][2])/60
    end_time=as.numeric(str_split(end,':')[[1]][1])+as.numeric(str_split(end,':')[[1]][2])/60
    if(end_time>start_time){
      return(end_time-start_time)
    }else{
      return(end_time-start_time+24)
    }
  }
}

hour_result <- apply(result$hours,c(1,2),hour_compute)
hour_sum <- apply(hour_result,1,sum)
df3=data.frame(state=result$state,hour_sum=hour_sum)
df3=aggregate(hour_sum~state,data=df3,FUN=mean)
df3=filter(df3,state%in%state.abb)
df3=join(df3,std_state,by='state')


parking_result=result$attributes$BusinessParking
parking_result=!(is.na(parking_result)| parking_result=='None')
df4=data.frame(state=result$state,parking_lot=parking_result)
df4=aggregate(parking_lot~state,data=df4,FUN=mean)
df4=filter(df4,state%in%state.abb)
df4=join(df4,std_state,by='state')

###########
t <- list(
  family = "Courier New",
  size = 14,
  color = "blue")
t1 <- list(
  family = "Times New Roman",
  size=40,
  color = "Black"
)
t2 <- list(
  family = "Courier New",
  size = 14,
  color = "green")
t3 <- list(family = 'Arial')


updatemenus <- list(
  list(
    active = 0,
    type= 'buttons',
    showactive = F,
    buttons = list(
      list(
        label = 'Average Stars',
        method = "update",
        args = list(list(visible = c(T, F, F, F)))),
      list(
        label = 'Average Review Counts',
        method = "update",
        args = list(list(visible = c(F, T, F, F))))
      ,list(
        label = 'Average Working Hours',
        method = "update",
        args = list(list(visible = c(F, F, T, F))))
      ,list(
        label = 'Ratio of Parking Place',
        method = "update",
        args = list(list(visible = c(F, F, F, T))))
    )))

p<-plot_geo() %>%
  #add_trace(z = df$stars, locations = c("x"), colors = 'Blues',
  #          visible=T,showscale=T) %>%
  add_trace(
    z = df$stars, text = df$name, span = I(0), visible=T, showscale=T,
    locations = df$state, locationmode = 'USA-states',
    colors = colorRampPalette(c(rgb(0,0,0.3,1), rgb(0,0.6,0.9,1)))( 30 )
  ) %>%
  add_trace(
    z = df2$review_count, text = df2$name, span = I(0), visible=F, showscale=T,
    locations = df$state, locationmode = 'USA-states',
    colors = 'Blues'
  ) %>%
  add_trace(
    z = df3$hour_sum, text = df3$name, span = I(0), visible=F, showscale=T,
    locations = df$state, locationmode = 'USA-states',
    colors = 'Greens'
  ) %>%
  add_trace(
    z = df4$parking_lot, text = df4$name, span = I(0), visible=F, showscale=T,
    locations = df$state, locationmode = 'USA-states',
    colors = 'Blues'
  ) %>%

  layout(geo = list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    lakecolor = toRGB('white')),
         title = list(text = 'Businesses on Yelp in Different States', font = t1),
    margin=list(l=0,r=0,b=0,t=80),
    updatemenus=updatemenus)

saveWidget(p, "htmls/Map.html", selfcontained = F, libdir = "lib")
