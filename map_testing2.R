## map testing 2 with plotly

library(plotly)
library(tidyverse)

## data
mapfilter <- table_1_2 %>%
  filter(year==2012,
         source=="T1 Personal Master File",
         gender == "Both",
         type == "Incoming",
         income_source == "Outside the Jurisdiction")

mapdat <- simple_pr_shapefile %>%
  inner_join(mapfilter,by='PRUID') %>%
  
  mutate(geometry=sf::st_transform(
    simple_pr_shapefile$geometry,
    crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs "))

if(input$SeriesInput == "Employees"){
  seriesvar <- mapdat$count
  seriestitle <- 'Number of Inter-Jurisdictional Employees'
  
  pal_count_PR <- createClasses(mapdat$count , "Blues", "transparent", 5)
  
  geo_labels_PR <- sprintf(
    "<strong>%s (Employees):  %s </strong>",
    mapdat$province, format(mapdat$count, big.mark = ",")) %>%
    lapply(htmltools::HTML) # add labels 
  
} else if(input$SeriesInput == "Income"){
  
  seriesvar <- mapdat$income
  seriestitle <- 'Income of Inter-Jurisdictional Employees'
  
  pal_count_PR <- createClasses(mapdat$income, "Reds", "transparent", 5)
  
  geo_labels_PR <- sprintf(
    "<strong>%s (Income): %s </strong>",
    mapdat$province, format(mapdat$income, big.mark = ",")) %>%
    lapply(htmltools::HTML) # add labels  
}

## continuous
ggplotly(
  ggplot(mapdat) +
    geom_sf(aes(fill=log(count)/log(10),
                text=sprintf("<b>%s</b><br>Count: %s<br>Income: %s",
                             province,
                             format(count,big.mark=','),
                             paste0('$',format(round(income/1000000,1),big.mark=','),' M'))),
            
            color="#444444",
            alpha=0.75) + theme_bw() +
    
    scale_fill_gradient(low='#f1eef6',
                        high='#045a8d',
                        na.value='grey.50') +
    
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
          
          legend.position='none'),
  
  tooltip='text') %>%
  style(hoveron='all')


# 
# ## discrete
# ggplotly(
#   ggplot(mapdat) +
#     geom_sf(aes(fill=colorbins,
#                 text=sprintf("<b>%s</b><br>Count:%s<br>Income:%s",
#                              province,
#                              format(count,big.mark=','),
#                              paste0(format(round(income/1000000,1),big.mark=','),' M'))),
#             
#             color="#444444",
#             alpha=0.75) + theme_bw() +
#     
#     # scale_fill_gradient(low='#f1eef6',
#     #                     high='#045a8d',
#     #                     na.value='grey.50') +
#     
#     scale_fill_brewer(palette='PuBu') +
#     
#     theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#           axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#           axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
#   
#   tooltip='text') %>%
#   style(hoveron='all')
# 
# 
# 
# 
# 
# 
# 
# ggplotly(
#   ggplot(mapdat) + 
#     geom_sf(aes(fill=colorbins,
#                 text=paste0("province:",province)),
#             
#             color='#444444',
#             alpha=0.75) + theme_bw() +
#     
#     
#     theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#           axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#           axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
#   
#   tooltip=) %>%
#   style(hoveron='all')
  
  