# mapfilter <- reactive({
#   table_1_2 %>%
#     filter(year%in% input$YearInput,
#            source %in% input$SourceInput,
#            gender %in% input$GenderInput,
#            type %in% input$TypeInput,
#            income_source %in% input$IncomeSource)
# })

## test space

mapfilter <- table_1_2 %>%
  filter(year==2012,
         source=="T1 Personal Master File",
         gender == "Both",
         type == "Incoming",
         income_source == "Outside the Jurisdiction")


output$mainmap <- renderPlot({
  
  # mapdat <- simple_pr_shapefile %>%
  #   inner_join(mapfilter(), by='PRUID') %>%
  #   
  #   mutate(mapformat=sf::st_transform(
  #     simple_pr_shapefile$geometry,
  #     crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs "))
  
  
  mapdat <- simple_pr_shapefile %>%
    inner_join(mapfilter,by='PRUID') %>%
    
    mutate(mapformat=sf::st_transform(
      simple_pr_shapefile$geometry,
      crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs "))
  
  ## set color scheme for concept (employees in blue, income in red)
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
  
  
  ggplot() + geom_sf(data=mapdat$mapformat,
                     fill=pal_count_PR$pal(seriesvar),
                     group='count',
                     
                     color='#444444',
                     alpha=0.75) + theme_bw() + 
    
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  
})








output$clickpoint <- reactive({st_as_sf(data.frame(x=input$mapclick$x,
                                                   y=input$mapclick$y),
                                        coords=c('x','y'),crs=4326)})

output$clickprov <- reactive({
  simple_pr_shapefile %>%
    mutate(inprov=apply(X=simple_pr_shapefile,
                        MARGIN=1,
                        FUN=function(x) st_contains(x[3]$geometry,output$clickpoint,sparse=F)[1,1])) %>%
    filter(inprov) %>%
    pull(PRENAME) %>%
    renderText()
})

output$info <- renderText({
  paste0("x=", input$map_click$x, "\ny=", input$map_click$y)
})



# clickpoint <- st_as_sf(data.frame(x=887371.552948097,
#                                   y=-594718.652849642),
#                        coords=c('x','y'),crs=4326)






## test space

mapfilter <- table_1_2 %>%
  filter(year==2012,
         source=="T1 Personal Master File",
         gender == "Both",
         type == "Incoming",
         income_source == "Outside the Jurisdiction")

test_mapframe <- simple_pr_shapefile %>%
  inner_join(mapfilter,by='PRUID')





mapdat <- sf::st_transform(simple_pr_shapefile$geometry,crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs ")
bounds <- st_bbox(mapdat)

canada_map <- ggplot() + geom_sf(data=mapdat,fill='white') + 
  # coord_sf(xlim=c(bounds$xmin,bounds$xmax*1.3),
  #          ylim=c(bounds$ymin*1.3,bounds$ymax)) + 
  theme_bw() +
  
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

################
ggplot() + geom_sf(data=test_mapframe2$mapplotting,
                   fill=pal_count_PR$pal(test_mapframe2$count),
                   group='count',
                   
                   color='#444444',
                   alpha=0.75) + 
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())


################

PRcount %>%
  addPolygons( data = IJE_table1_filted,stroke = TRUE, color = "#444444", weight = 1, smoothFactor = 0.5,
               opacity = 1.0, fillOpacity = 0.5,fillColor = pal_count_PR$pal(seriesvar),group = "count",
               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                   bringToFront = FALSE),
               label = geo_labels_PR,
               labelOptions = labelOptions(
                 style = list(padding = "3px 8px"),
                 textsize = "13px",
                 direction = "auto"),              
               popup =paste( '<B>', IJE_table1_filted$province, '</B>', "<br>",
                             "Number of Employees: ", format(IJE_table1_filted$count, big.mark = ","), "<br>",
                             "Income: ", format(IJE_table1_filted$income, big.mark = "," ), "<br>")
  )%>% 
  # addCircles(lng = IJE_table1_filted$lon, lat  = IJE_table1_filted$lat, weight = 2, radius = sqrt(IJE_table1_filted$income)*5,opacity = 1.0, fillOpacity = 0.5, fillColor = "transparent",
  #            color="red", label = labels_inc, highlightOptions = highlightOptions(color = "white", weight = 3,
  #                                                                                 bringToFront = TRUE), group="Income")%>%
  addLegend(position ="topright", pal = pal_count_PR$pal, values = seriesvar,
            opacity = 1, title = seriestitle, na.label = "No Data")%>%
  # addLayersControl( overlayGroups = c("Income"),
  #                   options = layersControlOptions(collapsed = TRUE, autoZIndex=TRUE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) 

# atl_map <- ggplot() + geom_sf(data=simple_pr_shapefile$geometry[2:4],fill='white') + theme_bw() +
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
#         plot.margin=unit(c(0,0,0,0),"cm"))
# 
# canada_map + 
#   geom_rect(xmin=1200000,xmax=3450000,ymin=-2500000,ymax=-500000) + 
#   annotation_custom(ggplotGrob(atl_map),xmin=1200000,xmax=3450000,ymin=-2500000,ymax=-500000)
