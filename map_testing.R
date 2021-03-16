mapdat <- sf::st_transform(simple_pr_shapefile$geometry,crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs ")
bounds <- st_bbox(mapdat)

canada_map <- ggplot() + geom_sf(data=mapdat,fill='white') + 
  coord_sf(xlim=c(bounds$xmin,bounds$xmax*1.3),
           ylim=c(bounds$ymin*1.3,bounds$ymax)) + theme_bw() +
  
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

atl_map <- ggplot() + geom_sf(data=simple_pr_shapefile$geometry[2:4],fill='white') + theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm"))

canada_map + 
  geom_rect(xmin=1200000,xmax=3450000,ymin=-2500000,ymax=-500000) + 
  annotation_custom(ggplotGrob(atl_map),xmin=1200000,xmax=3450000,ymin=-2500000,ymax=-500000)
