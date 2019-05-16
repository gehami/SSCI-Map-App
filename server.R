#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#CURRENTLY FUCTIONING, BUT ISSUES ON LINE #447 THAT WE JUST SORTA SKIMMED OVER

############ Packages ###########

if(!require(plyr)){
  install.packages("plyr")
  require(plyr)
}
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
}
if(!require(htmltools)) {
  install.packages("htmltools", repos = "https://cloud.r-project.org/")
  require(htmltools)
}
if(!require(devtools)) {
  install.packages("devtools", repos = "https://cloud.r-project.org/")
  require(devtools)
}
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  require(leaflet)
}
if(!require(shiny)) {
  install.packages("shiny", repos = "https://cloud.r-project.org/")
  require(shiny)
}
if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}
if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}
if(!require(DT)){
  install.packages("DT")
  library(DT)
}
if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)
}

if(!require(sf)){
  install.packages("sf")
  require(sf)
}

######### Functions ############


subset_ma = function(ma, school = NULL){
  if(is.null(school)){return(NULL)}
  if(!require(plyr)){
    install.packages("plyr")
    require(plyr)
  }
  ret_dat = ma[tolower(ma$school) %in% tolower(school),]
  ret_dat = ret_dat %>% arrange(date, level) %>% mutate(month = floor_date(date, "month")) %>% arrange(month, -level) %>% 
    mutate(y_month = ave(as.numeric(month), as.numeric(month), FUN = seq_along))
  ret_dat = ret_dat %>% arrange(date, level) %>% mutate(week = floor_date(date, "week")) %>% arrange(week, -level) %>% 
    mutate(y_week = ave(as.numeric(week), as.numeric(week), FUN = seq_along))
  ret_dat = ret_dat %>% arrange(date, -level) %>% mutate(y_day = ave(as.numeric(date), as.numeric(date), FUN = seq_along))
  return(ret_dat)
}

graphify = function(sbst, time_period = "week", 
                    color_choices = c("red", "orange", "green"), num_colors = 1,
                    font_family = "sans-serif", font_size = 18, font_color = "#7f7f7f",
                    marker_size = 20){
  if(!require(grDevices)){
    install.packages("grDevices")
    require(grDevices)
  }
  if(!require(leaflet)){
    install.packages("leaflet")
    require(leaflet)
  }
  if(identical(tolower(substr(time_period, 1, 1)), "d")){
    x_val = sbst$date
    y_val = sbst$y_day
    time_period = "Day"
  }else if(identical(tolower(substr(time_period, 1, 1)), "m")){
    x_val = sbst$month
    y_val = sbst$y_month
    time_period = "Month"
  }else{
    x_val = sbst$week
    y_val = sbst$y_week
    time_period = "Week"
  }
  date_char = as.character(sbst$date)
  date_print = paste0(substr(date_char, 6,7),"/", substr(date_char, 9,10) , "/", substr(date_char, 1, 4))
  names = gsub("[Ll][Vv]*[[:digit:]][[:punct:]]*[[:space:]]*[[:punct:]]*[[:space:]]*", "", gsub("[[:digit:]]{4,}[[:space:]]*[^[:alnum:]]*[[:space:]]*[^[:alnum:]]*", "",sbst$sheet_val))
  names = paste0(names, "<br />", "Level ", sbst$level, "<br />", date_print, "<br />",
                 sbst$school, "<br />")
  clrs = NULL
  if(num_colors == 1 | length(color_choices) < 2){clrs = color_choices}else{
    for(n in 2 : length(color_choices)){
      clrs = c(clrs, colorRampPalette(colors = c(color_choices[n - 1], color_choices[n]), space = "lab")(num_colors))
    }
  }
  mypal = colorNumeric(clrs, domain = sbst$level)
  sbst$color = mypal(sbst$level)
  f <- list(
    family = font_family,
    size = font_size,
    color = font_color
  )
  pt = tryCatch(plot_ly(data = sbst, x = x_val, y = y_val, type = 'scatter', showlegend = F,
               text = names, mode = "markers", hoverinfo = "text",
               marker = list(color = sbst$color, size = marker_size, opacity = 0.5)) %>%
    layout(yaxis = list(title = paste0("Incidents in the ", time_period), titlefont = f),
           xaxis = list(title = "Date", titlefont = f)), 
    error = function(e) NULL)
  # pt$x= NULL
  # 
  return(pt)
}



# library(leaflet)
# 
# m <- leaflet() %>% addTiles() %>%
#   addMarkers(c(-122.327298, -122.327444), c(47.597131, 47.597444), layerId = c("my layer id", "other layer id")
#   )
# 
# grep("my", m$x$calls[[2]]$args[[4]])

###### Prep Work #########

# setwd("C:/Users/Albert.Gehami/Desktop/Crime-Mapping")
sci_plot = readRDS("ssci_for_plotting.rds")
sci_plot = sci_plot[-grep("S\\.D\\.", sci_plot$school),]
# sci = sci_plot
full_forms = readRDS("SSCI_IR_ma_16-18_for_plotting_with_latlon_and_council_dist.rds")

# cd_bounds = rgdal::readOGR(dsn = "C:/Users/albert.gehami/Desktop/Crime-Mapping", layer = "Council Districts Map")
# ff_spdf = SpatialPointsDataFrame(coords = full_forms[,c("lon", "lat")], data = full_forms, 
#                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# cd_bounds = spTransform(cd_bounds, ff_spdf@proj4string)
# 
# #determining which district each school lies in.
# full_forms$council_dist = sp::over(ff_spdf, cd_bounds)$DISTRICT

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px; margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
}

# top_10 = sci[order(-sci$total), c("school", "total")]
# colnames(top_10) = c("School", "Incidents")






rc1 <- colorRampPalette(colors = c("red", "yellow"), space = "Lab")(100)

## Make vector of colors for values larger than 0 (180 colors)
rc2 <- colorRampPalette(colors = c("yellow", "green"), space = "Lab")(100)

## Combine the two color palettes
rampcols <- c(rc1, rc2)

mypal <- colorNumeric(palette = rampcols, domain = sci_plot$point_color)


map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  # addWMSTiles(
  #   "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
  #   layers = c("1-degree grid", "5-degree grid"),
  #   options = WMSTileOptions(format = "image/png8", transparent = TRUE),
  #   attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.28, zoom = 12) #%>%



school_bounds = readRDS("school_bounds_for_16-18_IRs.rds")

#exporting all of the above data for the auditors
# write.csv(full_forms, 'All SSCI IRs for 2016-2018 in tabular form.csv')
# write.csv(sci_plot, 'SSCI Incident Counts by School.csv')
# 
# library(maptools)
# library(rgdal)
# writeOGR(obj = school_bounds, dsn = "School Bundary Files", layer = 'school_boundaries', driver = 'ESRI Shapefile')

####### Running the server #############


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
sci_dat = reactive({
    date_range = c(input$timeframe[1]:input$timeframe[2])
    dat = full_forms[full_forms$date %in% date_range,]
    dat_collapse_info = dat %>% arrange(school) %>% 
      plyr::mutate(total = rep(plyr::count(dat$school)[,2], times = plyr::count(dat$school)[,2])) %>%
      plyr::mutate(lvl1 = rep(as.numeric(table(factor(dat$school)[dat$level == 1])), times = plyr::count(dat$school)[,2])) %>%
      plyr::mutate(lvl2 = rep(as.numeric(table(factor(dat$school)[dat$level == 2])), times = plyr::count(dat$school)[,2])) %>%
      plyr::mutate(lvl3 = rep(as.numeric(table(factor(dat$school)[dat$level == 3])), times = plyr::count(dat$school)[,2]))
    sci_fill = dat_collapse_info[!duplicated(dat_collapse_info$school),c("OBJECTID", "lvl1", "lvl2", "lvl3", "total", "lat", "lon")]
    colnames(sci_fill) = c("school", "lvl1", "lvl2", "lvl3", "total", "lat", "lon")
    # sci_fill = dat_collapse
    # sci_fill = sci_fill[,-which(colnames(sci_fill) == "address")]
    # sci_fill = dat_collapse    
    for(n in 1 : nrow(sci_fill)){
      sci_fill$point_color[n] = mean(c(rep.int(1, sci_fill$lvl1[n]), rep.int(2, sci_fill$lvl2[n]), rep.int(3, sci_fill$lvl3[n])))
    }
    
    sci_fill$point_color[sci_fill$total < 1] = NA
    sci_fill$color_1_2_3 = sci_fill$point_color
    sci_fill$color_1_2 = NA
    sci_fill$color_2_3 = NA
    sci_fill$color_1_3 = NA
    
    for(n in 1 : nrow(sci_fill)){
      sci_fill$color_1_2[n] = mean(c(rep.int(1, sci_fill$lvl1[n]), rep.int(2, sci_fill$lvl2[n])), na.rm = T)
    }
    sci_fill$color_1_2[is.nan(sci_fill$color_1_2)] = NA
    
    for(n in 1 : nrow(sci_fill)){
      sci_fill$color_2_3[n] = mean(c(rep.int(2, sci_fill$lvl2[n]), rep.int(3, sci_fill$lvl3[n])), na.rm = T)
    }
    sci_fill$color_2_3[is.nan(sci_fill$color_2_3)] = NA
    
    for(n in 1 : nrow(sci_fill)){
      sci_fill$color_1_3[n] = mean(c(rep.int(1, sci_fill$lvl1[n]), rep.int(3, sci_fill$lvl3[n])), na.rm = T)
    }
    sci_fill$color_1_3[is.nan(sci_fill$color_1_3)] = NA
    
    sci_fill$color_1 = 1
    sci_fill$color_1[sci_fill$lvl1 < 1] = NA
    
    sci_fill$color_2 = 2
    sci_fill$color_2[sci_fill$lvl2 < 1] = NA
    
    sci_fill$color_3 = 3
    sci_fill$color_3[sci_fill$lvl3 < 1] = NA
    
    #shout out to Andrie on Stack Overflow for this nifty function
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    range02 <- function(x, y){(x-min(x))/(max(y)-min(x))}
    
    
    
    sci_fill$scale_total = range01(sci_fill$total)
    sci_fill$scale_1_2_3 = sci_fill$scale_total
    sci_fill$scale_1_2 = range02(rowSums(cbind(sci_fill$lvl1, sci_fill$lvl2)), sci_fill$total)
    sci_fill$scale_1_3 = range02(rowSums(cbind(sci_fill$lvl1, sci_fill$lvl3)), sci_fill$total)
    sci_fill$scale_2_3 = range02(rowSums(cbind(sci_fill$lvl3, sci_fill$lvl2)), sci_fill$total)
    sci_fill$scale_1 = range02(sci_fill$lvl1, sci_fill$total)
    sci_fill$scale_2 = range02(sci_fill$lvl2, sci_fill$total)
    sci_fill$scale_3 = range02(sci_fill$lvl3, sci_fill$total)
    colnames(sci_fill)[1] = c("school")
    return(sci_fill)
  })
  
label_det = reactive({
    sci_fill = sci_dat()
    labs <- lapply(seq(nrow(sci_fill)), function(i) {
      paste0( '<p>', 'School: ', sci_fill[i, "school"], '<p></p>', 
              'Total Incidents: ',sci_fill[i, "total"], '</p><p>', 
              'Level One: ', sci_fill[i, "lvl1"],'</p><p>', 
              'Level Two: ', sci_fill[i, "lvl2"], '</p>',
              'Level Three: ', sci_fill[i, "lvl3"], '</p>') 
    })
    return(as.character(labs))
  })

   
display_data = reactive({

    school = input$map_marker_click$id
    
    return(school)
  })
  
level_vals = reactive({
    if(!require(raster)){
      install.packages("raster")
      require(raster)
    }
    lvls = input$Levels
    lvl_cols = sci_dat()[,grep(paste0("lvl", lvls, collapse = "|"), colnames(sci_dat()))]
    if(length(dim(lvl_cols)) > 1){
      return(raster::rowSums(lvl_cols, na.rm = T))
    }else{
      return(lvl_cols)
    }
  })
level_scale = reactive({
    lvls = input$Levels
    scale_col = sci_dat()[,grep(paste0("^scale_", paste0(lvls, collapse = "_"), "$"), colnames(sci_dat()))]
    return(scale_col)
  })
level_color = reactive({
    lvls = input$Levels
    color_col = sci_dat()[,grep(paste0("^color_", paste0(lvls, collapse = "_"), "$"), colnames(sci_dat()))]
    return(color_col)
  })
  
output$map = renderLeaflet({

    clearMarkers(map)
    clearControls(map)
    map %>% addCircleMarkers(
      lng = sci_dat()$lon,
      lat = sci_dat()$lat,
      weight = 0.5,
      fillColor = mypal(level_color()),
      radius = (level_scale() * 30 + 7),
      fillOpacity = 0.8,
      stroke = T,
      layerId = sci_dat()$school,
      label = lapply(label_det(), HTML),
      group = "Points") %>%
      addLegend(position = "topleft", pal = mypal, values = sci_dat()$color_1_2_3,
                title = "<p>Average Severity<p></p> of Incidents</p>",
                opacity = 0.9) %>%
      addLegendCustom("grey", labels = seq(0, max((level_vals()), na.rm = T), by = 7), sizes = (seq(0, max(level_vals(), na.rm = T), by = 7) + 7),
                      title = "Number of incidents")
  })

  
schools_selected = reactiveVal()  

observeEvent(input$map_marker_click,{
  inpt = input$map_marker_click$id
  if(grepl("highlighted_point", inpt)){
    remove_row = gsub("highlighted_point", "", inpt)
    new_select = schools_selected()[-which(schools_selected() %in% remove_row)]
  }else{
    new_select = c(schools_selected(), inpt)
  }
  schools_selected(new_select)
})
observeEvent(input$Top_10_rows_selected,{
  inpt = sub_sci()$school[input$Top_10_rows_selected]
  if(inpt %in% schools_selected()){
    new_select = schools_selected()[-which(schools_selected() %in% inpt)]
  }else{
    new_select = c(schools_selected(), inpt)
  }
  schools_selected(new_select)
})
observeEvent(input$cd, ignoreNULL = F,{
  if(is.null(cds_schools_selected())){
    cds_schools_selected(input$cd)
    add_schools = unique(full_forms$school[full_forms$council_dist %in% input$cd])
    new_select = unique(c(schools_selected(), add_schools))
    schools_selected(unique(new_select))
    cds_schools_selected(add_schools)
  }else{
    add_schools = unique(full_forms$school[full_forms$council_dist %in% input$cd])
    remove_schools = cds_schools_selected()[which(!(cds_schools_selected() %in% add_schools))]
    # print(remove_schools)
    # remove_schools = full_forms$school[full_forms$council_dist %in% remove_dists]
    new_select = unique(c(schools_selected()[which(!(schools_selected() %in% remove_schools))], full_forms$school[full_forms$council_dist %in% input$cd]))
    schools_selected(unique(new_select))
    cds_schools_selected(add_schools)
    # print(remove_schools)
    
  }
  # print(new_select)
  # print(cds_schools_selected())
})
observeEvent(c(input$map_marker_click, input$Top_10_rows_selected, input$cd),ignoreNULL = F,{
  if(is.null(schools_selected())){return(NA)}else{
    proxy = leafletProxy("map")
    remove_schools = NULL
    if(!is.null(prev_schools())){
      remove_schools = prev_schools()[which(!(prev_schools() %in% schools_selected()))]
    }
    # remove_schools = current_schools[which(!(current_schools %in% schools_selected()))]
    add_schools = schools_selected()[which(!(schools_selected() %in% prev_schools()))]
    if(!is.null(remove_schools)){
      leafletProxy('map') %>% removeMarker(paste0(remove_schools, "highlighted_point")) %>% removeShape(paste0(remove_schools, "boundary"))
    }
    if(!is.null(add_schools)){
      add_inds = which(sci_dat()$school %in% add_schools)
      
      proxy = leafletProxy('map')
      proxy %>% addCircleMarkers(
        lat = sci_dat()$lat[add_inds],
        lng = sci_dat()$lon[add_inds],
        layerId = paste0(add_schools, "highlighted_point"),
        radius = (level_scale()[add_inds] * 30 + 7),
        fillOpacity = 0.9,
        # label = lapply(grep(paste0("School: ",add_schools,"<", collapse = "|"), label_det(), value = T), HTML), #struggling to get this to work. 
        stroke = T,
        fillColor = "blue")
      # print(unlist(strsplit(gsub("(>)( )", "\\1~", HTML(grep(paste0("School: ",add_schools,"<", collapse = "|"), unlist(label_det()), value = T))), "~")))
    }
  }
  current_schools = schools_selected()
  prev_schools(current_schools)
  # print(current_schools)
})

prev_schools = reactiveVal()


map_schools_selected = reactiveVal()
dt_schools_selected = reactiveVal()
cds_schools_selected = reactiveVal()

output$text <- renderUI({
  HTML(paste0(c("Showing IRs for:", schools_selected()), collapse = "<br />"))
})

det_levels = reactive({
  inpt = input$Levels
  if(identical(inpt, c("1"))){
    return(5)
  }else if(identical(inpt, c("2"))){
    return(6)
  }else if(identical(inpt, c("3"))){
    return(7)
  }else if(identical(inpt, c("1", "2"))){
    return(2)
  }else if(identical(inpt, c("2", "3"))){
    return(4)
  }else if(identical(inpt, c("1", "3"))){
    return(3)
  }else{
    return(1)
  }
})
 
sub_sci = reactive({
  lvls = paste0("lvl",input$Levels)
  lvl_list = input$Levels
  if(length(lvl_list) < 1){
    lvls = c("lvl1", "lvl2", "lvl3")
    lvl_list = c("1", "2", "3")
    # top_10 = sci_dat()[]
  }
  lvls = lvls[order(lvls)]
  if(length(lvls) < 2){
    inds = order(-sci_dat()[,lvls])
    top_10 = sci_dat()[inds, c("school", "total", "lat", "lon", "point_color",
                         "color_1_2", "color_2_3", "color_1_3", "color_1", "color_2", "color_3", "scale_total", 
                         "scale_1_2", "scale_1_3", "scale_2_3", "scale_1", "scale_2", "scale_3")]
    top_10$total = sci_dat()[inds,lvls]
  }else{
    inds = order(-rowSums(sci_dat()[,lvls], na.rm = T))
    top_10 = sci_dat()[inds, c("school", "total", "lat", "lon", "point_color",
                         "color_1_2", "color_2_3", "color_1_3", "color_1", "color_2", "color_3", "scale_total", 
                         "scale_1_2", "scale_1_3", "scale_2_3", "scale_1", "scale_2", "scale_3")]
    top_10$total = rowSums(sci_dat()[,lvls], na.rm = T)[inds]
  }
  if(length(lvls) == 1){
    lvl_name = paste0("Level ", lvl_list, " Incidents")
  }else if(length(lvls) == 2){
    lvl_name = paste0("Level ", lvl_list[1], " & ", lvl_list[2], " Incidents")
  }else{
    lvl_name = "Total Incidents"
  }
  colnames(top_10) = c("school", lvl_name, "lat", "lon", "point_color",
                       "color_1_2", "color_2_3", "color_1_3", "color_1", "color_2", "color_3", "scale_total", 
                       "scale_1_2", "scale_1_3", "scale_2_3", "scale_1", "scale_2", "scale_3")
  rownames(top_10) = NULL
  return(top_10)
})  

# output$Top_10 = renderTable(sub_sci(), digits = 0)
  
output$Top_10 <- renderDataTable(DT::datatable(data.frame(School = sub_sci()[,1], 
                                                          Incidents = sub_sci()[,2]),
                                               rownames = F, selection = "single", options=list(stateSave = T)))

prev_row <- reactiveVal()


full_forms_in_date = reactive(
  return(full_forms[full_forms$date %in% c(c(input$timeframe[1]:input$timeframe[2])),])
)

observeEvent(input$timeframe,{schools_selected(NULL)})

output$plot = tryCatch(renderPlotly(graphify(subset_ma(full_forms_in_date(), school = schools_selected()), time_period = input$time_frame)),
                       error = function(e) NULL,
                       warning = function(w) NULL)


output$graph_text = renderUI(htmltools::HTML(paste0("Showing ", input$time_frame, " IRs for:<br />",
                                paste0(schools_selected(), collapse = "<br />"))))



})
