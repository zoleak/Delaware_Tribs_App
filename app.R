### App for Delaware Tribs Project ###
### Author: Kevin Zolea ###
###############################################################################
### Download necessary packages ###
### uncomment if packages aren't installed already###
#if (!require(pacman)) {
#  install.packages('pacman')
#  
#}
#
#pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
#               "readr","cowplot","lubridate","scales","shinythemes","plotly",
#               "stringr","data.table","rlang","purrr","rmapshaper",
#               "shiny","DT","leaflet","sf","shinyWidgets",
#               "rsconnect","shinyjs","htmltools","htmlwidgets","leaflet.extras","readr")
###############################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(readxl)
library(shinycssloaders)
library(readr)
library(cowplot)
library(lubridate)
library(scales)
library(shinythemes)
library(plotly)
library(stringr)
library(data.table)
library(rlang)
library(purrr)
library(rmapshaper)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(sf)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(readr)
### Read in data ###
del_trib_data<-read_csv("results.csv",col_names = T)%>%
  dplyr::filter(!val == "NA")
###############################################################################
### Read in flow stations shapefile ###
#USGS_flow_stations<-st_read("shapefiles/USGS_Flow_Stations_KMZ.shp")
###############################################################################
### Read in 2018 water quality stations excel spreadsheet ###
wq_stations<-read_xlsx("Stations_Lookup_051319.xlsx",col_names = T)
### Create vector of all the stations in the del_trib_data dataframe to make a filter for the stations we want from the stations lookup spreadsheet###
stations_wanted<-unique(del_trib_data$locid)
### Filter 2018 wq stations spreadsheet for only the stations in del_trib project ###
del_trib_wq_stations<-wq_stations%>%
  dplyr::filter(locid %in% stations_wanted)%>%
  dplyr::filter(!locid == "NJDEP_AMERICORPS-WAPARK1")
### Create new column with Trib nambes based on HUC number (Doing this to make locids reactive on map) ###
del_trib_wq_stations<-del_trib_wq_stations%>%
  mutate(Trib=case_when(str_detect(HUC14,".*000[1-6]0") ~ "Pennsauken", #Search for 000 followed by a digit from 1-6 followed by 0
                        str_detect(HUC14,".*2021500[1-7]0" ) ~ "Raccoon",
                        str_detect(HUC14,".*2020800[1-9]0" ) ~ "Rancocas",
                        str_detect(HUC14,".*2020600[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020400[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020200[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020300[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020500[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020700[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2010400[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010700[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010600[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010800[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010500[1-9]0")~ "BlacksCrosswicks",
                        TRUE ~ HUC14)) 
###############################################################################
### Read in HUC14s of NJ shapefile ###
NJ_huc14s<-st_read("shapefiles/2014_NJ_Integrated_Report_AU.shp")
###############################################################################
### Read in water shapefile ###
trib_waters<-st_read("shapefiles/del_trib_waters.shp")
###############################################################################
### Read in each trib shapefile ###
pennsuaken<-st_read("shapefiles/penn_hucs.shp")
rancocas<-st_read("shapefiles/RancocasHUCsfinal.shp")
raccoon<-st_read("shapefiles/Raccoon.shp")
blackscrosswicks<-st_read("shapefiles/BlacksCrosswicksHUCsfinal.shp")
tribs_merged<-st_read("shapefiles/tribs_merge.shp")
### Do same thing as above to get trib name column for tribs_merged shapefile to be able to zoom to trib based on drop down for plots ###
tribs_merged<-tribs_merged%>%
  dplyr::mutate(HUC14 = as.character(HUC14))%>%
  dplyr::mutate(Trib=case_when(str_detect(HUC14,".*000[1-6]0") ~ "Pennsauken", #Search for 000 followed by a digit from 1-6 followed by 0
                        str_detect(HUC14,".*2021500[1-7]0" ) ~ "Raccoon",
                        str_detect(HUC14,".*2020800[1-9]0" ) ~ "Rancocas",
                        str_detect(HUC14,".*2020600[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020400[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020200[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020300[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020500[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2020700[1-9]0")~ "Rancocas",
                        str_detect(HUC14,".*2010400[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010700[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010600[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010800[1-9]0")~ "BlacksCrosswicks",
                        str_detect(HUC14,".*2010500[1-9]0")~ "BlacksCrosswicks",
                        TRUE ~ HUC14)) 
###############################################################################
### Change projections to work with leaflet map ###
#USGS_flow_stations<-st_transform(USGS_flow_stations, crs="+init=epsg:4326")
NJ_huc14s<-st_transform(NJ_huc14s, crs="+init=epsg:4326")
NJ_huc14s <- st_zm(NJ_huc14s, drop = T, what = "ZM")
pennsuaken<-st_transform(pennsuaken, crs="+init=epsg:4326")
rancocas<-st_transform(rancocas, crs="+init=epsg:4326")
raccoon<-st_transform(raccoon, crs="+init=epsg:4326")
blackscrosswicks<-st_transform(blackscrosswicks, crs="+init=epsg:4326")
trib_waters<-st_transform(trib_waters,crs = "+init=epsg:4326")%>%
  ms_simplify(.) #### ms_simplify makes shapefile read in faster when app is launched ###
names(st_geometry(trib_waters)) = NULL### Needed to get polygons on map because ms_simplify gives names to geometry; which gives error ###
tribs_merged<-st_transform(tribs_merged, crs="+init=epsg:4326")
tribs_merged<-st_zm(tribs_merged, drop = T, what = "ZM")%>%
  ms_simplify(.) #### ms_simplify makes shapefile read in faster when app is launched ###
  names(st_geometry(tribs_merged)) = NULL### Needed to get polygons on map because ms_simplify gives names to geometry; which gives error ###
tribs_merged<-st_transform(tribs_merged, crs="+init=epsg:4326")%>%
  ms_simplify(.)
###############################################################################
### Define UI for application ###
ui <- navbarPage(theme = shinytheme("yeti"),
                 tags$b("NJDEP Delaware Tribs Project"),
                 ###### Here : insert shinydashboard dependencies ######
                 header = tagList(
                   useShinydashboard()
                 ),
                 #######################################################
                 tabPanel("About App",
                          div(class= "outer",
                              tags$head(
                                #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
                                includeCSS("www/styles.css")),
                              h1(""),
                              h2("Introduction:"),
                              h3("The goal of this app is to aid in decision making for the Delaware Tributaries Project's 
                                 modeling efforts. It will enable users to comprehend the current data we have and the 
                                 monitoring stations from which the data is collected. This, in turn, will provide useful 
                                 information for the project's modelers. The models developed in this project will provide 
                                 useful loading information from upstream nontidal portions of the waterbodies into the 
                                 tidal portions, as well as the ability to develop site-specific criteria if necessary."),
                              br(),
                              HTML('<center><img src="http://media.nj.com/centraljersey_impact/photo/9558763-large.jpg"></center>')),
                          tags$head(tags$script(HTML('
                                                     var customHref = function(tabName) {
                                                     var dropdownList = document.getElementsByTagName("a");
                                                     for (var i = 0; i < dropdownList.length; i++) {
                                                     var link = dropdownList[i];
                                                     if(link.getAttribute("data-value") == tabName) {
                                                     link.click();
                                                     };
                                                     }
                                                     };
                                                     ')))),
                 tabPanel("Data",
                          infoBox("Total # of Monitoring Stations:",length(unique(del_trib_data$locid)),
                                  color = "light-blue",icon = icon("map-marker"),
                                  fill = TRUE),
                          infoBox("Total # of Parameters:",length(unique(del_trib_data$parameter)),
                                  color = "light-blue",icon = icon("water"),
                                  fill = TRUE),
                          infoBox("Study Year Range:", "2008-2018",
                                  color = "light-blue",icon = icon("calendar"),
                                  fill = TRUE),
                          downloadButton('downloadData','Download Data'),
                          DT::dataTableOutput("data")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Map",
                          h1("The map below depicts the locations of the water quality 
                          stations used to collect data for this study, as well as the 
                          four tributary study regions and their respective HUC 14s."),
                          leafletOutput("del_trib_map", height = "95vh")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         tags$style(".well {background-color:#222222;}"),
                                         selectizeInput("trib_info",label = strong("Select Tributary:",
                                                        style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                        choices =c("Pennsauken","Raccoon","Rancocas","BlacksCrosswicks"),
                                                        selected = ""),
                                         uiOutput("parameter"),
                                         uiOutput("locid"),
                                        HTML("<font color = 'white'>Author: Kevin Zolea\n (kevin.zolea@dep.nj.gov)</font>")),
                            mainPanel(plotlyOutput("plot1")%>%withSpinner(type = 5, color = "blue"),
                                      leafletOutput("map2")%>%withSpinner(type = 5, color = "blue")))))
###############################################################################
###############################################################################
### Creates server for app ###
server <- function(input, output,session) {
  ### Creates interactive data table for data tab ###
###############################################################################
  output$data <- DT::renderDataTable({
    DT::datatable(del_trib_data,filter = 'top',options = list(scrollX = TRUE,
                                                                           pageLength = 100))
  })
################################################################################## 
### Allows user to download data from data table page ###  
  output$downloadData<-downloadHandler(
    filename = function(){
      paste("dataset-",Sys.Date(),".tsv",sep="")
    },
    content = function(file){
      write_tsv(del_trib_data,file)
    })
###########################################################################  
### Creates interactive map of the 4 tribs with monitoring stations and flow stations ###
  output$del_trib_map<-renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      setView(lng = -74.4 ,lat =40, zoom = 10)%>%
      addPolygons(data = NJ_huc14s,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0,stroke = T,color = "black",
                  group = "HUC14s",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste0("HUC14 #:",HUC14TXT,sep = ","),
                  layerId = ~ NJ_huc14s$HUC14TXT)%>%
      addPolygons(data = raccoon,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Raccoon",stroke = T,color = "#f442e8",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste0("Raccoon Trib:",HUC14,sep = ","),
                  layerId = ~ raccoon$HUC14)%>%
      addPolygons(data = rancocas,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Rancocas",stroke = 1,color = "orange",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Rancocas Trib:",HUC14),
                  layerId = ~ rancocas$HUC14)%>%
      addPolygons(data = pennsuaken,color = "#F3161B",weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Pennsuaken",stroke = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Pennsuaken Trib:",HUC14),
                  layerId = ~ pennsuaken$HUC14)%>%
      addPolygons(data = blackscrosswicks,color = "purple",weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Blackscrosswicks",stroke = T,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Blackscrosswicks Trib:",HUC14),
                  layerId = ~ blackscrosswicks$HUC14)%>%
      addPolygons(data = trib_waters,color = "blue",weight = 1,smoothFactor = 1,
                  opacity = 0, fillOpacity = 1)%>%
      #addMarkers(data = USGS_flow_stations, group = "USGS Flow Stations",
      #           popup = ~paste("<h4> Station:</h4>",locid_x,sep = ""))%>%
      addMarkers(data = del_trib_wq_stations,lng = ~LongDeg,lat = ~LatDeg, group = "WQ Stations",
                 popup = ~paste("<h4> Station:</h4>",locid,sep = ""),
                 clusterOptions = markerClusterOptions())%>%
      addLayersControl(
        overlayGroups = c("Rancocas","Blackscrosswicks","Pennsuaken","Raccoon",
                          "Monitoring Stations"),
        options = layersControlOptions(collapsed = FALSE))
    
  })
###############################################################################
### Create reactive dataframe based on Trib input ###
  datasub<-reactive({
       foo <- subset(del_trib_data, Trib == input$trib_info)
       return(foo)
     })
### Drop down menu updates based on input from trib drop down ###
  output$parameter<-renderUI({
    selectizeInput("parameter_input",label = strong("Select Parameter:",
                                                    style = "color:white;font-weight: bold;font-size:1.3em;"),
                   choices = unique(datasub()$parameter),
                   selected = unique(datasub()$parameter[1]))
  })
  
  datasub2<-reactive({
    foo<-subset(datasub(),parameter == input$parameter_input)
    return(foo)
    
  })
###############################################################################
  ### Drop down menu updates based on input from trib drop down ###
    output$locid<-renderUI({
      selectizeInput("locid",label = strong("Select Monitoring Station:",
                              style = "color:white;font-weight: bold;font-size:1.3em;"),
                     multiple = TRUE,
                     choices = unique(datasub2()$locid),
                     selected = unique(datasub2()$locid[1]))
    })
   
    datasub3<-reactive({
      foo<-subset(datasub2(),locid == input$locid)
      return(foo)
    })
###############################################################################  
### Now create plot based on drop downs!!! ###
  output$plot1<-renderPlotly({
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
    p<-ggplot(data = datasub3(),aes(x=ActivityStartDate,y=val,group=1,
                                    text = paste("Date:",ActivityStartDate,
                                                 "<br>Value:",val,
                                                 "<br>Station:",locid)))+
      geom_point(aes(color = locid),size=1.3)+
      labs(x="Date",y=input$parameter_input,col="Monitoring Station",
           title = paste0(input$parameter_input," in ",input$trib_info," Tributary",collapse = ""))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    
    ### Converts ggplot object into plotly object ###
    ggplotly(p,dynamicTicks = "x",tooltip = "text")#%>%
      #add_annotations(text = "Station:",xref="paper", yref="paper",
      #                x=1.02, xanchor="left",
      #                y=0.8, yanchor="bottom",    # Same y as legend below
      #                legendtitle=F, showarrow=FALSE) #%>%
      #layout(legend=list(y=0.8, yanchor="top"))
    
    
  })
###############################################################################  
### Creates map under plot to see where each huc is ###
  output$map2<-renderLeaflet({
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      setView(lng = -74.4 ,lat =40, zoom = 7)%>%
      addPolygons(data = NJ_huc14s,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0,stroke = T,color = "black",
                  group = "HUC14s",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste0("HUC14 #:",HUC14TXT,sep = ","),
                  layerId = ~ NJ_huc14s$HUC14TXT)%>%
      addPolygons(data = tribs_merged,color = "blue",weight = 1,smoothFactor = 0,
                  opacity = 0, fillOpacity = 0)%>%
      addPolygons(data = raccoon,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Raccoon",stroke = T,color = "#f442e8",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste0("Raccoon Trib:",HUC14,sep = ","),
                  layerId = ~ raccoon$HUC14)%>%
      addPolygons(data = rancocas,weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Rancocas",stroke = 1,color = "orange",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Rancocas Trib:",HUC14),
                  layerId = ~ rancocas$HUC14)%>%
      addPolygons(data = pennsuaken,color = "#F3161B",weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Pennsuaken",stroke = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Pennsuaken Trib:",HUC14),
                  layerId = ~ pennsuaken$HUC14)%>%
      addPolygons(data = blackscrosswicks,color = "purple",weight = 1,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.4,
                  group = "Blackscrosswicks",stroke = T,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste("Blackscrosswicks Trib:",HUC14),
                  layerId = ~ blackscrosswicks$HUC14)%>%
      addPolygons(data = trib_waters,color = "blue",weight = 1,smoothFactor = 1,
                  opacity = 0, fillOpacity = 1)%>%
      #addMarkers(data = USGS_flow_stations, group = "USGS Flow Stations",
       #          popup = ~paste("<h4> Station:</h4>",locid_x,sep = ""))%>%
      addLayersControl(
        overlayGroups = c("Rancocas","Blackscrosswicks","Pennsuaken","Raccoon",
                          "Monitoring Stations"),
        options = layersControlOptions(collapsed = FALSE))
  
  })
############################################################################
### Make markers reactive to trib input ###
  wq_stations_reac<-reactive({
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
    del_trib_wq_stations%>%
      dplyr::filter(Trib == input$trib_info)
  })
### Updates map based on which trib is selected from drop down, then markers are added based on that ###
  observe({
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
  leafletProxy("map2")%>%
      addMarkers(data = wq_stations_reac(),lng = ~LongDeg,lat = ~LatDeg, group = "WQ Stations",
                 popup = ~paste("<h4> Station:</h4>",locid,sep = ""),
                 clusterOptions = markerClusterOptions())
  })
############################################################################
### Highlights polygon when Trib is clicked on drop down menu ###
############################################################################
  observeEvent(input$trib_info,{
    
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
    
    proxy<- leafletProxy("map2")
    
    click<-input$map2_shape_click
    
    hucsub<-subset(tribs_merged, Trib == input$trib_info)
    
    selected<- tribs_merged[tribs_merged$Trib == input$trib_info,]
    
    bbox<-st_bbox(selected)%>%
      as.vector()
    
    if(nrow(hucsub) == 0){
      proxy %>% removeShape(layerId = "Selected")
    } else if(length(click$id) && input$huc_input != click$id){
      proxy %>% 
        setView(lng = click$lng , lat = click$lat, zoom=10)%>%
        addPolygons(data = selected,
                    #fillColor = "yellow",
                    fillOpacity = 0.1,
                    color = "orange",
                    opacity = 2,
                    weight = 5,
                    stroke = T,
                    layerId = "Selected",
                    popup = paste("Trib Name:\n",selected$Trib,"<br>",
                                  sep = ""),
                    highlightOptions = highlightOptions(color = "blue",
                                                        weight = 2,bringToFront = TRUE))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    } else if(!length(click$id)){
      proxy %>% 
        setView(lng = click$lng , lat = click$lat, zoom=8)%>%
        addPolygons(data = selected,
                    #fillColor = "yellow",
                    fillOpacity = 0.1,
                    color = "orange",
                    opacity = 2,
                    weight = 5,
                    stroke = T,
                    layerId = "Selected",
                    popup = paste("Trib Name:\n",selected$Trib,"<br>",
                                  sep = ""),
                    highlightOptions = highlightOptions(color = "blue",
                                                        weight = 2,bringToFront = TRUE))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    }
    
  })
  
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  observe({
    req(input$trib_info)
    req(input$parameter_input)
    req(input$locid)
    
    click <- input$map2_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("map2")%>%
      setView(lng = click$lng , lat = click$lat, zoom=10)
  })

}
###############################################################################
# Run the application 
shinyApp(ui = ui, server = server)






