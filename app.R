### App for Delaware Tribs Project ###
### Author: Kevin Zolea ###
### Year: 2019 ###
###############################################################################
### Download necessary packages ###
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
               "readr","cowplot","lubridate","scales","shinythemes","plotly",
               "stringr","data.table","rlang","purrr","rmapshaper",
               "shiny","DT","leaflet","sf","shinyWidgets",
               "rsconnect","shinyjs","htmltools","htmlwidgets","leaflet.extras","readr")
###############################################################################
### Read in data ###
del_trib_data<-read_csv("results.csv",col_names = T)%>%
  dplyr::filter(!val == "NA")
###############################################################################
### Read in flow stations shapefile ###
USGS_flow_stations<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "USGS_Flow_Stations_KMZ")
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
#NJ_huc14s<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "2014_NJ_Integrated_Report_AU")
###############################################################################
### Read in water shapefile ###
trib_waters<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "del_trib_waters")
###############################################################################
### Read in each trib shapefile ###
pennsuaken<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "penn_hucs")
rancocas<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "RancocasHUCsfinal")
raccoon<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "Raccoon")
blackscrosswicks<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "BlacksCrosswicksHUCsfinal")
tribs_merged<-st_read("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/shapefiles",layer = "tribs_merge")
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
USGS_flow_stations<-st_transform(USGS_flow_stations, crs="+init=epsg:4326")
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
                 tabPanel("About App",
                          div(class= "outer",
                              tags$head(
                                #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
                                includeCSS("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/4tribinfo/Del_Tribs_Project/del_tribs_app/www/styles.css")),
                              h1(""),
                              h2("Introduction:"),
                              h3("The purpose of this app is to help guide decision making for the modeling efforts in the Delaware Tributaries Project."),
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
                          downloadButton('downloadData','Download Data'),
                          DT::dataTableOutput("data")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Map",
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
                                         #uiOutput("huc"),
                                         uiOutput("parameter"),
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
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Default)") %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      setView(lng = -74.4 ,lat =40, zoom = 10)%>%
      #addPolygons(data = NJ_huc14s,weight = 1,smoothFactor = 1,
      #            opacity = 1, fillOpacity = 0,stroke = T,color = "black",
      #            group = "HUC14s",
      #            highlightOptions = highlightOptions(color = "blue",
      #                                                weight = 2,bringToFront = TRUE),
      #            label = ~ paste0("HUC14 #:",HUC14TXT,sep = ","),
      #            layerId = ~ NJ_huc14s$HUC14TXT)%>%
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
      addMarkers(data = USGS_flow_stations, group = "USGS Flow Stations",
                 popup = ~paste("<h4> Station:</h4>",locid_x,sep = ""))%>%
      addMarkers(data = del_trib_wq_stations,lng = ~LongDeg,lat = ~LatDeg, group = "WQ Stations",
                 popup = ~paste("<h4> Station:</h4>",locid,sep = ""),
                 clusterOptions = markerClusterOptions())%>%
      addLayersControl(
        baseGroups = c("Satellite (Default)", "Grey", "OSM"),
        overlayGroups = c("Rancocas","Blackscrosswicks","Pennsuaken","Raccoon",
                          "USGS Flow Stations","WQ Stations"),
        options = layersControlOptions(collapsed = FALSE))
    
  })
###############################################################################
### Create reactive dataframe based on Trib input ###
  datasub<-reactive({
       foo <- subset(del_trib_data, Trib == input$trib_info)
       return(foo)
     })
  
### Drop down menu updates based on input from trib drop down ###
#  output$huc<-renderUI({
#    selectizeInput("huc_input",label = strong("Select HUC14:",style = "color:white;font-weight: bold;font-size:1.3em;"),
#                   choices = unique(datasub()$HUC14),
#                   selected = unique(datasub()$HUC14[1]))
#  })
  
#  datasub2<-reactive({
#    foo<-subset(datasub(),HUC14 == input$huc_input)
#    return(foo)
#  })
  
### Drop down menu updates based on input from huc drop down ###
  output$parameter<-renderUI({
    selectizeInput("parameter_input",label = strong("Select Parameter:",style = "color:white;font-weight: bold;font-size:1.3em;"),
                   choices = unique(datasub()$parameter),
                   selected = unique(datasub()$parameter[1]))
  })
  
  datasub2<-reactive({
    foo<-subset(datasub(),parameter == input$parameter_input)
    return(foo)
    
  })
###############################################################################  
### Now create plot based on drop downs!!! ###
  output$plot1<-renderPlotly({
    req(input$trib_info)
    #req(input$huc_input)
    req(input$parameter_input)
    p<-ggplot(data = datasub2(),aes(x=ActivityStartDate,y=val,group=1,
                                    text = paste("Date:",ActivityStartDate,
                                                 "<br>Value:",val,
                                                 "<br>Station:",locid)))+
      geom_point(aes(color = locid),size=1.3)+
      labs(x="Date",y=input$parameter_input,
           title = paste0("Trib:",input$trib_info,sep = ""))+
      theme(legend.title = element_blank())
      
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
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Default)") %>%
      setView(lng = -74.4 ,lat =40, zoom = 7)%>%
      #addPolygons(data = NJ_huc14s,weight = 1,smoothFactor = 1,
      #            opacity = 1, fillOpacity = 0,stroke = T,color = "black",
      #            group = "HUC14s",
      #            highlightOptions = highlightOptions(color = "blue",
      #                                                weight = 2,bringToFront = TRUE),
      #            label = ~ paste0("HUC14 #:",HUC14TXT,sep = ","),
      #            layerId = ~ NJ_huc14s$HUC14TXT)%>%
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
      addMarkers(data = USGS_flow_stations, group = "USGS Flow Stations",
                 popup = ~paste("<h4> Station:</h4>",locid_x,sep = ""))%>%
      addLayersControl(
        baseGroups = c("Satellite (Default)", "Grey", "OSM"),
        overlayGroups = c("Rancocas","Blackscrosswicks","Pennsuaken","Raccoon",
                          "USGS Flow Stations","WQ Stations"),
        options = layersControlOptions(collapsed = FALSE))
  
  })
############################################################################
### Make markers reactive to trib input ###
  wq_stations_reac<-reactive({
    req(input$trib_info)
    del_trib_wq_stations%>%
      dplyr::filter(Trib == input$trib_info)
  })
### Updates map based on which trib is selected from drop down, then markers are added based on that ###
  observe({
  leafletProxy("map2")%>%
      addMarkers(data = wq_stations_reac(),lng = ~LongDeg,lat = ~LatDeg, group = "WQ Stations",
                 popup = ~paste("<h4> Station:</h4>",locid,sep = ""),
                 clusterOptions = markerClusterOptions())
  })
############################################################################
### Highlights polygon when Trib is clicked on drop down menu ###
############################################################################
  observeEvent(input$trib_info,{
    
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

}
###############################################################################
# Run the application 
shinyApp(ui = ui, server = server)






