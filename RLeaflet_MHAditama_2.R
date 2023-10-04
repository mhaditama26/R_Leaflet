library(leaflet)
library(dplyr)
library(ggplot2)
library(ggmap)
library(sp)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(leaflet.extras)
library(png)
library(raster)
library(leafem)
library(leaflet.extras2)

#mapview(breweries)
##data valid
##data update 2009
##ippkh <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/NTB_Top_PP_pl_250k_2009Forest.shp")
##data update tatabatas Feb 23
ippkh <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/RTK59update_PTAMNT_Rekons2022_20230613/PPKH_PTAMNT_Rekons2022_poly_WGS.shp")

kontrakkarya <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/NNT_KK_2016_Polygon.shp")
ieks <- readOGR("X:/AMNT_VECTOR/Kontrak_Karya_AMNT_.shp")
iupk <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/NTB_Lnd_Ten_pl_2017PTAMNT_IUPK_WGS.shp")
projectarea <- readOGR ("X:/AMNT_VECTOR/AMNT_ADMIN/NTB_Lnd_Ten_pl_2017_Project_Area_WGS.shp")
iupl <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/SBW_IUP_pl.shp")
ksb <- readOGR("X:/AMNT_VECTOR/AMNT_ADMIN/KSB_Top_Brd_pl_25k_Regence_WGS.shp")

dembh <- raster("Y:/DEM_BH/Hillshade DEM BH WGS.tif")
SW <- read.csv("X:/AMNT_ENV/MONITORING/stasiun.csv")
SW_icon = makeIcon ("X:/AMNT_ENV/MONITORING/SW.png",25,25)
#BH_WV2_Rs_2018
#Kawasan_Hutan_NTB

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

printPlugin <- htmlDependency(
  "leaflet-browser-print","1.0.6",
  src = "X:/R SCRIPT/Library/leaflet.browser.print-1/dist",
  script = "leaflet.browser.print.js"
)





m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery,toggleDisplay = TRUE, minimized = TRUE)%>%
  setView(lng = 116.842, lat = -8.971, zoom = 13) %>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "DEM_RAW",options = WMSTileOptions( format = "image/png",transparent = T),group = "National Elevation")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "DEM_BH",options = WMSTileOptions( format = "image/png",transparent = T),group = "Batu Hijau Elevation")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_WV_2_Rs_2016",options = WMSTileOptions( format = "image/png",transparent = T),group = "Batu Hijau 2016")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_WV2_Rs_2018",options = WMSTileOptions(format = "image/png",transparent = F),group = "Batu Hijau 2018")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_WV_Rs_2019",options = WMSTileOptions(format = "image/png",transparent = T),group = "Batu Hijau 2019")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_SkySat_2021",options = WMSTileOptions(format = "image/png",transparent = T),group = "Batu Hijau 2021")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "Kawasan_Hutan_NTB",options = WMSTileOptions(format = "image/png",transparent = T),group = "Forest Boundary 2017")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "NTB_Top_Veg_pl_250k_2009Forest",options = WMSTileOptions( format = "image/png",transparent = T),group = "Forest Boundary 2009")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "SBW_Top_Loc_tx_25k_Settlement",options = WMSTileOptions( format = "image/png",transparent = T),group = "Attribute")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "SBW_Top_Loc_tx_25k_Bay",options = WMSTileOptions(format = "image/png",transparent = T),group = "Attribute")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "SBW_Top_Loc_tx_25k_Cap",options = WMSTileOptions(format = "image/png",transparent = T),group = "Attribute")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "Sungai",options = WMSTileOptions(format = "image/png",transparent = T),group = "River")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_2022",options = WMSTileOptions(format = "image/png",transparent = T),group = "Batu Hijau 2022")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "RTK59update_PTAMNT_Rekons2022_WGS",options = WMSTileOptions(format = "image/png",transparent = T),group = "Forest Boundary 2022")%>%
  addWMSTiles("http://indbhsv0078:8080/geoserver/amnt/wms?",layers = "BH_2023",options = WMSTileOptions(format = "image/png",transparent = T),group = "Batu Hijau 2023")%>%
  
  leafem::addMouseCoordinates() %>%
  leafem::addLogo(img = "http://webgis.amnt.id/webgis_js/icon/favicon-16x16.png", url = "http://www.amman.co.id/",
                  position = "bottomleft",
                  offset.x = 7,
                  offset.y = 7,
                  width = 16,
                  height = 16) %>% 
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "hectares",
    activeColor = "#3D535D",
    completedColor = "#7D4479")%>%
  
  #  registerPlugin(printPlugin) %>%
  #    onRender("function(el, x) {
  #            L.easyPrint({
  #                    title: 'My awesome print button',
  #                  	position: 'bottomright',
  #                    sizeModes: ['A4Portrait', 'A4Landscape'],
  #                    exportOnly : true,
  #                    filename :'map.jpg'
  #            }).addTo(this);}") %>%
  
registerPlugin(printPlugin) %>%
  onRender("function(el, x) {
           L.control.browserPrint({
           title: 'My awesome print button',
           documentTitle : 'Amman Mineral Nusa Tenggara',
          printModes: [
              L.control.browserPrint.mode.landscape('Landscape'),
              L.control.browserPrint.mode.portrait('Portrait')
            ],
           manualMode: false
           }).addTo(this);}") %>%
  
  addPolygons (data = ippkh,
               fill = T,
               fillOpacity = 0,
               color = "#FF00FF", 
               weight = 2,
               label = ippkh$Keterangan,
               popup = paste (ippkh$Keterangan, "<br>",
                              ippkh$Fungsi_KH),
               group = "IPPKH Batu Hijau") %>%
  addPolygons (data = iupl,
               fill = T,
               fillOpacity = 0,
               color = "#C0C0C0", 
               weight = 2,
               label = iupl$perusahaan,
               popup = paste (iupl$perusahaan),
               group = "IUP / IUPK Sumbawa") %>%
  addPolygons (data = projectarea,
               fill = T,
               fillOpacity = 0,
               color = "#EEC311", 
               weight = 2,
               label = projectarea$keterangan,
               popup = paste (projectarea$keterangan),
               group = "Project Area") %>%
  addPolygons (data = ksb,
               fill = T,
               fillOpacity = 0,
               color = "#EEC311", 
               weight = 2,
               label = ksb$Nama,
               popup = paste (ksb$Nama),
               group = "Kab. Sumbawa Barat") %>%
  addPolygons (data = kontrakkarya,
               fill = T,
               fillOpacity = 0,
               color = ifelse (
                 kontrakkarya$Area == "Blok I - Batu Hijau",
                 yes = "#33FF3F",
                 no = ifelse (
                   kontrakkarya$Area == "Blok II - Elang",
                   yes = "#3968BC",
                   no = ifelse (
                     kontrakkarya$Area == "Blok III - Rinti",
                     yes = "#D6DD3C",
                     no = ifelse(
                       kontrakkarya$Area == "Blok IV - North Lunyuk",
                       yes = "#EEC311",
                       no = "#000000" 
                     )
                   )
                 )
               ), 
               weight = 2,
               label = kontrakkarya$Area,
               popup = paste (kontrakkarya$Area),
               group = "Contract of Work")%>%
  addPolygons (data = ieks,
               fill = T,
               fillOpacity = 0,
               color = ifelse (
                 ieks$Keterangan == "Blok I - Batu Hijau",
                 yes = "#33FF3F",
                 no = ifelse (
                   ieks$Keterangan == "Blok II - North Lunyuk",
                   yes = "#3968BC",
                   no = ifelse (
                     ieks$Keterangan == "Blok III - Dodo Elang",
                     yes = "#D6DD3C",
                     no = ifelse(
                       ieks$Keterangan == "Blok IV - Rinti",
                       yes = "#EEC311",
                       no = ifelse(
                         ieks$Keterangan == "Blok V - Labangka",
                         yes = "#FF00FF",
                         no = "#000000"
                       )
                     )
                   )
                 )
               ), 
               weight = 2,
               label = ieks$Keterangan,
               popup = paste (ieks$Keterangan),
               group = "IPPKH Eksplorasi")%>%
  addPolygons (data = iupk,
               fill = T,
               fillOpacity = 0,
               color = ifelse (
                 iupk$blok == "I",
                 yes = "#33FF3F",
                 no = ifelse (
                   iupk$blok == "II",
                   yes = "#3968BC",
                   no = ifelse (
                     iupk$blok == "III",
                     yes = "#D6DD3C",
                     no = ifelse(
                       iupk$blok == "IV",
                       yes = "#EEC311",
                       no = "#000000" 
                     )
                   )
                 )
               ), 
               weight = 2,
               label = iupk$keterangan,
               popup = paste (iupk$keterangan),
               group = "IUPK")%>%
  #addRasterImage(dembh,opacity = 0.8)%>%
  addLegend("bottomright",  
            colors = c("#33FF3F","#3968BC","#D6DD3C","#EEC311"),
            labels = c("Blok I - Batu Hijau","Blok II - Elang","Blok III - Rinti","Blok IV - North Lunyuk"),
            title = "Kontrak Karya",
            opacity = 1,
            group = "Contract of Work")%>%
  addLegend("bottomright",  
            colors = c("#33FF3F","#3968BC","#D6DD3C","#EEC311","#FF00FF"),
            labels = c("Blok I - Batu Hijau","Blok II - North Lunyuk","Blok III - Dodo Elang","Blok IV - Rinti","Blok V - Labangka"),
            title = "IPPKH Eksplorasi",
            opacity = 1,
            group = "IPPKH Eksplorasi")%>%
  addLegend("bottomright",  
            colors = c("#33FF3F","#3968BC","#D6DD3C","#EEC311"),
            labels = c("Blok I","Blok II","Blok III","Blok IV"),
            title = "IUPK",
            opacity = 1,
            group = "IUPK")%>%
  addLegend("bottomright",  
            colors = c("#C1FFA8","#FEFEAA","#38A800","#FFFFFF","#C48AFF"),
            labels = c("Hutan Produksi Terbatas","Hutan Produksi","Hutan Lindung","Area Penggunaan Lain","Kawasan Suaka Alam"),
            title = "Kawasan Hutan",
            opacity = 1,
            group = "Forest Boundary 2017")%>%
  addLegend("bottomright",  
            colors = c("#C1FFA8","#FEFEAA","#38A800","#FFFFFF","#C48AFF"),
            labels = c("Hutan Produksi Terbatas","Hutan Produksi","Hutan Lindung","Area Penggunaan Lain","Kawasan Suaka Alam"),
            title = "Kawasan Hutan",
            opacity = 1,
            group = "Forest Boundary 2009")%>%
  addLegend("bottomright",  
            colors = c("#248e82","#ac813d"),
            labels = c("max 1019 m","min -5 m"),
            title = "Elevasi",
            opacity = 1,
            group = "National Elevation")%>%
  addLegend("bottomright",  
            colors = c("#8e2430","#0715cd"),
            labels = c("max 1025 m","min -315 m"),
            title = "Elevasi",
            opacity = 1,
            group = "Batu Hijau Elevation")%>%
  addDrawToolbar(position = "topleft",
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  addResetMapButton() %>%
  addSearchFeatures(
    targetGroups = (c("IPPKH", "Kontrak Karya","SW Monitoring","IUPK")),
    options = searchFeaturesOptions(
      zoom=15, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  addLayersControl(baseGroups = c("Esri World Imagery","Batu Hijau 2016","Batu Hijau 2018","Batu Hijau 2019","Batu Hijau 2021","Batu Hijau 2022","Batu Hijau 2023"),
                   overlayGroups = c("Contract of Work","IPPKH Batu Hijau","IPPKH Eksplorasi", "IUPK",
                                     "Project Area","Forest Boundary 2022","Forest Boundary 2017","Forest Boundary 2009",
                                     "IUP / IUPK Sumbawa","Kab. Sumbawa Barat","Batu Hijau Elevation",
                                     "National Elevation","River","Attribute",
                                     "<a href ='http://webgis.amnt.id/webgis_js/Reclamation.html' </a>  Reclamation",
                                     "<a href ='http://webgis.amnt.id/Monitoring.html' </a>  Monitoring Point",
                                     "<a href ='http://webgis.amnt.id/amnt_RehabDAS.html' </a>  Watershed Rehabilitation",
                                     "<a href ='http://webgis.amnt.id/webgis_js/MonitoringEcology.html' </a>  Ecology",
                                     "<a href ='http://webgis.amnt.id/webgis_js/security.html' </a>  LV & SmartBadge Territory"),
                   options = layersControlOptions(collapsed = TRUE))
m %>% hideGroup (c("Contract of Work","IUPK","Project Area","Forest Boundary 2022","Forest Boundary 2017","Forest Boundary 2009",
                   "IPPKH Eksplorasi","IUP / IUPK Sumbawa","Attribute","Batu Hijau Elevation","National Elevation",
                   "Kab. Sumbawa Barat","River",
                   "<a href ='http://webgis.amnt.id/webgis_js/Reclamation.html' </a>  Reclamation",
                   "<a href ='http://webgis.amnt.id/Monitoring.html' </a>  Monitoring Point",
                   "<a href ='http://webgis.amnt.id/amnt_RehabDAS.html' </a>  Watershed Rehabilitation",
                   "<a href ='http://webgis.amnt.id/webgis_js/MonitoringEcology.html' </a>  Ecology",
                   "<a href ='http://webgis.amnt.id/webgis_js/security.html' </a>  LV & SmartBadge Territory"))


