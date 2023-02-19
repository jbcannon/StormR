



#' Get type for icon (leaflet)
#'
#' @noRd
#' @param msw numeric
#'
#' @return corresponding icon
getType <- function(msw) {

  if (is.na(msw)) {
    type <- NA

  } else if (msw < sshs[1]) {
    type <- "TD"

  } else if (msw >= sshs[1] & msw < sshs[2]) {
    type <- "TS"

  } else if (msw >= sshs[2] & msw < sshs[3]) {
    type <- "cat1"

  } else if (msw >= sshs[3] & msw < sshs[4]) {
    type <- "cat2"

  } else if (msw >= sshs[4] & msw < sshs[5]) {
    type <- "cat3"

  } else if (msw >= sshs[5] & msw < sshs[6]) {
    type <- "cat4"

  } else if (msw >= sshs[6]) {
    type <- "cat5"
  }

  return(type)
}





#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#'
#' @return color associated with the observation
getColors <- function(msw) {

  if (is.na(msw)) {
    color <- NA

  } else if (msw < sshs[1]) {
    color <- sshsPalette[1]

  } else if (msw >= sshs[1] & msw < sshs[2]) {
    color <- sshsPalette[2]

  } else if (msw >= sshs[2] & msw < sshs[3]) {
    color <- sshsPalette[3]

  } else if (msw >= sshs[3] & msw < sshs[4]) {
    color <- sshsPalette[4]

  } else if (msw >= sshs[4] & msw < sshs[5]) {
    color <- sshsPalette[5]

  } else if (msw >= sshs[5] & msw < sshs[6]) {
    color <- sshsPalette[6]

  } else if (msw >= sshs[6]) {
    color <- sshsPalette[7]
  }

  return(color)
}





#' Plot track of a a storm
#'
#' This function plots the track of a storm on a map that should be previously plotted
#'
#' @noRd
#' @param st Storm object
#'
#' @return NULL
plotTrack <- function(st) {

  cex <- 1
  lon <- st@obs.all$lon
  lat <- st@obs.all$lat
  msw <- st@obs.all$msw
  colors <- unlist(lapply(msw, getColors))

  graphics::lines(
    lon,
    lat,
    col = "black",
    lty = st@lty.track,
    lwd = 1,
    cex = cex
  )

  graphics::points(
    lon,
    lat,
    col = colors,
    pch = 19,
    lwd = 1,
    cex = cex
  )

}




#' Add labels on plot
#'
#' Add ISO Times and name labels on the track of a storm on a map that should be
#' previsouly plotted
#' @noRd
#' @param st Storm object
#' @param by numeric. Increment of the sequence for the labels to plot
#' @param pos numeric. Must be between 1 and 4. Corresponds to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right)
#'
#' @return NULL
plotLabels <- function(st, by, pos) {

  cex <- 0.6
  ind <- round(seq(1, st@numobs.all, by))

  for (i in ind) {
    lon <- st@obs.all$lon[i]
    lat <- st@obs.all$lat[i]

    graphics::text(
      lon,
      lat,
      labels = paste0(st@name,"\n",
                     st@obs.all$iso.time[i],"\n",
                     "(",i,")\n"),
      pos = pos,
      cex = cex
    )
  }

}






#' Check inputs for plotStorms function
#'
#' @noRd
#' @param sts Storms object
#' @param names character vector
#' @param category numeric vector
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @param legends logical
#' @param loi logical
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param reset_setting logical
#' @param mode character
#' @return NULL
checkInputsPs <- function(sts, names, category, labels, by,
                          pos, legends, loi, xlim, ylim,
                          reset_setting, mode){

  #Checking sts input
  stopifnot("no data to plot" = !missing(sts))

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be characters" = identical(class(names), "character"))
    stopifnot("Invalid storm names (storm not found)" = names %in% sts@names)
  }

  #Checking category input
  if (!is.null(category)) {
    stopifnot("category must be numeric(s)" = identical(class(category), "numeric"))
    stopifnot("Invalid category input" = category %in% c(-1,-2,0,1,2,3,4,5))
  }

  #Checking xlim input
  if (!is.null(xlim)) {
    stopifnot("xlim must be numeric" = identical(class(xlim), "numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 &
                xlim <= 360)
  }

  #Checking ylim input
  if (!is.null(ylim)) {
    stopifnot("ylim must be numeric" = identical(class(ylim), "numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 &
                ylim <= 90)
  }

  #Checking logical inputs
  stopifnot("legends must be logical" = identical(class(legends), "logical"))
  stopifnot("loi must be logical" = identical(class(loi), "logical"))
  stopifnot("labels must be logical" = identical(class(labels), "logical"))
  stopifnot("reset_setting must be logical" = identical(class(reset_setting), "logical"))

  #Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  #Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))

  #Checking mode input
  stopifnot("mode must be character" = identical(class(mode), "character"))
  stopifnot("mode must length 1" = length(mode) == 1)
  stopifnot("Invalid mode" = mode %in% c("Classic", "Interactive"))



}





#' Plot several storm tracks
#'
#' This function plots a set of storm tracks contained in a Storms object (see “getStorms” function).
#' Depending on the inputs, the user can choose to plot only a desired set of storms
#' extracted from the Storms object .
#'
#' @param sts Storms object
#' @param names character vector. Name(s) of the storm(s) to plot on map. Default
#' value is set to NULL, which will plot every storm in sts. To see which storms
#' are included in a Storms object, you can use "sts@names"
#' @param category numeric vector. Should be either a category or a range of categories
#' in the Saffir Simpson scale (-2 to 5). Default value is set to NULL which
#' will consider every storm in sts. Otherwise it will consider only storms that
#' reached category input
#' @param loi logical. Whether or not to plot spatial.loi.buffer on the map
#' Default value is set to TRUE.
#' @param labels logical. Whether or not to plot ISO Times and name labels
#' @param by numeric. Defines the frequency at which labels are plotted for the
#' 3-hourly records. Default value is set to 8 which represents a 24h time interval
#' between each labeled observations. Ignored if labels == FALSE
#' @param pos numeric. Must be between 1 and 4. Correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#' Default value is set to 3. Ignored if labels == FALSE
#' @param legends logical. Whether or not to plot legends. Default value is set
#' to TRUE
#' @param xlim numeric vector. A set of longitude coordinates that controls the
#' longitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' @param ylim numeric vector. A set of latitude coordinates that controls the
#' latitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' @param reset_setting logical. Whether the graphical parameter should be reset on exit. Default
#' value is set to TRUE. It is usefull for the plotBehaviour function. We highly recommand not to change this
#' input.
#' @param mode character
#' @returns NULL
#' @import rworldxtra
#'
#' @examples
#' #Plot category 5 TCs in the WP Basin between 2010 and 2020
#' plotStorms(sts_wp, category = c(3,5))
#'
#' #Plot a single storm (ERICA) with labels every 24h
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE)
#'
#' #Plot a single storm (ERICA), with labels every 6h on the right side
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE, by = 2, pos = 4)
#'
#' #Plot a single storm (ERICA)
#' plotStorms(sts_nc, names = "ERICA")
#'
#'
#' @export
plotStorms <- function(sts, names = NULL, category = NULL, labels = FALSE,
                       by = 8, pos = 3, legends = TRUE, loi = TRUE, xlim = NULL,
                       ylim = NULL, reset_setting = TRUE,
                       mode = "Classic"){


  checkInputsPs(sts, names, category, labels, by, pos, legends,
                loi, xlim, ylim, reset_setting, mode)


  #Handling spatial extent
  ext <- terra::ext(sf::st_bbox(sts@spatial.loi.buffer)$xmin,
                    sf::st_bbox(sts@spatial.loi.buffer)$xmax,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymin,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymax)

  if (!is.null(xlim)) {
    xlim <- xlim[order(xlim)]
    ext$xmin <- xlim[1]
    ext$xmax <- xlim[2]
  }

  if (!is.null(ylim)) {
    ylim <- ylim[order(ylim)]
    ext$ymin <- ylim[1]
    ext$ymax <- ylim[2]
  }

  #Handling categories and names
  if(!is.null(category) & is.null(names)){

    if(length(category) == 2){
      category <- category[order(category)]
      cat.inf <- category[1]
      cat.sup <- category[2]
      ind <- which(sts@sshs >= cat.inf & sts@sshs <= cat.sup)

    }else{
      #length category == 1
      ind <- which(sts@sshs == category)
    }

    sts.aux <- unlist(sts@data)[ind]

  }else{

    if(!is.null(names)){
      if(!is.null(category))
        warning("category input ignored\n")

      ind <- which(sts@names %in% names)
      sts.aux <- unlist(sts@data)[ind]

    }else{
      sts.aux <- sts@data
    }
  }


  if(mode == "Classic"){

    #Plotting base map
    opar <- graphics::par(no.readonly = TRUE)
    #graphics::par(mar = c(5, 2, 2, 4), mai = c(0, 0, 0, 0))

    world <- rworldmap::getMap(resolution <- "high")
    maps::map(world,
              fill = TRUE,
              col = groundColor,
              bg = oceanColor,
              wrap = c(0, 360),
              xlim = c(ext$xmin-1, ext$xmax+1),
              ylim = c(ext$ymin-1, ext$ymax+1))
    maps::map.axes(cex.axis = 1)

    #Plotting loi
    if (loi)
      plot(sts@spatial.loi.buffer, lwd = 1, border = "blue", add = T)

    if(loi & as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT")
      plot(sts@spatial.loi, lwd = 2, col = "blue", pch = 4, add = T)

    #Plotting track(s) and labels
    lapply(sts.aux, plotTrack)
    if(labels)
      lapply(sts.aux, plotLabels, by, pos)

    #Adding legends
    if(legends) {

      leg <- c(expression(paste("TD (< 18 m.s" ^ "-1",")")),
               expression(paste("TS (18 to 32 m.s" ^ "-1",")")),
               expression(paste("Cat. 1 (33 to 42 m.s" ^ "-1",")")),
               expression(paste("Cat. 2 (43 to 49 m.s" ^ "-1",")")),
               expression(paste("Cat. 3 (50 to 58 m.s" ^ "-1",")")),
               expression(paste("Cat. 4 (58 to 70 m.s" ^ "-1",")")),
               expression(paste("Cat. 5 ( >= 70 m.s" ^ "-1",")")))

      col <- c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

      lty <- rep(0,7)
      pch <- rep(19,7)
      lwd <- rep(1,7)

      if (loi){
        if(loi & as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT"){
          leg <- c(leg, "LOI")
          col <- c(col, "blue")
          lty <- c(lty, 0)
          pch <- c(pch,4)
          lwd <- c(lwd, 2)
        }
        leg <- c(leg, "LOI buffer")
        col <- c(col, "blue")
        lty <- c(lty, 1)
        pch <- c(pch, NA)
        lwd <- c(lwd, 1)
      }



      graphics::legend(x = "topright", inset = c(-0.7, 0), xpd = TRUE,
                       legend = leg,
                       col = col,
                       lty = lty,
                       pch = pch,
                       lwd = lwd,
                       cex = 0.8)

    }

    if(reset_setting)
      on.exit(graphics::par(opar))

    return(NULL)

  }else{

    #Init map
    map <- leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = F,
                                                              minZoom = 2,
                                                              maxZoom = 12),
                            width = 650,
                            height = 700) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap, group = "Satellite",
                                options = leaflet::providerTileOptions(errorTileUrl = "Tile not found")) %>%
      leaflet::fitBounds(lng1 = as.numeric(ext$xmin), lng2 = as.numeric(ext$xmax),
                         lat1 = as.numeric(ext$ymin), lat2 = as.numeric(ext$ymax))

    #Plotting loi
    if(loi)
      map <- leaflet::addPolylines(map,
                                   data = sts@spatial.loi.buffer,
                                   fillColor = "transparent",
                                   label = "Buffer limit")

    if(loi & as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT")
      map <- leaflet::addCircleMarkers(map,
                                       data = sts@spatial.loi,
                                       fillColor = "transparent",
                                       label = "LOI")


    #Plotting track(s) and labels
    for(st in sts.aux){

      data <- st@obs.all
      data$type <- unlist(lapply(data$msw,getType))

      lbs <- paste0(st@name," (",row.names(data),")\n",data$iso.time)

      #Plot track
      map <- leaflet::addPolylines(map,
                                   data = data,
                                   lng = ~lon,
                                   lat = ~lat,
                                   color = "black",
                                   weight = 2)

      map <- leaflet::addMarkers(map,
                                 data = data,
                                 lng = ~lon,
                                 lat = ~lat,
                                 icon = ~Categories[type],
                                 label = lbs)
    }


    #Adding legends
    map <- leaflet::addLegend(map,
                              "topright",
                              colors = c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C",
                                                  "#FD8D3C", "#F03B20", "#BD0026"),


                              labels = c("Tropical Depression (< 18 m/s)",
                                         "Tropical Storm (18-32 m/s)",
                                         "Category 1 (33-42 m/s)",
                                         "Category 2 (43-49 m/s)",
                                         "Category 3 (50-58 m/s)",
                                         "Category 4 (58-70 m/s)",
                                         "Category 5 (>= 70 m/s)"),

                              title = "Saffir Simpson Hurricane Scale",
                              opacity = 1)

    map

    return(map)

  }

}
