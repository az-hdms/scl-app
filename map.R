# ICONS -------------------------------------------------------------------


blueIcon <- makeIcon(
  iconUrl = "./markers/marker-blue.png",
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)
redIcon <- makeIcon(
  iconUrl = "./markers/marker-red.png",
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)
blackIcon <- makeIcon(
  iconUrl = "./markers/marker-black.png",
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)
greyIcon <- makeIcon(
  iconUrl = "./markers/marker-grey.png",
  iconWidth = 9, iconHeight = 15,
  iconAnchorX = 4.5, iconAnchorY = 15,
  popupAnchorX = -1, popupAnchorY = -9
)



# MAPPING FUNCTION --------------------------------------------------------


pod_render_map <- function(data, snap = T) {
  if(nrow(data) != 0) {
    
    data <- st_transform(data, 4326)
    
    if(snap) {
      b <- st_bbox(data)
      
      leafletProxy("map") %>% 
        clearGroup("Submitted Data") %>%
        addMarkers(
          data = data,
          icon = blueIcon,
          layerId = paste0(data$id),
          group = "Submitted Data",
          popup = ~do.call(pod_popup, args = list(data = data)),
          # options = markerOptions(pane = "pod"),
          clusterOptions = markerClusterOptions(maxClusterRadius = 2)
          # clusterOptions = markerClusterOptions(clusterPane = "pod")
        ) %>% 
        fitBounds(b[[1]] - 1,  b[[2]] - 1, b[[3]] + 1, b[[4]] + 1)  # had to use double-brackets to get rid of names
    } else {
      
      leafletProxy("map") %>% 
        clearGroup("Submitted Data") %>%
        addMarkers(
          data = data,
          icon = blueIcon,
          layerId = paste0(data$id),
          group = "Submitted Data",
          popup = ~do.call(pod_popup, args = list(data = data)),
          # options = markerOptions(pane = "pod"),
          clusterOptions = markerClusterOptions(maxClusterRadius = 2)
          # clusterOptions = markerClusterOptions(clusterPane = "pod")
        ) 
    }
    

  } else {
    leafletProxy("map") %>% clearGroup("Submitted Data")
  }
}


# POPUP -------------------------------------------------------------------


pod_popup = function(data) {
  glue::glue(
    '
    <h5>{data$species}</h5>
    <strong>ObsDate:</strong>           {data$obs_date        } <br/>
    <strong>NameUsed:</strong>          {data$name_used       } <br/>
    <strong>Count:</strong>             {data$num_observed    } <br/>
    <strong>County:</strong>            {data$county          } <br/>
    <strong>Directions:</strong>        {data$directions      } <br/>
    <strong>X:</strong>                 {data$loc_x           }  
    <strong>Y:</strong>                 {data$loc_y           } <br/>
    <strong>General Notes:</strong>     {data$general_notes   } <br/>
    {actionButton("pod_move", "Move", onclick = "Shiny.onInputChange(\'move\', Math.random())")}
    '
  )
}



# ANNOTATION UI -----------------------------------------------------------


pod_annotation_ui <- tagList(
  h1("POD"),
  textInput("pod_search", "Search species", value = "Abutilon parishii")
)


# ANNOTATION LOGIC --------------------------------------------------------


pod_annotate_db <- function(con, input) {
  # out <- st_read(con, query = glue::glue("select * from pod_clean_full where species = '{input$`pod_search`}'"))
  # print(out)
  print("server code for POD")
}



# MOVE --------------------------------------------------------------------


pod_move_popup = function() {
  glue::glue(
    '
    {actionButton(
      "confirm", "Confirm", icon = icon("check"), width = "140px",
      onclick = "Shiny.onInputChange(\'move_confirm_popup\', Math.random())",
      style="color: #fff; background-color: #449d44; border-color: #449d44;"
    )}<br/>
    {actionButton(
      "move_cancel", "Cancel", icon = icon("ban"), width= "140px",
      onclick = "Shiny.onInputChange(\'move_cancel\', Math.random())",
      style="color: #fff; background-color: #d9534f; border-color: #d9534f;"
    )}
    '
  )
}

pod_move_confirm_modal_ui = 
  tagList(
    textAreaInput("pod_mapping_notes", "Mapping Notes")
  )

pod_move_confirm_function <- function(input) {
  print(input$pod_mapping_notes)
}




# Past SCL Data -----------------------------------------------------------

scl_render_map <- function(data) {
  if(nrow(data) != 0) {
    
    data <- st_transform(data, 4326)
      
    leafletProxy("map") %>% 
      clearGroup("Past SCL Data") %>%
      addMarkers(
        data = data,
        icon = greyIcon,
        layerId = paste0(data$po_id),
        group = "Past SCL Data",
        popup = ~do.call(scl_popup, args = list(data = data)),
        # options = markerOptions(pane = "pod"),
        clusterOptions = markerClusterOptions(maxClusterRadius = 2)
        # clusterOptions = markerClusterOptions(clusterPane = "pod")
      ) 
    
  } else {
    leafletProxy("map") %>% clearGroup("Past SCL Data")
  }
}


scl_popup = function(data) {
  glue::glue(
    '
    <h5>{data$species}</h5>
    <strong>ObsDate:</strong>           {data$obs_date        } <br/>
    <strong>NameUsed:</strong>          {data$name_used       } <br/>
    <strong>Count:</strong>             {data$num_observed    } <br/>
    <strong>County:</strong>            {data$county          } <br/>
    <strong>Directions:</strong>        {data$directions      } <br/>
    <strong>X:</strong>                 {data$loc_x           }  
    <strong>Y:</strong>                 {data$loc_y           } <br/>
    <strong>General Notes:</strong>     {data$general_notes   } <br/>
    '
  )
}

# Move -------------------------------------------------------------------

add_move_marker <- function(data, move_popup, id_field) {
  
  leafletProxy("map") %>%
    addMarkers(
      data = data %>% st_transform(4326),
      # lng = data$lng,
      # lat = data$lat,
      layerId = paste0("move_", getElement(data, id_field)),
      popup = do.call(move_popup, list()),
      group = "move",
      icon = redIcon,
      options = markerOptions(draggable = T)
    )
}



add_non_move_marker <- function(data, id_field) {
  leafletProxy("map") %>%
    addMarkers(
      data = data %>% st_transform(4326),
      layerId = paste0("move_", getElement(data, id_field)),
      icon = blackIcon,
      group = "move",
      # popup = ~do.call(popup, args = list(data = data)),
      options = markerOptions(clickable = F)
    )
}

