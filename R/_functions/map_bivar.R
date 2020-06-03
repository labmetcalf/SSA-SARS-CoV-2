#' Bivariate risk maps
#' Bivariate plots of various risk factors.
#' @param x x variable to plot
#' @param y y variable to plot
#' @param lab_x label for x variable
#' @param lab_y label for y variable
#' @param loc_labs labels for locations
#' @param loc_ids ids to match to shapefile
#' @param sf_obj sf object to plot the map, should have column called loc_id to match to the data loc_ids 
#' @param continuous if TRUE uses color by alpha to generate a continuous gradient, if FALSE maps 
#' onto discrete color values
#' @param trans_x name of function by which to transform variable (i.e "log"), 
#' check ggplot2::scale_y_continuous for other options
#' @param trans_y name of function by which to transform variable (i.e "log", "sqrt),
#'  check ggplot2::scale_x_continuous for other options
#' @param rev_x reverse color scale for x variable?
#' @param rev_y reverse color scale for y variable?
#' @param probs_x numeric vector [0, 1], how to classify low-mid-high
#' @param probs_y numeric vector [0, 1], how to classify low-mid-high
#' @param static  if TRUE returns a patchwork of ggplots; if FALSE then returns list of 
#' leaflet & plotly objects
#' @return Returns either a patchwork object or list of html plots of bivariate map, legend, and scatter
#' plot. 
#' @section Dependencies: dplyr, glue, stringr, scales, patchwork, cowplot, purrr, leaflet, plotly,
#' htmltools, also global function get_color_continuous
#' 

map_bivariate <- function(x, y, lab_x, lab_y, loc_labs, loc_ids, sf_obj,
                          continuous = FALSE, trans_x = "log", trans_y = "log",
                          rev_x = FALSE, rev_y = FALSE,
                          probs_x = c(0.25, 0.75), probs_y = c(0.25, 0.75), 
                          static = TRUE) {
 
  # First transform vars
  if(!is.null(trans_x)) x_val <- get(trans_x)(x) 
  if(!is.null(trans_y)) y_val <- get(trans_y)(y)
        
  # Then get bivariate df
  bivar_df <- data.frame(x, y, x_perc = ecdf(x_val)(x_val), y_perc = ecdf(y_val)(y_val),
                         loc_lab = loc_labs, loc_id = loc_ids) 
  
  # Reverse or not?
  bivar_df %>%
     mutate(rev_x = rev_x, rev_y = rev_y,
            x_perc_map = ifelse(rev_x == TRUE, 1 - x_perc, x_perc), 
            y_perc_map = ifelse(rev_y == TRUE, 1 - y_perc, y_perc)) -> bivar_df
  
  if(continuous == FALSE) {
    pal <- c("low-low" = "#E8E6F2", "low-mid" = "#B5D3E7", "low-high" = "#4FADD0", 
             "mid-low" = "#E5B4D9", "mid-mid" = "#B8B3D8", "mid-high" = "#3983BB", 
             "high-low" = "#DE4FA6", "high-mid" = "#B03598", "high-high" = "#2A1A8A")
    
    bivar_df %>%
      mutate(class_x = case_when(x_perc_map <= probs_x[1] ~ "low", 
                                 x_perc_map > probs_x[1] & x_perc_map < probs_x[2] ~ "mid", 
                                 x_perc_map >= probs_x[2] ~ "high"),
             class_y = case_when(y_perc_map <= probs_y[1] ~ "low", 
                                 y_perc_map > probs_y[1] & y_perc_map < probs_y[2] ~ "mid", 
                                 y_perc_map >= probs_y[2] ~ "high"),
             color = pal[match(paste(class_x, class_y, sep = "-"), names(pal))]) -> bivar_df
    
    legend_df <-  data.frame(color = pal, x_coord = rep(c(0, 1, 2), each = 3), 
                             y_coord = rep(c(0, 1, 2), times = 3))
    leg_x_labs <- c(glue("< {round(probs_x[1]*100)} %"), 
                    glue("{round(probs_x[1]*100)} - {round(probs_x[2]*100)} %"),
                    glue("> {round(probs_x[2]*100)} %"))
    leg_y_labs <-  c(glue("< {round(probs_y[1]*100)} %"), 
                     glue("{round(probs_y[1]*100)} - {round(probs_y[2]*100)} %"), 
                     glue("> {round(probs_y[2]*100)} %"))
  }
  
  if(continuous == TRUE) {
    bivar_df %>%
      mutate(color = pmap_chr(list(x_perc = x_perc_map, y_perc = y_perc_map), 
                          get_color_continuous)) -> bivar_df
    legend_df <- expand.grid(x_coord = seq(0, 1, by = 0.1), 
                             y_coord = seq(0, 1, by = 0.1))
    legend_df %>%
      mutate(color = pmap_chr(list(x_perc = x_coord, y_perc = y_coord), 
                          get_color_continuous)) -> legend_df
    leg_x_labs <- seq(0, 1, by = 0.1)
    leg_x_labs[seq(2, length(leg_x_labs), by = 2)] <- ""
    leg_y_labs <- leg_x_labs
  }
  
  sf_obj <- left_join(sf_obj, bivar_df)
  
  # Format labels
  if(rev_x == TRUE) leg_x_labs <- rev(leg_x_labs)
  if(rev_y == TRUE) leg_y_labs <- rev(leg_y_labs)
  
  
  ggplot(data = legend_df, aes(x = factor(x_coord), y = factor(y_coord), fill = color)) +
    geom_tile() +
    scale_fill_identity() +
    labs(x = str_wrap(paste("Percentile", lab_x), 25), y = str_wrap(paste("Percentile", lab_y), 25)) +
    theme_half_open() +
    scale_x_discrete(labels = leg_x_labs) +
    scale_y_discrete(labels = leg_y_labs) +
    theme(axis.text.y = element_text(size = 8), 
          axis.text.x = element_text(angle = 90, size = 8),
          text = element_text(size = 10)) -> legend

  ggplot(data = bivar_df, 
         aes(x = x, y = y, fill = color,
             text = glue("{loc_lab}
                         {str_wrap(lab_x, 25)}: {round(x, 2)}
                         {str_wrap(lab_y, 25)}: {round(y, 2)}"))) +
    geom_point(size = 2.5, color = "black", shape = 21) +
    scale_fill_identity() +
    labs(x = str_wrap(lab_x, 25), y = str_wrap(lab_y, 25)) + 
    scale_x_continuous(trans = trans_x, labels = label_number()) +
    scale_y_continuous(trans = trans_y, labels = label_number()) +
    theme_minimal_grid() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12)) -> scatter
  
  if(static == TRUE) {
    
    ggplot(sf_obj) + 
      geom_sf(aes(fill = color)) + 
      scale_fill_identity() + 
      theme_map() -> map 
    
   layout_inset <- c(patchwork::area(t = 1, b = 6, l = 1, r = 8), 
                     patchwork::area(t = 5, b = 5, l = 2, r = 2),
                     patchwork::area(t = 2, b = 5, l = 9, r = 11),
                     patchwork::area(t = 1, b = 6, l = 12, r = 12))

    out <- map + legend + scatter + plot_spacer() + plot_layout(design = layout_inset)
    
  } else {
    
    labels <- sprintf(
      "<strong>%s</strong><br/> %s: %0.2f (%0.2f percentile) <br/> %s: %0.2f (%0.2f percentile)",
      sf_obj$name_0, str_wrap_html(lab_x, 40), sf_obj$x, sf_obj$x_perc*100, str_wrap_html(lab_y, 40), 
      sf_obj$y, sf_obj$y_perc*100 
    ) %>% lapply(htmltools::HTML)
    
    
    # Use plotly & leaflet
    leaflet() %>%
      fitBounds(-25.35875, -40.37063, 63.5003, 37.54327) %>% # from bbox(admin2)
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = sf_obj,
                  color = "black", weight = 0.001, smooth = 0.3, 
                  fillColor = ~color,
                  fillOpacity = 0.8,
                  dashArray = NULL,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "grey",
                    dashArray = NULL,
                    fillOpacity = 0.75,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = sf_obj, color = "#444444", weight = 2, fill = FALSE) -> map
    
    ggplotly(scatter, tooltip = "text") %>% hide_legend() -> scatter
    
    ggplotly(legend) %>% hide_legend() %>% style(hoverinfo = "skip") -> legend
    
    out <- list(legend = legend, scatter = scatter, map = map)
  }
  return(out)
}

#' Continuous color palette
#' Adapted from Ian's code
#' @param x_perc the percentile of the x variable (from ecdf)
#' @param y_perc the percentile of the y variable (from ecdf)
#' @return colors for each 
#' @section Dependencies: none
#'  
get_color_continuous <- function(x_perc, y_perc) {
  
    alpha <- 0.5*max(x_perc, y_perc) + 0.5*x_perc*y_perc
    
    if(x_perc >= y_perc) col.val <- colorRamp(c("#B03598", "#2A1A8A"))(y_perc / x_perc)
    if(x_perc < y_perc)  col.val <- colorRamp(c("#3983BB", "#2A1A8A"))(x_perc / y_perc) 
    if(x_perc == 0 & y_perc == 0) col.val <- rep(255, 3)
    
    rgb(col.val[1], col.val[2], col.val[3], alpha = alpha*255, maxColorValue = 255)
}

# Helper function for formatting html labels
str_wrap_html <- function(x, width) { gsub("\n", "<br/>", str_wrap(x, width = width))}
