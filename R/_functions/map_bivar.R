#' Bivariate risk maps
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

map_bivariate <- function(x, y, xlab, ylab, loc_labs, loc_ids, sf_obj,
                          continuous = FALSE, transform = log, 
                          x_probs = c(0.25, 0.75), y_probs = c(0.25, 0.75), 
                          auto_annotate = TRUE) {
  browser()
  # First transform vars
  if(!is.null(transform)) {
    x_val <- transform(x); y_val <- transform(y);
  } else {
    x_val <- x; y_val <- y
  }

  # Then get bivariate df
  bivar_df <- data.frame(x, y, x_perc = ecdf(x_val)(x_val), y_perc = ecdf(y_val)(y_val),
                         loc_lab = loc_labs, loc_id = loc_ids) 
  
  if(continuous == FALSE) {
    pal <- c("low-low" = "#E8E6F2", "low-mid" = "#B5D3E7", "low-high" = "#4FADD0", 
             "mid-low" = "#E5B4D9", "mid-mid" = "#B8B3D8", "mid-high" = "#3983BB", 
             "high-low" = "#DE4FA6", "high-mid" = "#B03598", "high-high" = "#2A1A8A")
    
    bivar_df %>%
      mutate(class_x = case_when(x_perc <= x_probs[1] ~ "low", 
                                 x_perc > x_probs[1] & x_perc < x_probs[2] ~ "mid", 
                                 x_perc >= x_probs[2] ~ "high"),
             class_y = case_when(y_perc <= y_probs[1] ~ "low", 
                                 y_perc > y_probs[1] & y_perc < y_probs[2] ~ "mid", 
                                 y_perc >= y_probs[2] ~ "high"),
             color = pal[match(paste(class_x, class_y, sep = "-"), names(pal))]) -> bivar_df
    
    legend_df <-  data.frame(color = pal, x_coord = rep(c(0, 1, 2), each = 3), 
                             y_coord = rep(c(0, 1, 2), times = 3))
  }
  
  if(continuous == TRUE) {
    bivar_df %>%
      mutate(color = pmap(list(x_perc = x_perc, y_perc = y_perc), 
                          get_color_continuous)) -> bivar_df
    legend_df <- expand.grid(x_coord = seq(0, 1, by = 0.1), 
                             y_coord = seq(0, 1, by = 0.1))
    legend_df %>%
      mutate(color = pmap(list(x_perc = x_coord, y_perc = y_coord), 
                          get_color_continuous)) -> legend_df
    
  }
  
  if(auto_annotate == TRUE) {
    bivar_df %>%
      filter(class_x != "mid", class_y != "mid") %>%
      mutate(class = paste(class_x, class_y, sep = "-")) %>%
      group_by(class) %>%
      sample_n(1) %>%
      mutate(label = glue("{loc_lab} is in the {round(x_perc*100)} percentile for {xlab} and in the {round(y_perc*100)} percentile for {ylab}")) %>%
      right_join(bivar_df) %>%
      mutate(label = ifelse(is.na(label), "", str_wrap(label, 40))) -> bivar_df
  }
  
  sf_obj <- left_join(sf_obj, bivar_df, by = c("iso" = "loc_id"))
  
  ggplot(data = legend_df, aes(x = factor(x_coord), y = factor(y_coord), fill = color)) +
    geom_tile() +
    scale_fill_identity() +
    labs(x = str_wrap(xlab, 20), y = str_wrap(ylab, 20)) +
    theme_half_open() -> legend
  
  if(continuous == FALSE) {
    legend +
      scale_x_discrete(labels = c(glue("< {round(x_probs[1]*100)} %"), 
                                  glue("{round(x_probs[1]*100)} - {round(x_probs[2]*100)} %"),
                                  glue("> {round(x_probs[2]*100)} %"))) +
      scale_y_discrete(labels = c(glue("< {round(y_probs[1]*100)} %"), 
                                  glue("{round(y_probs[1]*100)} - {round(y_probs[2]*100)} %"),
                                  glue("> {round(y_probs[2]*100)} %"))) -> legend
  }
  
  if(static == TRUE) {
    
    ggplot(sf_obj) + 
      geom_sf(aes(fill = color)) + # polygon fill
      scale_fill_identity() + # color scale no guide!
      theme_map() -> map # cowplot theme map 
    
    ggplot(data = bivar_df, aes(x = x, y = y, fill = color)) +
      geom_point(size = 2.5, color = "black", shape = 21) +
     # geom_text_repel(aes(label = label), min.segment.length = 0,
     #                 hjust = -0.5, ylim = c(-10, max(bivar_df$y) + 10)) +
      scale_fill_identity() +
      labs(x = xlab, y = ylab) + 
     # ylim(c(-10, max(bivar_df$y) + 10)) +
      theme_minimal_grid() -> scatter
    
   layout_inset <- c(patchwork::area(t = 1, b = 6, l = 1, r = 8), 
                     patchwork::area(t = 5, b = 5, l = 2, r = 2),
                     patchwork::area(t = 2, b = 5, l = 8, r = 10))

    out <- map + legend + scatter + plot_layout(design = layout_inset)
    
  } else {
    
    labels <- sprintf(
      "<strong>Country: %s</strong><br/> %s: %0.2f (%0.2f percentile) <br/> %s: %0.2f (%0.2f percentile)",
      sf_obj$name_0, xlab, sf_obj$x, sf_obj$x_perc*100, ylab, sf_obj$y, sf_obj$y_perc*100 
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
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = sf_obj, color = "#444444", weight = 2, fill = FALSE) -> map
    
    plot_ly(data = bivar_df, x = ~x, y = ~ y, text = ~loc_labs) %>% 
      add_markers(marker = list(color = ~color), size = 2,
                  hovertemplate = glue("%{text} <br>{{xlab}: %{x} <br> {{ylab}: %{y}", 
                                       .open = "{{")) %>%
      layout(xaxis = list(title = paste0(xlab)), yaxis = list(title = paste0(ylab))) -> scatter

    ggplotly(legend) %>% hide_legend() %>% style(hoverinfo = "skip") -> legend
    
    out <- list(legend = legend, scatter = scatter, map = map)
  }
  return(out)
}

#' Continuous color palette
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

get_color_continuous <- function(x_perc, y_perc) {
  
    alpha <- 0.5*max(x_perc, y_perc) + 0.5*x_perc*y_perc
    
    if(x_perc >= y_perc) col.val <- colorRamp(c("red2", "purple3"))(y_perc / x_perc)
    if(x_perc < y_perc)  col.val <- colorRamp(c("mediumblue", "purple3"))(x_perc / y_perc) 
    if(x_perc == 0 & y_perc == 0) col.val <- rep(255, 3)
    
    rgb(col.val[1], col.val[2], col.val[3], alpha = alpha*255, maxColorValue = 255)
}
