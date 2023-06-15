#' "Initiative_by"
#' 
#' @description This function is a wrapper function for `networkD3::forceNetwork`.
#' explain what it does - 
#'
#' @param initiative_data a data frame object containing the initiative data
#' @param by the values around which nodes are formed
#' @param filter_var filtered variable
#' @param filter_vals values associated to the filter
#' @param node_size character string specifying the a column in the `initiative_data` data frame with some value to vary the node radius's with. See also \code{radiusCalculation}.
#' @param group character string specifying the group of each node in the `initiative_data` data frame.
#' @param font_size numeric font size in pixels for the node text labels.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param colour_scale character string specifying the categorical colour scale for the nodes. See https://github.com/d3/d3/blob/master/API.md#ordinal-scales.
#' @param font_family font family for the node text labels.
#' @param link_distance numeric or character string. Either numberic fixed distance between the links in pixels (actually arbitrary relative to the diagram's size). Or a JavaScript function, possibly to weight by Value. For example: linkDistance = JS("function(d){return d.value * 10}").
#' @param link_width numeric or character string. Can be a numeric fixed width in pixels (arbitrary relative to the diagram's size). Or a JavaScript function, possibly to weight by Value. The default is linkWidth = JS("function(d) { return Math.sqrt(d.value); }").
#' @param radius_calculation character string. A javascript mathematical expression, to weight the radius by Nodesize. The default value is radiusCalculation = JS("Math.sqrt(d.nodesize)+6").
#' @param charge numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value).
#' @param link_colour character vector specifying the colour(s) you want the link lines to be. Multiple formats supported (e.g. hexadecimal).
#' @param opacity numeric value of the proportion opaque you would like the graph elements to be.
#' @param zoom logical value to enable (TRUE) or disable (FALSE) zooming.
#' @param arrows logical value to enable directional link arrows.
#' @param bounded logical value to enable (TRUE) or disable (FALSE) the bounding box limiting the graph's extent. See http://bl.ocks.org/mbostock/1129492.
#' @param display_labels 
#' @param click_action character string with a JavaScript expression to evaluate when a node is clicked.
#'
#' @return Returns a network graph object
#' @export
#'
#' @examples # todo
initiative_by <- function(initiative_data, by = "pays", sep = ".", filter_var = NULL, filter_vals = NULL,
                          node_size = c("type", "age"), group = NULL,
                          font_size = 7, height = NULL, width = NULL, colour_scale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                          font_family = "serif", link_distance = 50, link_width = JS("function(d) { return Math.sqrt(d.value); }"),
                          radius_calculation = "4*Math.sqrt(d.nodesize)+2", charge = -30,
                          link_colour = "#666", opacity = 0.6, zoom = FALSE, arrows = FALSE,
                          bounded = FALSE, display_labels = 0, click_action = NULL){
  # characteristics of nodes
  node_size <- match.arg(node_size)
  #  if (!is.null(filter_var)){
  #    initiative_data <- initiative_data %>%
  #      filter(.data[[filter_var]] %in% filter_vals) 
  #  }
  # Replacement of the boxes in the "initiative_name" column with the associated values in the "other_initiative" column"
  init <- replace_other(data = initiative_data, group = "nom_initiative", group_other = "autre_initiative", group_other_name = "autre_initiative")
  # Rename the column "initiative_name" by id_init
  init <- dplyr::rename(init, id_init = nom_initiative)
  # creation of the CCRP column for CCRP donors 
  if (!is.null(group)){
    init <- init %>%
      dplyr::mutate(CCRP = case_when(
        paste0("donateur", sep, "ccrp_mcknight_foundation") == 1 ~ group, 
        TRUE   ~ "autre"))
  }
  # Creation of age column
  if (node_size == "age"){
    init <- init %>% dplyr::mutate(age = 2023 - date_creation)
  }
  # pivoting and filtering of data 
  init.by <- init %>% 
    #  pivot_wider(names_from = Pays_Autre_nom, values_from = Pays_Autre, names_prefix = "Pays_") %>%
    #  select(!c(Pays_)) %>% 
    tidyr::pivot_longer(cols = starts_with(paste0(by, sep)), names_to = "id_by")%>% 
    dplyr::mutate(id_by = gsub("^.*?/","", id_by)) %>%
    dplyr::mutate(id_by = gsub(paste0(by, sep), "", id_by))%>%
    dplyr::filter(value == 1)
  # Elimination of repetitions.
  by.init <- init.by %>% 
    distinct(id_by)
  
  # Create notes and links data ------------------------------------------------
  by_var <- by
  print("A")
  
  nodes_init_by <- bind_rows (
    "Initiative" = rename(init, id = id_init),
    !!by_var := rename(by.init, id = id_by), 
    .id = "type")
  
  if (node_size == "type"){
    nodes_init_by <- nodes_init_by %>%
      mutate(type_weight = case_when(
        type == by ~ 1,
        type == 'Initiative' ~ 2))
  } else {
    nodes_init_by <- nodes_init_by %>%
      dplyr::mutate(type_weight = case_when(
        type == by ~ 0,
        type == 'Initiative' ~ age))
  }
  if (!is.null(group)){
    nodes_init_by <- nodes_init_by %>%
      dplyr::mutate(group_type = case_when(
        type == by ~ by,
        type == 'Initiative' ~ .data[[group]])) %>% # replace CCRP with {{ group }}?
      dplyr::mutate(id_index = row_number()-1) %>%
      select(id, id_index, type, type_weight, group_type) %>%
      dplyr::mutate(group_type = replace_na(group_type, "Unknown"))
  } else {
    nodes_init_by <- nodes_init_by %>%
      dplyr::mutate(id_index = row_number()-1) %>%
      select(id, id_index, type, type_weight)
  }
  
  links_init_by <- init.by %>%
    left_join(y=dplyr::rename(select(nodes_init_by, id, id_index), 
                              target=id_index), 
              by = c("id_by"="id")) %>%
    left_join(y=dplyr::rename(select(nodes_init_by, id, id_index), 
                              source=id_index), 
              by = c("id_init"="id")) %>% 
    select(c(target, source))
  
  if (display_labels) {
    display_labels = 1
  } else {
    display_labels = 0
  }
  
  # Nodesize = type_weight, age_weight
  if (is.null(group)){
    group_type = "type"
  } else {
    group_type = "group_type"
  }
  forceNetwork(Links = links_init_by, Nodes = nodes_init_by,
               Source = "source", Target = "target",
               NodeID = "id", Nodesize = "type_weight",
               Group = group_type,
               legend = TRUE,
               fontSize = font_size,
               height = height, width = width,
               colourScale = colour_scale, fontFamily = font_family,
               linkDistance = link_distance, linkWidth = link_width,
               radiusCalculation = radius_calculation, charge = charge,
               linkColour = link_colour, opacity = opacity, zoom = zoom,
               arrows = arrows, bounded = bounded, opacityNoHover = display_labels,
               clickAction = click_action)
}