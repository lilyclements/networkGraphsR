#' "Initiative_by"
#' 
#' @description This function is a wrapper function for `networkD3::forceNetwork`.
#' explain what it does - 
#'
#' @param initiative_data a data frame object containing the initiative data
#' @param by 
#' @param filter_var 
#' @param filter_vals 
#' @param node_size character string specifying the a column in the `initiative_data` data frame with some value to vary the node radius's with. See also \code{radiusCalculation}.
#' @param group character string specifying the group of each node in the `initiative_data` data frame.
#' @param font_size 
#' @param height 
#' @param width 
#' @param colour_scale 
#' @param font_family 
#' @param link_distance 
#' @param link_width 
#' @param radius_calculation 
#' @param charge 
#' @param link_colour 
#' @param opacity 
#' @param zoom 
#' @param arrows 
#' @param bounded 
#' @param display_labels 
#' @param click_action 
#'
#' @return Returns a network graph object
#' @export
#'
#' @examples # todo
initiative_by <- function(initiative_data, by = "pays", filter_var = NULL, filter_vals = NULL,
                          node_size = c("type", "age"), group = NULL,
                          font_size = 7, height = NULL, width = NULL, colour_scale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                          font_family = "serif", link_distance = 50, link_width = JS("function(d) { return Math.sqrt(d.value); }"),
                          radius_calculation = "4*Math.sqrt(d.nodesize)+2", charge = -30,
                          link_colour = "#666", opacity = 0.6, zoom = FALSE, arrows = FALSE,
                          bounded = FALSE, display_labels = 0, click_action = NULL){
  node_size <- match.arg(node_size)
  #  if (!is.null(filter_var)){
  #    initiative_data <- initiative_data %>%
  #      filter(.data[[filter_var]] %in% filter_vals) 
  #  }
  init <- replace_other(data = initiative_data, group = "nom_initiative", group_other = "autre_initiative", group_other_name = "autre_initiative")
  init <- rename(init, id_init = nom_initiative)
  if (!is.null(group)){
    init <- init %>%
      dplyr::mutate(CCRP = case_when(
        `donateur/ccrp_mcknight_foundation` == 1 ~ group, 
        TRUE   ~ "autre"))
  }
  if (node_size == "age"){
    init <- init %>% dplyr::mutate(age = 2023 - date_creation)
  }
  init.by <- init %>% 
    #  pivot_wider(names_from = Pays_Autre_nom, values_from = Pays_Autre, names_prefix = "Pays_") %>%
    #  select(!c(Pays_)) %>% 
    tidyr::pivot_longer(cols = starts_with(paste0(by, "/")), names_to = "id_by") %>% 
    dplyr::mutate(id_by = gsub("^.*?/","", id_by)) %>%
    dplyr::mutate(id_by = gsub(paste0(by, "/"), "", id_by)) %>%
    dplyr::filter(value == 1)
  
  by.init <- init.by %>% 
    distinct(id_by)
  
  # Create notes and links data ------------------------------------------------
  by_var <- by
  
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
      mutate(type_weight = case_when(
        type == by ~ 0,
        type == 'Initiative' ~ age))
  }
  if (!is.null(group)){
    nodes_init_by <- nodes_init_by %>%
      mutate(group_type = case_when(
        type == by ~ by,
        type == 'Initiative' ~ .data[[group]])) %>% # replace CCRP with {{ group }}?
      mutate(id_index = row_number()-1) %>%
      select(id, id_index, type, type_weight, group_type) %>%
      mutate(group_type = replace_na(group_type, "Unknown"))
  } else {
    nodes_init_by <- nodes_init_by %>%
      mutate(id_index = row_number()-1) %>%
      select(id, id_index, type, type_weight)
  }
  
  links_init_by <- init.by %>%
    left_join(y=rename(select(nodes_init_by, id, id_index), 
                       target=id_index), 
              by = c("id_by"="id")) %>%
    left_join(y=rename(select(nodes_init_by, id, id_index), 
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