#' "individuals_by"
#' 
#' @description  individuals_by function creates a force-directed network visualization using the forceNetwork function from the networkD3 package. 
#'
#' @param individual_data The data frame containing the individual-level data. By default, it assumes a data frame called `individuals`.
#' @param ind_id The variable/column in the `individual_data` data frame that represents the individual identifier. By default, it assumes a column named "nom".
#' @param group The variable/column in the `individual_data` data frame that represents the grouping variable. It is used to assign colors to the nodes in the visualization. If not provided, the visualization will not group the nodes.
#' @param group_other is a string representing the name of the column in the "individual_data" data block that contains additional group information for each individual.
#' @param group_other_name is a string representing a custom name for the "group_other" column to display in the plot.
#' @param font_size numeric font size in pixels for the node text labels.By default, it is set to 7.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param colour_scale character string specifying the categorical colour scale for the nodes. See \code{https://github.com/d3/d3/blob/master/API.md#ordinal-scales}.
#' @param font_family font family for the node text labels.
#' @param link_distance numeric or character string. Either numberic fixed distance between the links in pixels (actually arbitrary relative to the diagram's size). Or a JavaScript function, possibly to weight by Value. For example: linkDistance = JS("function(d){return d.value * 10}").
#' @param link_width numeric or character string. Can be a numeric fixed width in pixels (arbitrary relative to the diagram's size). Or a JavaScript function, possibly to weight by Value. The default is linkWidth = JS("function(d) { return Math.sqrt(d.value); }").
#' @param radius_calculation character string. A javascript mathematical expression, to weight the radius by Nodesize. The default value is radiusCalculation = JS("Math.sqrt(d.nodesize)+6").
#' @param charge numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value).
#' @param link_colour character vector specifying the colour(s) you want the link lines to be. Multiple formats supported (e.g. hexadecimal).
#' @param opacity numeric value of the proportion opaque you would like the graph elements to be.
#' @param zoom logical value to enable (TRUE) or disable (FALSE) zooming.
#' @param arrows 	logical value to enable directional link arrows. 
#' @param bounded logical value to enable (TRUE) or disable (FALSE) the bounding box limiting the graph's extent. See \code{http://bl.ocks.org/mbostock/1129492}.
#' @param display_labels is a numeric value representing the number of characters of the label to display on each node.
#' @param click_action 	character string with a JavaScript expression to evaluate when a node is clicked.
#'
#' @return Returns a network graph object
#' @export
#'
#' @examples # TODO
individuals_by <- function(individual_data = individuals, ind_id = nom, group = NULL, group_other = NULL, group_other_name = NULL,
                           font_size = 7, height = NULL, width = NULL, colour_scale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                           font_family = "serif", link_distance = 50, link_width = JS("function(d) { return Math.sqrt(d.value); }"),
                           radius_calculation = "4*Math.sqrt(d.nodesize)+2", charge = -30,
                           link_colour = "#666", opacity = 0.6, zoom = FALSE, arrows = FALSE,
                           bounded = FALSE, display_labels = 0, click_action = NULL){
  
  #  if (!is.null(filter_var)){
  #    individual_data <- individual_data %>%
  #      filter(.data[[filter_var]] %in% filter_vals) 
  #  }
  
  # ind data
  ind <- individual_data %>%
    dplyr::mutate(id_ind = gsub("^.*?/","", {{ ind_id }}))
  
  # put this if statement elsewhere - own function
  # replace "other" columns with what the user selected:
  if (group %in% c("pays", "activite_prof")){
    if (group == "pays") {
      group_other_name = "autre_pays"
    } else if (group %in% c("activite_prof")){
      group_other_name = "autre"
    }
    ind <- replace_other(data = ind,
                         group = group,
                         group_other_name = group_other_name,
                         RAS = TRUE)
  }
  
  ind <- replace_other(data = ind,
                       group = "institutions_associees",
                       group_other_name = "autre")
  ind <- replace_other(data = ind,
                       group = "initiatives_associees",
                       group_other_name = "autre_initiative")
  
  # for prep fun
  ind.init_inst <- ind %>% 
    # Create a variable giving the institution
    tidyr::pivot_longer(cols = starts_with("institutions_associees/"), names_to = "id_inst") %>% 
    dplyr::mutate(id_inst = gsub("^.*?/", "", id_inst)) %>%
    dplyr::mutate(id_inst = gsub("institutions_associees/","", id_inst)) %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(!value) %>% 
    
    # Create a variable giving the initiative
    tidyr::pivot_longer(cols = starts_with("initiatives_associees/"), names_to = "id_init") %>% 
    dplyr::mutate(id_init = gsub("^.*?/","", id_init)) %>%
    dplyr::mutate(id_init = gsub("initiatives_associees/","", id_init)) %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(!value)%>%
    
    # combine the initiative and institution column into one
    tidyr::pivot_longer(cols = c(id_inst,id_init), 
                        names_to = "inst_init_type", values_to = "id_inst_init") %>% 
    dplyr::mutate(inst_init_type = case_when(
      inst_init_type == 'id_inst' ~ "Institution",
      inst_init_type == 'id_init' ~ "Initiative")) 
  
  # Split the data `ind.init_inst` to just Institutions
  inst.ind <- ind.init_inst %>%
    dplyr::filter(inst_init_type == "Institution") %>%
    dplyr::distinct(id_inst_init) %>%
    dplyr::rename(id_inst = id_inst_init)
  
  # Split the data `ind.init_inst` to just Initatives
  init.ind <- ind.init_inst %>%
    dplyr::filter(inst_init_type == "Initiative") %>% 
    dplyr::distinct(id_inst_init) %>%
    dplyr::rename(id_init = id_inst_init) # rename "id_init" by "id_inst_init"
  
  # Creating the node data: bind our three data sets together
  # (individual, inst.ind, init.ind)
  # Add a column to state if it is individual, institution, or initiative data
  nodes_init_by <- bind_rows (
    "Individu(e)" = rename(ind, id=id_ind), 
    "Institution" = rename(inst.ind, id=id_inst), 
    "Initiative" = rename(init.ind, id=id_init),
    .id = "type") %>%
    # add a weight
    tidyr::mutate(type_weight = case_when(
      type == 'Individu(e)' ~ 1,
      type == 'Institution' ~ 2,
      type == 'Initiative' ~ 3))
  
  # Create notes and links data
  # if there is a group variable, then create a "group_type" variable
  if (!is.null(group)){
    nodes_init_by <- nodes_init_by %>%
      dplyr::mutate(group_type = case_when(
        type == 'Individu(e)' ~ .data[[group]],
        type == 'Institution' ~ "Institution",
        type == 'Initiative' ~ "Initiative")) %>%
      dplyr::mutate(id_index = row_number() - 1) %>%
      dplyr::select(c("id", "id_index", "type_weight", "group_type"))
  } else {
    nodes_init_by <- nodes_init_by %>%
      dplyr::mutate(id_index = row_number()-1) %>%
      dplyr::select(c("id", "id_index", "type", "type_weight"))
  }
  
  # Merge the id_index column into the ind.init_inst data by initiative
  # set this id_index column to be "target"
  links_init_by <- ind.init_inst %>%
    dplyr::left_join(y=rename(select(nodes_init_by, id, id_index), 
                              target=id_index), 
                     by = c("id_inst_init"="id"))
  
  # Now merge in the id_index column into the ind.init_inst data by individual
  # set this id_index column to be "source"
  links_init_by <- links_init_by %>%
    dplyr::left_join(y=rename(select(nodes_init_by, id, id_index), 
                              source=id_index), 
                     by = c("id_ind"="id"))
  
  # Add a numerical weight to whether it is institution or initiative
  links_init_by <- links_init_by %>%
    dplyr::mutate(inst_init_type_weight = case_when(
      inst_init_type == "Institution" ~ '2',
      inst_init_type == "Initiative" ~ '1')) %>%
    dplyr::select(c(target, source, inst_init_type, inst_init_type_weight))
  
  if (display_labels) {
    display_labels = 1
  } else {
    display_labels = 0
  }
  
  if (is.null(group)){
    networkD3::forceNetwork(Links = links_init_by,
                            Nodes = nodes_init_by,
                            Source = "source",
                            Target = "target",
                            NodeID = "id",
                            Nodesize = "type_weight",
                            Group = "type",
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
  else {
    networkD3::forceNetwork(Links = links_init_by,
                            Nodes = nodes_init_by, 
                            Source = "source",
                            Target = "target", 
                            NodeID = "id",
                            Nodesize = "type_weight",
                            Group = "group_type",
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
}

