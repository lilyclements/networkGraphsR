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
      mutate(CCRP = case_when(
        `donateur/ccrp_mcknight_foundation` == 1 ~ group, 
        TRUE   ~ "autre"))
  }
  if (node_size == "age"){
    init <- init %>% mutate(age = 2023 - date_creation)
  }
  init.by <- init %>% 
    #  pivot_wider(names_from = Pays_Autre_nom, values_from = Pays_Autre, names_prefix = "Pays_") %>%
    #  select(!c(Pays_)) %>% 
    pivot_longer(cols = starts_with(paste0(by, "/")), names_to = "id_by") %>% 
    mutate(id_by = gsub("^.*?/","", id_by)) %>%
    mutate(id_by = gsub(paste0(by, "/"), "", id_by)) %>%
    filter(value == 1)
  
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
    forceNetwork(Links = links_init_by, Nodes = nodes_init_by,
                 Source = "source", Target = "target",
                 NodeID = "id", Nodesize = "type_weight",
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
  } else {
    forceNetwork(Links = links_init_by, Nodes = nodes_init_by, 
                 Source = "source", Target = "target", 
                 NodeID = "id", Nodesize = "type_weight",
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
    mutate(id_ind = gsub("^.*?/","", {{ ind_id }}))
  
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
    pivot_longer(cols = starts_with("institutions_associees/"), names_to = "id_inst") %>% 
    mutate(id_inst = gsub("^.*?/", "", id_inst)) %>%
    mutate(id_inst = gsub("institutions_associees/","", id_inst)) %>%
    filter(value == 1) %>%
    select(!value) %>% 
    # Create a variable giving the initiative
    pivot_longer(cols = starts_with("initiatives_associees/"), names_to = "id_init") %>% 
    mutate(id_init = gsub("^.*?/","", id_init)) %>%
    mutate(id_init = gsub("initiatives_associees/","", id_init)) %>%
    filter(value == 1) %>%
    select(!value)%>%
    # combine the initiative and institution column into one
    pivot_longer(cols = c(id_inst,id_init), 
                 names_to = "inst_init_type", values_to = "id_inst_init") %>% 
    mutate(inst_init_type = case_when(
      inst_init_type == 'id_inst' ~ "Institution",
      inst_init_type == 'id_init' ~ "Initiative")) 
  
  # Split the data `ind.init_inst` to just Institutions
  inst.ind <- ind.init_inst %>%
    filter(inst_init_type == "Institution") %>%
    distinct(id_inst_init) %>%
    rename(id_inst = id_inst_init)
  
  # Split the data `ind.init_inst` to just Initatives
  init.ind <- ind.init_inst %>%
    filter(inst_init_type == "Initiative") %>% 
    distinct(id_inst_init) %>%
    rename(id_init = id_inst_init)
  
  # Creating the node data: bind our three data sets together
  # (individual, inst.ind, init.ind)
  # Add a column to state if it is individual, institution, or initiative data
  nodes_init_by <- bind_rows (
    "Individu(e)" = rename(ind, id=id_ind), 
    "Institution" = rename(inst.ind, id=id_inst), 
    "Initiative" = rename(init.ind, id=id_init),
    .id = "type") %>%
    # add a weight
    mutate(type_weight = case_when(
      type == 'Individu(e)' ~ 1,
      type == 'Institution' ~ 2,
      type == 'Initiative' ~ 3))
  
  # Create notes and links data
  # if there is a group variable, then create a "group_type" variable
  if (!is.null(group)){
    nodes_init_by <- nodes_init_by %>%
      mutate(group_type = case_when(
        type == 'Individu(e)' ~ .data[[group]],
        type == 'Institution' ~ "Institution",
        type == 'Initiative' ~ "Initiative")) %>%
      mutate(id_index = row_number() - 1) %>%
      select(c("id", "id_index", "type_weight", "group_type"))
  } else {
    nodes_init_by <- nodes_init_by %>%
      mutate(id_index = row_number()-1) %>%
      select(c("id", "id_index", "type", "type_weight"))
  }
  
  # Merge the id_index column into the ind.init_inst data by initiative
  # set this id_index column to be "target"
  links_init_by <- ind.init_inst %>%
    left_join(y=rename(select(nodes_init_by, id, id_index), 
                       target=id_index), 
              by = c("id_inst_init"="id"))
  # Now merge in the id_index column into the ind.init_inst data by individual
  # set this id_index column to be "source"
  links_init_by <- links_init_by %>%
    left_join(y=rename(select(nodes_init_by, id, id_index), 
                       source=id_index), 
              by = c("id_ind"="id"))
  # Add a numerical weight to whether it is institution or initiative
  links_init_by <- links_init_by %>%
    mutate(inst_init_type_weight = case_when(
      inst_init_type == "Institution" ~ '2',
      inst_init_type == "Initiative" ~ '1')) %>%
    select(c(target, source, inst_init_type, inst_init_type_weight))
  
  if (display_labels) {
    display_labels = 1
  } else {
    display_labels = 0
  }
  
  if (is.null(group)){
    forceNetwork(Links = links_init_by,
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
  } else {
    forceNetwork(Links = links_init_by,
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
replace_other <- function(data = individuals, group = NULL, group_other = paste0("autre_", group), group_other_name = NULL, RAS = FALSE){
  group_var <- group
  # if (group_other == "autre_pays"){
  #   data <- data %>% 
  #     mutate(!!group_var := case_when(
  #       .data[[group]] == group_other_name ~ case_when(
  #         is.na(.data[[group_other]]) ~ "autre",
  #         TRUE ~ .data[[group_other]]),
  #       is.na(.data[[group]]) ~ 'RAS',
  #       TRUE ~ .data[[group]]))
  # } else {
  if (RAS){
    data <- data %>% 
      mutate(!!group_var := case_when(
        .data[[group]] == group_other_name ~ .data[[group_other]], 
        is.na(.data[[group]]) ~ "RAS",
        TRUE ~ .data[[group]])) 
  } else {
    data <- data %>% 
      mutate(!!group_var := case_when(
        .data[[group]] == group_other_name ~ .data[[group_other]], 
        TRUE ~ .data[[group]]))
  }
  # }
  return(data)
}

# when
# variable (group) == "autre" ~ (variable group col name),
#   is.na(group) ,
#   TRUE ~ groip