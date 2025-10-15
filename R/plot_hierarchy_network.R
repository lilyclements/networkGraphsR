#' Plot a Hierarchical Network
#'
#' @description
#' Creates an interactive network visualization of hierarchical relationships 
#' (e.g., parent–child links) within a dataset using 
#' **\{networkD3\}**'s `forceNetwork()` function.
#'
#' The function identifies parent–child connections, constructs node and link 
#' indices, and plots a force-directed network where each node represents an 
#' entity (such as an initiative or activity), and links show hierarchical 
#' relationships.
#'
#' @param data A data frame containing identifiers and relationship columns.
#' @param id_col The column in `data` containing unique identifiers for each node. 
#'   Defaults to `"__id"`.
#' @param parent_col The column in `data` specifying each item's parent identifier. 
#'   Defaults to `"parent_id"`.
#' @param child_col The column in `data` specifying each item's child identifier. 
#'   Defaults to `"child_id"`.
#' @param group_col A column used to group or color nodes in the network 
#'   (e.g., by responsible person, or tags). Defaults to `"group"`.
#' @param ... Additional arguments passed to `networkD3::forceNetwork()`, 
#'   such as `legend`, `bounded`, or `fontSize`.
#'
#' @details
#' This function first extracts parent–child pairs from the specified columns, 
#' builds a list of all unique nodes, assigns each a numeric index, and then 
#' joins those indices back to create the link table required by 
#' `networkD3::forceNetwork()`.  
#' 
#' Each node is associated with a group label (if provided), which controls 
#' its color in the resulting visualization.
#'
#' @return
#' An interactive `htmlwidget` object produced by `networkD3::forceNetwork()`, 
#' which can be viewed directly in RStudio, a browser, or embedded in 
#' R Markdown or Shiny applications.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' df <- data.frame(
#'   id = c("A", "B", "C", "D"),
#'   parent_id = c(NA, "A", "A", "B"),
#'   child_id = c("B", "C", "D", NA),
#'   group = c("Team 1", "Team 1", "Team 2", "Team 2")
#' )
#'
#' # Plot the hierarchy network
#' plot_hierarchy_network(
#'   data = df,
#'   id_col = "id",
#'   parent_col = "parent_id",
#'   child_col = "child_id",
#'   group_col = "group",
#'   legend = TRUE,
#'   fontSize = 16
#' )
#' }
#'
#' @seealso [networkD3::forceNetwork()]
#' @export
plot_hierarchy_network <- function(
  data,
  id_col = "__id",
  parent_col = "parent_id",
  child_col = "child_id",
  group_col = "group", ...
) {
  df <- data %>%
    dplyr::select(
      ID = {{ id_col }},
      Parent = {{ parent_col }},
      Child = {{ child_col }},
      Group = {{ group_col }}
    )

  # Extract parent and child relationships
  df_parent <- df %>%
    dplyr::select(ID, Parent) %>%
    dplyr::rename(Child = ID) %>%
    dplyr::filter(!is.na(Parent)) %>%
    dplyr::distinct()

  df_child <- df %>%
    dplyr::select(ID, Child) %>%
    dplyr::rename(Parent = ID) %>%
    dplyr::filter(!is.na(Child)) %>%
    dplyr::distinct()

  df_all <- dplyr::full_join(df_parent, df_child)

  # Create node index
  id_index_data <- df_all %>%
    tidyr::pivot_longer(cols = c(Child, Parent)) %>%
    dplyr::select(-name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(id_index = dplyr::row_number() - 1)

  # Create link indices
  df_links_data <- df_all %>%
    dplyr::left_join(id_index_data, by = c("Parent" = "value")) %>%
    dplyr::rename(parent_index = id_index) %>%
    dplyr::left_join(id_index_data, by = c("Child" = "value")) %>%
    dplyr::rename(child_index = id_index)

  # Add group info to nodes
  id_index_data <- id_index_data %>%
    dplyr::left_join(df %>% dplyr::select(ID, Group), by = c("value" = "ID")) %>%
    dplyr::distinct()

  # Plot network
  networkD3::forceNetwork(
    Links = df_links_data,
    Nodes = id_index_data,
    Source = "parent_index",
    Target = "child_index",
    NodeID = "value",
    Group = "Group",
    ...
  )
}