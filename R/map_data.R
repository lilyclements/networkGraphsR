#' Map Data for Network Visualisation
#'
#' This function takes a data frame and generates a network visualization using the \code{forceNetwork} function 
#' from the \code{networkD3} package. It separates and expands the data based on the specified values and node values,
#' and creates links between them.
#'
#' @param data The original data frame.
#' @param values The column name representing the primary values to be mapped.
#' @param node_values A character vector specifying the column names representing the node values.
#'
#' @return A network visualisation generated using the \code{forceNetwork} function.
#' @export
#'
#' @examples
#' # An example with simple data 
#' df_original <- data.frame(initiative = c("A", "B", "C", "D"),
#'                           country = c("BF", "BF Niger", "Niger", "Niger"),
#'                           ex = c("a b", "a b", "a", "b"))
#' map_data(df_original, "initiative", "country")
#' map_data(df_original, "initiative", "country", "ex")
#' map_data(df_original, "initiative", c("country", "ex"), "ex")
#' 
#' 
#' df_original2 <- data.frame(individuals = c("A", "B", "C", "D"),
#'                            institutions = c("a b", "a", "a", "b c"),
#'                            initiatives = c("z", "z", "y z", "y"))
#' map_data(df_original2, "individuals", c("institutions", "initiatives"))
#' map_data(df_original2, "individuals", c("institutions", "initiatives"), "institutions")
#' 
map_data <- function(data, values, node_values, colour_values = NULL) {
  # repeat for each in node_values because of different sizes in cols
  # separate_longer_delim doesn't repeat for each col, but groups
  for (i in node_values){
    data <- data %>%
      tidyr::separate_longer_delim(cols = {{ i }}, " ")
  }
  
  # Step 1: Expand data for df_output_1
  df_expanded <- data %>%
    tidyr::pivot_longer(cols = c({{ values }}, {{ node_values }}),
                 names_to = "group",
                 values_to = "id") %>%
    dplyr::select(c("id", "group")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(group)
  
  if (is.null(colour_values)){ 
    df_expanded <- df_expanded %>%
      dplyr::distinct() %>%
      dplyr::mutate(id_index = dplyr::row_number() - 1) %>%
      dplyr::rename(group_type = group) 
  } else {
    data_1 <- data %>%
      dplyr::select(c(id = {{ values }}, group_type = {{ colour_values }}))
    df_expanded <- df_expanded %>%
      dplyr::left_join(data_1, by = "id") %>%
      #rename(group_type = {{ colour_values }}) %>%
      dplyr::distinct() %>%
      dplyr::mutate(id_index = dplyr::row_number() - 1) %>%
      dplyr::mutate(group_type = ifelse(is.na(group_type), group, group_type))
  }

  # Step 2: Create df_output_2
  df_mapped <- data %>%
    dplyr::left_join(df_expanded, by = setNames("id", values)) %>%
    dplyr::rename(source = id_index) 
  
  df_mapped <- df_mapped %>%
    tidyr::pivot_longer(cols = all_of({{ node_values }}), values_to = "id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(df_expanded, by = "id") %>%
    dplyr::rename(target = id_index) %>%
    dplyr::select(c(target, source))
  
  return(networkD3::forceNetwork(Links = df_mapped, Nodes = df_expanded,
                      Source = "source", Target = "target",
                      NodeID = "id",
                      Group = "group_type",
                      legend = TRUE,
                    bounded = TRUE))
}
