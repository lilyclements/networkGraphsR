#' Replace IDs with Descriptive Labels
#'
#' @description
#' Replaces identifier codes (e.g., numeric or string IDs) in a target dataset
#' with corresponding descriptive labels (e.g., names) from a lookup table.  
#' 
#' This is useful for making datasets more readable, especially when working
#' with hierarchical or relational data where links are defined by IDs.
#'
#' @param lookup_table A data frame containing the mapping between IDs and labels.
#' @param id_col The column name in `lookup_table` that contains the unique IDs.
#'   Defaults to `"__id"`.
#' @param label_col The column name in `lookup_table` that contains the
#'   corresponding descriptive labels. Defaults to `"name_id"`.
#' @param target_data The data frame containing the column whose values
#'   should be replaced.
#' @param target_col The name of the column in `target_data` where ID values
#'   will be replaced by their corresponding labels. Defaults to `"resp_person"`.
#'
#' @details
#' The function creates a named lookup vector from the `lookup_table`, where
#' names are IDs and values are labels. It then replaces each value in
#' `target_data[[target_col]]` with the matching label.
#' 
#' Any IDs not found in the lookup table will return `NA`. To retain unmatched
#' IDs, additional NA handling can be added.
#'
#' @return
#' A character vector of labels corresponding to the values in
#' `target_data[[target_col]]`.
#'
#' @examples
#' # Example lookup and target data
#' people <- data.frame(
#'   id = c(1, 2, 3),
#'   name_id = c("Alice", "Bob", "Charlie")
#' )
#'
#' initiatives <- data.frame(
#'   resp_person = c(1, 3, 2, 1)
#' )
#'
#' # Replace IDs with names
#' replace_ids_with_labels(
#'   lookup_table = people,
#'   id_col = "id",
#'   label_col = "name_id",
#'   target_data = initiatives,
#'   target_col = "resp_person"
#' )
#'
#' @export
replace_ids_with_labels <- function(
  lookup_table,
  id_col = "__id",
  label_col = "name_id",
  target_data,
  target_col = "resp_person"
) {
  # create lookup vector: names = IDs, values = labels
  lookup_vector <- setNames(lookup_table[[label_col]], lookup_table[[id_col]])
  
  # replace IDs in target column with labels
  new_value <- lookup_vector[as.character(target_data[[target_col]])]
  
  return(new_value)
}