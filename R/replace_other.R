#' "replace_other"
#' 
#' @description This function replaces a specific group in a data frame with another group.
#'
#' @param data The data frame in which the replacement will be performed. By default, it assumes a data frame called `individuals`.
#' @param group The name of the group variable to be replaced. This parameter is required.
#' @param group_other The name of the replacement group variable. It is constructed by appending the prefix "autre_" to the original group name. For example, if `group` is "pays", then `group_other` becomes "autre_pays".
#' @param group_other_name The specific value within the `group` variable that should be replaced. When this value is encountered in the `group` variable, it will be replaced with the corresponding value from `group_other`. This parameter is required.
#' @param RAS  A logical value indicating whether the replacement should be performed with "RAS" when the `group_other_name` is encountered. If `RAS` is set to `TRUE`, the replacement will be "RAS"; otherwise, it will be the corresponding value from `group_other`. By default, it is set to `FALSE`.
#'
#'
#' @note The `mutate` function is used to create a new column in the data frame named `group_var`, which replaces the values based on the provided conditions. When `RAS` is set to `TRUE`, it checks if the value in the `group` variable is equal to `group_other_name`. If it is, the corresponding value from `group_other` is assigned to `group_var`. Additionally, if the value in the `group` variable is `NA` (missing), it is replaced with "RAS". If `RAS` is set to `FALSE`, the replacement is performed without using "RAS".
#'
#' @return Returns data
#' @export
#'
#' @examples # TODO


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
      
      #create a new column in the data frame named "group_var", which replaces the values based on the provided conditions. 
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