# Read in the Data
initiatives <- readxl::read_excel("data/IDEMS_database_example.xlsx",
      sheet = "initatives")

people_data <- read_excel("data/IDEMS_database_example.xlsx", 
    sheet = "people") %>%
  dplyr::select(c(`__id`, name_id = `label`))

## Replace strings with values
# create a named vector: names = IDs, values = labels
lookup_persons_data <- setNames(people_data$name_id,
  people_data$`__id`)

initiatives_data_long <- initiatives %>%
  mutate(parent_initiative = strsplit(parent_initiative, " ")) %>%
  unnest(parent_initiative)
initiatives_data_long <- initiatives_data_long %>%
  mutate(initiatives_children = strsplit(initiatives_children, " ")) %>%
  unnest(initiatives_children)

# rename the initiatives from the initiatives data
initiatives_data <- initiatives %>%
  dplyr::select(c("__id", "label"))

# create a named vector: names = IDs, values = labels
lookup <- setNames(initiatives_data$label,
  initiatives_data$`__id`)

# replace using the lookup
initiatives_data_long$initiatives_children <- lookup[as.character(initiatives_data_long$initiatives_children)]
initiatives_data_long$parent_initiative <- lookup[as.character(initiatives_data_long$parent_initiative)]
initiatives_data_long$`__id` <- lookup[as.character(initiatives_data_long$`__id`)]
initiatives_data_long$`__id` <- stringr::str_replace_all(initiatives_data_long$`__id`, " ", "_")
initiatives_data_long$parent_initiative <- stringr::str_replace_all(initiatives_data_long$parent_initiative, " ", "_")
initiatives_data_long$initiatives_children <- stringr::str_replace_all(initiatives_data_long$initiatives_children, " ", "_")
initiatives_data_long$resp_person <- lookup_persons_data[as.character(initiatives_data_long$resp_person)]

df <- initiatives_data_long %>% dplyr::select(c(ID = `__id`,
                                                Parent = `parent_initiative`,
                                                Child = `initiatives_children`,
                                                resp_person))

df_parent <- df %>% select(c(ID, Parent)) %>%
  rename(Child = ID) %>%
  filter(!is.na(Parent)) %>%
  distinct()

df_child <- df %>% select(c(ID, Child)) %>%
  rename(Parent = ID) %>%
  filter(!is.na(Child)) %>%
  distinct()

df_all <- full_join(df_parent, df_child)

id_index_data <- df_all %>%
  pivot_longer(cols = c(Child, Parent)) %>%
  dplyr::select(-c("name")) %>%
  distinct() %>%
  mutate(id_index = row_number() - 1) 

df_all_1 <- df_all %>%
  left_join(id_index_data, by = c("Parent" = "value")) %>%
  rename(parent_index = id_index) %>%
  left_join(id_index_data, by = c("Child" = "value")) %>%
  rename(child_index = id_index)

id_index_data <- id_index_data %>%
  left_join(df %>% dplyr::select("ID", "resp_person"), by = c("value" = "ID")) %>%
  distinct()

networkD3::forceNetwork(Links = df_all_1, Nodes = id_index_data,
                      Source = "parent_index", Target = "child_index",
                      NodeID = "value",
                      Group = "resp_person",
                      legend = TRUE,
                      bounded = TRUE,
fontSize = 20)
