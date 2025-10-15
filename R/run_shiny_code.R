# ---- Credentials (use keyring or env vars) ----
stored_password <- read.table("pass.txt")[1,]

# ---- Credentials ----
user <- "lily@idems.international"

## UI -----------------------------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Hierarchy Network"),
  shinydashboard::dashboardSidebar(
    shinyjs::useShinyjs(),
    shiny::br(),
    shiny::helpText("Reads data/IDEMS_database_example.xlsx"),
    shiny::selectInput(
      "personFilter",
      "Filter by responsible person:",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::numericInput("fontSize", "Font size", value = 16, min = 8, max = 40, step = 1),
    shiny::checkboxInput("legend", "Show legend", value = TRUE),
    shiny::checkboxInput("bounded", "Bounded layout", value = TRUE),
    shiny::actionButton("refresh", "Refresh")
  ),
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shinydashboard::box(width = 12, title = "Network", solidHeader = TRUE, status = "primary",
          networkD3::forceNetworkOutput("net", height = "650px"))
    )
  )
)

# --- Server ---------------------------------------------------------------
server <- function(input, output, session) {

  raw_data <- shiny::reactive({
  # Initiatives
  ruODK::ru_setup(
    svc = "https://odk.idems.international/v1/projects/13/forms/idems_activities_create_initiative.svc",
    un  = "lily@idems.international",
    pw  = stored_password
  )
  initiatives <- ruODK::odata_submission_get()

  # Initiatives Entities
  svc_initiatives_dataset    <- "https://odk.idems.international/v1/projects/13/datasets/initiatives.svc"  # <-- from Datasets > People > OData

  # 2) People (dataset entities) -> table "Entities" has entity __id and label
  entities_url <- paste0(svc_initiatives_dataset, "/Entities?$format=application/json")
  initiatives_entities <- request(entities_url) |>
    req_auth_basic(user, stored_password) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  initiatives_entities <- tibble::as_tibble(initiatives_entities$value)
    
  # People
  svc_people_dataset    <- "https://odk.idems.international/v1/projects/13/datasets/people.svc"  # <-- from Datasets > People > OData

  # 2) People (dataset entities) -> table "Entities" has entity __id and label
  entities_url <- paste0(svc_people_dataset, "/Entities?$format=application/json")
  people_entities <- request(entities_url) |>
    req_auth_basic(user, stored_password) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  people_entities <- tibble::as_tibble(people_entities$value)
    
  # Normalise UUIDs
  initiatives$id <- stringr::str_remove(initiatives$id, "^uuid:")

  list(initiatives = initiatives, initiatives_entities = initiatives_entities, people = people_entities)
  })

  initiatives_long <- shiny::reactive({
    req(raw_data())
    initiatives <- raw_data()$initiatives
    initiatives_entities <- raw_data()$initiatives_entities
    people_data <- raw_data()$people

    df <- initiatives %>%
      dplyr::mutate(parent_initiative = strsplit(parent_initiative, " ")) %>%
      tidyr::unnest(parent_initiative) %>%
      dplyr::mutate(initiatives_children = strsplit(initiatives_children, " ")) %>%
      tidyr::unnest(initiatives_children)

    df <- df %>%
      dplyr::mutate(
        resp_person = replace_ids_with_labels(people_data, "__id", "label", df, "resp_person"),
        initiatives_children = replace_ids_with_labels(initiatives_entities, "__id", "label", df, "initiatives_children"),
        parent_initiative    = replace_ids_with_labels(initiatives_entities, "__id", "label", df, "parent_initiative")
      )
    
    df
  })

  # --- Populate filter choices dynamically ---
  shiny::observe({
    df <- initiatives_long()
    choices <- sort(unique(df$resp_person))
    shiny::updateSelectInput(session, "personFilter", choices = choices, selected = choices)
  })

  # --- Filtered dataset ---
  filtered_data <- shiny::reactive({
    df <- initiatives_long()
    if (!is.null(input$personFilter) && length(input$personFilter) > 0) {
      df <- df %>% dplyr::filter(resp_person %in% input$personFilter)
    }
    df
  })

  # --- Plot network ---
  output$net <- networkD3::renderForceNetwork({
    df <- filtered_data()
    req(nrow(df) > 0)

    plot_hierarchy_network(
      data = df,
      id_col = "name",
      parent_col = "parent_initiative",
      child_col = "initiatives_children",
      group_col = "resp_person",
      legend = input$legend,
      bounded = input$bounded,
      fontSize = input$fontSize
    )
  })
}

shiny::shinyApp(ui, server)