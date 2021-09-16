box( # Project Timeline box open
  status = "primary",
  collapsible = FALSE,
  solidHeader = TRUE,
  title = "Project Timeline",
  width = 5,
  #height = "210px",
  column(
    width = 4,
    radioButtons(
      "status",
      label = "Current status of the project:",
      choices = c("Proposed","Ongoing","Completed")
    ),
    br()
  ),
  column(
    width = 8,
    fluidRow(
      column(
        width = 12,
        strong("Start Year and Month")
      )
    ),
    fluidRow( # start date row
      column(
        width = 4,
        selectInput("startYear",label = NULL, choices = 1900:2100, selected = 2020, selectize = FALSE)
      ),
      column(
        width = 4,
        selectInput("startMonth",label = NULL, choices = month.abb, selectize = FALSE)
      ),
      column(
        width = 4,
        checkboxInput("startUnknown",label = "Unknown", value = TRUE),
        bsTooltip("startUnknown","Please untick box to modify date", placement = "right")
      )
    ), # start date row close
    fluidRow(
      column(
        width = 12,
        strong("End Year and Month")
      )
    ),
    fluidRow( # end date row
      column(
        width = 4,
        selectInput("endYear",label = NULL, choices = 1900:2100, selected = 2020, selectize = FALSE)
      ),
      column(
        width = 4,
        selectInput("endMonth",label = NULL, choices = month.abb, selectize = FALSE)
      ),
      column(
        width = 4,
        checkboxInput("endUnknown",label = "Unknown", value = TRUE),
        bsTooltip("startUnknown","Please untick box to modify date", placement = "right")
      )
    ) # end date row close
  )
) # Project Timeline box close
