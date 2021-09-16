tabItem( # tabItem 2
  tabName = "newproject",
  tabsetPanel(
    id = 'researchInventory',
    selected = "New Project Form",
    tabPanel(
      title = "New Project Form",
      fluidPage(
        h2("Likely Suspect Framework for Atlantic Salmon: a review of current research project activity"),
        fluidRow(
          column(
            width = 12,
            h4("We are conducting a review of current and planned Atlantic salmon research activity to provide focus and identify knowledge gaps.
            We would be extremely grateful if you could complete and submit this form to ensure that your work is included in this process.")
          )
        ),
        box(
          # Project Detail box open
          status = "primary",
          collapsible = FALSE,
          solidHeader = TRUE,
          title = "Project Detail (* required fields)",
          width = 5,
          checkboxInput('iasrb',span("Check this box if this project is already described on the ",a(href="https://salmonatsea.com/research-inventory/","International Atlantic Salmon Research Board:",target="_blank"))),
          textInput("projectTitle", label = "Main Project Title *"),
          bsTooltip("projectTitle","Max 150 Characters", placement = "right"),
          textInput("workPackageTitle", label = "Work Package Title (if applicable)"),
          bsTooltip("workPackageTitle","Max 150 Characters", placement = "right"),
          textInput("organisations", label = "Organisations and/or Institutions"),
          bsTooltip("organisations","Max 200 Characters", placement = "right"),
          textAreaInput("summary", label = "Summary of Objectives *", height = "111px"), # height adjusted to match total height of right set of boxes
          bsTooltip("summary","Max 500 Characters", placement = "right"),
          textInput("urls", label = "Project and Publication Hyperlinks"),
          bsTooltip("urls","Max 200 Characters", placement = "right")
        ),
        box( # Contact Detail box open
          status = "primary",
          collapsible = FALSE,
          solidHeader = TRUE,
          title = "Contact Details",
          width = 2,
          textInput("coordinator", label = "Lead Contact Name *"),
          bsTooltip("coordinator","Max 50 Characters", placement = "right"),
          textInput("email", label = "Lead Contact Email *"),
          bsTooltip("email","Max 50 Characters", placement = "right")
        ),
        source("./src/ui/researchInventoryUI/box_project_timeline.R",local = TRUE)$value,
        source("./src/ui/researchInventoryUI/box_themes_focus_checks.R",local = TRUE)$value,
        column(
          width = 10
          ),
        column(
          width = 2,
          actionButton(
            "submitProject",
            "Submit Project",
            width = 200,
            class = "btn-info btn-lg"
          )
        )
      )
    ),
    tabPanel(
      title = "View Submitted Projects",
      fluidRow(
        # Should all users be able to view everyone's submissions?
        box(
          status = "primary",
          collapsible = TRUE,
          solidHeader = TRUE,
          title = "Newly Submitted Projects",
          width = 12,
          actionButton(
            'refreshProjectTable',
            "Refresh Table"
          ),
          DT::dataTableOutput('activityDF')
        )
      )
    )
  )
) # tabItem 2 close