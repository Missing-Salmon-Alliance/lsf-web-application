######
# Using a renderUI element to create a dynamic set of sidebar items based on logged on user (admin or not)
######

output$adminSidebarUI <- renderUI({
  req(user_info) # only action if user_info has been created
  if (user_info$result) { # if user logon is true:
    column(
      width = 12,
      br(),
      h4("Load and review"),
      selectizeInput('QCidSelector','Dropdown',choices = lsfMetadata()$id, width = '100%'),
      column(
        width = 6,
        actionLink('QCpreviousID',"Prev")
      ),
      column(
        width = 6,
        actionLink('QCnextID',"Next")
      ),
      br(),
      h6("...OR...",align = 'center'),
      textInput('QCcustomID','Manual input:'),
      actionButton('QCcustomIDButton',"Load from ID")
    )
  }
})


output$menu <- renderUI(
  if(!is.null(user_info) && user_info$result && user_info$admin){
    sidebarMenu(id="menu1",
                
                # Admin Menu items
                checkboxInput('debug',"Debug Info"),# DEBUG - Tick box to show input raw outputs
                menuItem("Introduction", tabName = 'introduction', icon = icon('info',verify_fa = FALSE)),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus',verify_fa = FALSE))),
                menuItem("Search and Explore",tabName = 'searchlsf', icon = icon('search-location',verify_fa = FALSE)),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram',verify_fa = FALSE)),
                menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram',verify_fa = FALSE)),
                menuItem("Administration", tabName = 'adminZone', icon = icon('cogs',verify_fa = FALSE))

                
    )
  }else{
    sidebarMenu(id="menu1",
                
                # Basic Menu items
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus',verify_fa = FALSE))),
                menuItem("Search and Explore",tabName = 'searchlsf', icon = icon('search-location',verify_fa = FALSE)),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram',verify_fa = FALSE)),
                menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram',verify_fa = FALSE))
    )
  }
)

output$searchSidebarFilters <- renderUI({
  shiny::div(
    h4("Filters"),
    splitLayout(
        shiny::actionButton(
          inputId = 'actionFilter',
          label = "Apply",
          icon = icon("filter", style = "color:black;"),
          width = '80%'
        ),
        shiny::actionButton(
          inputId = 'actionFilterReset',
          label = "Reset",
          icon = icon("arrows-rotate", style = "color:black;"),
          width = '80%'
        )
    ),
    textInput(
      inputId = 'keywordFilter',
      label = "Keyword Filter"
    ),
    bsTooltip("keywordFilter","Filter using keywords, delimit terms with a comma or semi-colon.", placement = "right"),
    shinyWidgets::pickerInput(
      inputId = 'domainFilter',
      label = "Life-Stage Domain",
      choices = stats::setNames(as.list(lsfDomains()$id),lsfDomains()$domainTitle),
      width = '100%',
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        selectedTextFormat = 'count',
        liveSearch = TRUE,
        container = 'body')
    ),
    shinyWidgets::pickerInput(
      inputId = 'esvFilter1',
      label = "Salmon Trait",
      choices = stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$id),
            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$esvTitle),
      width = '100%',
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        selectedTextFormat = 'count',
        liveSearch = TRUE,
        container = 'body')
    ),
    shinyWidgets::pickerInput(
      inputId = 'esvFilter2',
      label = "Biological Processes",
      choices = stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$id),
            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$esvTitle),
      width = '100%',
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        selectedTextFormat = 'count',
        liveSearch = TRUE,
        container = 'body')
    ),
    shinyWidgets::pickerInput(
      inputId = 'esvFilter3',
      label = "Physical Environment",
      choices = stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$id),
            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$esvTitle),
      width = '100%',
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        selectedTextFormat = 'count',
        liveSearch = TRUE,
        container = 'body')
    ),
    shinyWidgets::pickerInput(
      inputId = 'stockunitFilter',
      label = "Stock Unit",
      choices = stockUnits,
      width = '100%',
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        selectedTextFormat = 'count',
        liveSearch = TRUE,
        container = 'body')
    ), style = "font-size:80%") # reduce font size in table
})

