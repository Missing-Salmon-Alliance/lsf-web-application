######
# Using a renderUI element to create a dynamic set of sidebar items based on logged on user (admin or not)
######

output$adminSidebarUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
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
  if(!is.null(user_info()) && user_info()$admin){
    sidebarMenu(id="menu1",
                
                # Admin Menu items
                checkboxInput('debug',"Debug Info"),# DEBUG - Tick box to show input raw outputs
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus'))),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram')),
                menuItem("Search and Explore",tabname = 'searchTabs', icon = icon('search-location'),
                         menuSubItem("Explore Map", tabName = 'searchlsf', icon = icon('globe-europe')),
                         menuSubItem("Explore Hypotheses", tabName = 'hypothesisExplore', icon = icon('question')),
                         menuSubItem("Explore Life-Stage Domains", tabName = 'domainExplore', icon = icon('fish'))
                ),
                # menuItem("Metadata Node Report", tabName = 'metadataNodeReport', icon = icon('flag-checkered')),
                menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram')),
                menuItem("Administration", tabName = 'adminZone', icon = icon('cogs'))

                
    )
  }else{
    sidebarMenu(id="menu1",
                
                # Basic Menu items
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus'))),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram')),
                menuItem("Search and Explore",tabname = 'searchTabs', icon = icon('search-location'),
                  menuSubItem("Explore Map", tabName = 'searchlsf', icon = icon('globe-europe')),
                  menuSubItem("Explore Hypotheses", tabName = 'hypothesisExplore', icon = icon('question')),
                  menuSubItem("Explore Life-Stage Domains", tabName = 'domainExplore', icon = icon('fish'))
                  )
    )
  }
)

