######
# Using a renderUI element to create a dynamic set of sidebar items based on logged on user
######
output$menu <- renderUI(
  if(!is.null(user_info()) && user_info()$admin){
    sidebarMenu(id="menu1",
                
                # Admin Menu items
                checkboxInput('debug',"Debug Info"),# DEBUG - Tick box to show input raw outputs
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus'))),
                menuItem("Search and Explore",tabname = 'searchTabs', icon = icon('search-location'),
                         menuSubItem("Explore Map", tabName = 'searchlsf', icon = icon('globe-europe')),
                         menuSubItem("Explore Hypotheses", tabName = 'hypothesisExplore', icon = icon('question')),
                         menuSubItem("Explore Life-Stage Domains", tabName = 'domainExplore', icon = icon('fish'))
                ),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram')),
                menuItem("Metadata Node Report", tabName = 'metadataNodeReport', icon = icon('flag-checkered')),
                menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram')),
                menuItem("Administration", tabName = 'adminZone', icon = icon('cogs'))

                
    )
  }else{
    sidebarMenu(id="menu1",
                
                # Basic Menu items
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus'))),
                menuItem("Search and Explore",tabname = 'searchTabs', icon = icon('search-location'),
                  menuSubItem("Explore Map", tabName = 'searchlsf', icon = icon('globe-europe')),
                  menuSubItem("Explore Hypotheses", tabName = 'hypothesisExplore', icon = icon('question')),
                  menuSubItem("Explore Life-Stage Domains", tabName = 'domainExplore', icon = icon('fish'))
                  ),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram'))
    )
  }
)

