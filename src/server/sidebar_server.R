output$menu <- renderUI(
  if(!is.null(user_info()) && user_info()$admin){
    sidebarMenu(id="menu1",
                
                # Admin Menu items
                # DEBUG - Tick box to show input raw outputs, to be commented out in production
                checkboxInput('debug',"Debug Info"),
                menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus'))),
                menuItem("Search The LSF", tabName = 'searchlsf', icon = icon('searchengin')),
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
                menuItem("Search The LSF", tabName = 'searchlsf', icon = icon('searchengin')),
                menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram'))
    )
  }
)

