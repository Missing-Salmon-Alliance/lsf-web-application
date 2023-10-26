################ Search
tabItem( # tabItem 1
  tabName = "searchlsf",
  fluidPage(
    h1("Explore Data Resources",actionLink(inputId = 'info_modal_explore_guidance', 
                                           label = "",
                                           icon = icon("question-circle")))
  ),
  hr(style="border-color: purple"),
  uiOutput('searchMapTabUI')
)