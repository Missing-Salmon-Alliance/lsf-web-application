#shinyBS::bsCollapsePanel(title = "Domain and Variable Class",style = 'primary',
box(title = p("Domain and Variable Class",actionLink(inputId = "esvFields", label = "", icon = icon("question-circle"))),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
                         column(
                           width = 5,
                           shinyWidgets::checkboxGroupButtons('domainNodeList',
                                                              'Domain',
                                                              choices = LSFDomainTibble$domainTitle,
                                                              justified = F,
                                                              status = 'default',
                                                              checkIcon = checkboxGroupButtonsIcons),
                           shinyWidgets::checkboxGroupButtons('esvCategory',
                                                              'Variable Category',
                                                              choices = c('Biological','Physical','Salmon Trait'),
                                                              status = 'default',checkIcon = checkboxGroupButtonsIcons)
                         ),
                         column(
                           width = 7,
                           uiOutput('esvPerDomain')
                         )
)