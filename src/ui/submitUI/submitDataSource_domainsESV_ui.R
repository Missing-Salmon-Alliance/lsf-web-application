#shinyBS::bsCollapsePanel(title = "Domain and Variable Class",style = 'primary',
box(title = "Domain and Variable Class",status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
                         div(style="float:right",actionLink(inputId = "esvFields", label = "Help", icon = icon("question-circle"))),
                         br(),
                         column(
                           width = 5,
                           p("LSF Domain"),
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
                           p("LSF Variable Class"),
                           uiOutput('esvPerDomain')
                         )
)