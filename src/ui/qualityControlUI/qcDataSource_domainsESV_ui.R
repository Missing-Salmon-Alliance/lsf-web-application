box(
  title = p("Life-Stage Domain and Variable Class"),
  status = 'primary',
  solidHeader = TRUE,
  collapsible = TRUE,
  width = 12,
  footer = p(actionButton('submitQCDomainESV',"Commit Changes",class = 'btn-warning'),actionButton('resetQCDomainESV',"Reset Changes",class = 'btn-success')),
  shinyWidgets::checkboxGroupButtons('qcDomainNodeList',
                                     'Life-Stage Domain',
                                     choices = LSFDomainTibble$domainTitle,
                                     justified = F,
                                     status = 'default',
                                     size = 'sm',
                                     width = '100%',
                                     checkIcon = checkboxGroupButtonsIcons),
  uiOutput('qcesvPerDomain')
)