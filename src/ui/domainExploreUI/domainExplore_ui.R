############################
#domainExplore_ui.R START
############################
# tabItem domain explore ui
tabItem( # tabItem
  tabName = "domainExplore",
  conditionalPanel(
    condition = "output.logonTrue",
    h4("Explore available data resources based on salmon life-stage domains."),
    p("The life-stage domains represent a combination of salmon lifecycle and the environments within which they reside and transit."),
    box(
      width = 5,
      status = 'primary',
      title = "Step 1 - Filter by Life-Stage Domain (multi-select available)",
      shinyWidgets::checkboxGroupButtons('domainFilter',"Select a Salmon Life-Stage Domain",choices = LSFDomainTibble$domainTitle,
                                         checkIcon = checkboxGroupButtonsIcons)
    ),
    box(
      status = 'primary',
      title = "Step 2 - Select Variable Classes to filter resources in step 3",
      width = 7,
      shinyWidgets::checkboxGroupButtons('esvFilterBioDom',"Biological Variables",choices = c("Please select a Domain"),selected = "Please select a Domain"),
      shinyWidgets::checkboxGroupButtons('esvFilterPhysDom',"Physical Variables",choices = c("Please select a Domain"),selected = "Please select a Domain"),
      shinyWidgets::checkboxGroupButtons('esvFilterTraitDom',"Salmon Trait Variables",choices = c("Please select a Domain"),selected = "Please select a Domain")
    ),
    box(
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      title = "Step 3 - Available Resources for Selected Variable Classes",
      DT::DTOutput('domainExploreTable')
    )
  ),
  conditionalPanel(
    condition = "!output.logonTrue",
    h1("Life-Stage Domain Explore Area"),
    h3("Please authenticate to access this area")
  )
) # tabItem close
############################
#domainExplore_ui.R END
############################