############################
#hypothesisExplore_ui.R START
############################
# tabItem hypothesis ui
tabItem( # tabItem
  tabName = "hypothesisExplore",
  conditionalPanel(
    condition = "output.logonTrue",
    h4("Explore available data resources based on addressing a specific hypothesis."),
    p("We have developed 11 example Mortality Hypotheses to demonstrate data resource availability in this light."),
    box(
      status = 'primary',
      title = "Step 1 - Filter by Hypothesis",
      width = 5,
      column(
        width = 12,
        selectizeInput('hypothesisFilter',
                       "Select a Mortality Hypothesis...",
                       choices = c("",LSFHypothesisTibble$hypothesisTitle),
                       selected = "",
                       multiple = FALSE,
                       width = '100%'),
        selectizeInput('subHypothesisFilter',
                       "... and a Sub-Hypothesis",
                       choices = c("Please select a Mortality Hypothesis"),
                       selected = "Please select a Mortality Hypothesis",
                       multiple = FALSE,
                       width = '100%')
      )
    ),
    box(
      status = 'primary',
      title = "Step 2 - Select Variable Classes to filter resources in step 3",
      width = 7,
      #fluidRow(
        #width = 4,
      shinyWidgets::checkboxGroupButtons('esvFilterBioHyp',"Biological Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'xs',
                                         checkIcon = checkboxGroupButtonsIcons),
      #),
      #fluidRow(
        #width = 4,
      shinyWidgets::checkboxGroupButtons('esvFilterPhysHyp',"Physical Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'lg',
                                         checkIcon = checkboxGroupButtonsIcons),
      #),
      #fluidRow(
        #width = 4,
      shinyWidgets::checkboxGroupButtons('esvFilterTraitHyp',"Salmon Trait Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'xs',
                                         checkIcon = checkboxGroupButtonsIcons)
      #)
    ),
    box(
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      title = "Step 3 - Available Resources for Selected Variable Classes",
      DT::DTOutput('hypothesisExploreTable')
    )
  ),
  conditionalPanel(
    condition = "!output.logonTrue",
    h1("Hypothesis Explore Area"),
    h3("Please authenticate to access this area")
  )
) # tabItem close
############################
#hypothesisExplore_ui.R END
############################