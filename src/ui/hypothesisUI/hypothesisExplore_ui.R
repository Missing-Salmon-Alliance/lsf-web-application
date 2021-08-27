############################
#hypothesisExplore_ui.R START
############################
# tabItem hypothesis ui
tabItem( # tabItem
  tabName = "hypothesisExplore",
  conditionalPanel(
    condition = "output.logonTrue",
    h4("Explore available data resources based on addressing a specific hypothesis."),
    p("To advance international efforts to uncover the reasons for downward trends in
      salmon abundance, the MSA Likely Suspects Framework team has been trying to focus
      scientific efforts onto identifying a group of key hypotheses relating to possible
      drivers of salmon mortality. Read more about this activity on the",a(href="https://missingsalmonalliance.org","MSA website.",target="_blank"),
      "The set of 11 resultant priority candidate hypotheses statements are presented here
      as a way to filter available data resources based on how applicable variable classes
      are to the selected hypothesis."),
    box(
      status = 'primary',
      title = "Step 1 - Hypotheses",
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
      title = "Step 2 - Variable Classes Relevant to Selected Hypothesis",
      width = 7,
      shinyWidgets::checkboxGroupButtons('esvFilterBioHyp',"Biological Processes",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'xs',
                                         checkIcon = checkboxGroupButtonsIcons),
      shinyWidgets::checkboxGroupButtons('esvFilterPhysHyp',"Physical Environment",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'xs',
                                         checkIcon = checkboxGroupButtonsIcons),
      shinyWidgets::checkboxGroupButtons('esvFilterTraitHyp',"Salmon Traits",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                         justified = F,
                                         individual = T,
                                         status = "default",
                                         size = 'xs',
                                         checkIcon = checkboxGroupButtonsIcons)
    ),
    box(
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      title = "Step 3 - Results",
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