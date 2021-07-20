shinyBS::bsCollapsePanel(
  style = 'primary',
  title = "Framework Filters",
  h4("The data sources are indexed by variable classes. There are ~60 classes, you can subset them by either a Mortality Hypothesis or a set of Salmon Domains."),
  shinyBS::bsCollapse(
    id = "mapSearchFilterFramework",
    open = "Filter by Hypothesis and Variable Class",
    shinyBS::bsCollapsePanel(
       style = 'primary',
       title = "Filter by Hypothesis and Variable Class"
    #   column(
    #     width = 12,
    #     selectizeInput('hypothesisFilter',
    #                    "Select a Mortality Hypothesis...",
    #                    choices = c("",LSFHypothesisTibble$hypothesisTitle),
    #                    selected = "",
    #                    multiple = FALSE,
    #                    width = '100%'),
    #     selectizeInput('subHypothesisFilter',
    #                    "... and a Sub-Hypothesis",
    #                    choices = c("Please select a Mortality Hypothesis"),
    #                    selected = "Please select a Mortality Hypothesis",
    #                    multiple = FALSE,
    #                    width = '100%')
    #   ),
    #   column(
    #     width = 4,
    #     selectizeInput('esvFilterBioHyp',"Biological Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",multiple = FALSE)
    #   ),
    #   column(
    #     width = 4,
    #     selectizeInput('esvFilterPhysHyp',"Physical Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",multiple = FALSE)
    #   ),
    #   column(
    #     width = 4,
    #     selectizeInput('esvFilterTraitHyp',"Salmon Trait Variables",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",multiple = FALSE)
    #   )
    ),
    shinyBS::bsCollapsePanel(
      style = 'primary',
      title = "Filter by Domain and Variable Class"
      # column(
      #   width = 12,
      #   selectizeInput('domainFilter',"Select a Salmon Domain",choices = LSFDomainTibble$domainTitle,multiple = TRUE)
      # ),
      # column(
      #   width = 4,
      #   selectizeInput('esvFilterBioDom',"Biological Variables",choices = c("Please select a Domain"),selected = "Please select a Domain",multiple = FALSE)
      # ),
      # column(
      #   width = 4,
      #   selectizeInput('esvFilterPhysDom',"Physical Variables",choices = c("Please select a Domain"),selected = "Please select a Domain",multiple = FALSE)
      # ),
      # column(
      #   width = 4,
      #   selectizeInput('esvFilterTraitDom',"Salmon Trait Variables",choices = c("Please select a Domain"),selected = "Please select a Domain",multiple = FALSE)
      # )
    )
  )
)
