box( # Themes and Focus box open
  tags$style(".popover{
            font-size: 12px;
          }"),
  status = "primary",
  collapsible = FALSE,
  solidHeader = TRUE,
  title = "Atlantic Salmon Likely Suspects Themes and Mortality Focus",
  width = 7,
  column( # sub column 1 open
    width = 6,
    checkboxGroupInput( # checkboxGroupInput 1
      "theme",
      label = "Select Themes addressed by the research project",
      choiceNames = list(
        "Method development",
        "Carryover effects",
        "Local/regional mortality drivers",
        "Ocean basin scale drivers of marine mortality",
        "Demographic mechanisms",
        "Eco-evolutionary processes",
        "Getting the data together",
        "Guidance to managers"),
      choiceValues = list(
        "lsf_theme1",
        "lsf_theme2",
        "lsf_theme3",
        "lsf_theme4",
        "lsf_theme5",
        "lsf_theme6",
        "lsf_theme7",
        "lsf_theme8")
    ) # checkboxGroupInput 1 close
  ), # sub column 1 close
  column( # sub column 2 open
    width = 6,
    checkboxGroupInput( # checkboxGroupInput 2
      "mortalityFocus",
      label = "Select Mortality Focus areas addressed by the research project",
      choiceNames = list(
        "Freshwater conditions and carry-over mortality",
        "In-river smolt migration mortality",
        "Estuary smolt migration mortality",
        "Coastal/shelf migration post-smolt mortality",
        "Shelf current migration post-smolt mortality",
        "Ocean feeding sub-adult mortality",
        "Post Pre-Fisheries Abundance (PFA) ocean mortality",
        "Ocean feeding maturing adult mortality",
        "Estuary adult migration mortality"
      ),
      choiceValues = list(
        "lsf_focus1",
        "lsf_focus2",
        "lsf_focus3",
        "lsf_focus4",
        "lsf_focus5",
        "lsf_focus6",
        "lsf_focus7",
        "lsf_focus8",
        "lsf_focus9"
      ),
      inline = FALSE
    ), # checkboxGroupInput 2 close
    bsPopover("test","test") # strange behaviour, comment this out and server side bsPopover stops working
  )  # sub column 2 close
) # Themes and Focus box close