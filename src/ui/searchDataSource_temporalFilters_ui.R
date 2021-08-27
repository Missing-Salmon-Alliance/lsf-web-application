shinyBS::bsCollapsePanel(
  title = "Temporal Filters",
  style = 'primary',
  sliderInput('temporalSlider', "Select the Date Range", min = floor(min(LSFMetadataTibble$metadataCoverageStartYear)/10)*10, max = ceiling(max(LSFMetadataTibble$metadataCoverageEndYear)/10)*10, 
              value = c(1970,2010), step = 1, sep = "", width = '100%'),
  shinyWidgets::checkboxGroupButtons('monthsSelect', "Select the Relevant Months", choices = month.abb, selected = character(0),
                                     checkIcon = checkboxGroupButtonsIcons,
                                     size = 'xs')
)

