############################
# Modal Dialogs for information points throughout the app
###########################

observeEvent(input$submitDescript,{
  showModal(
    modalDialog(title = "",
                size = 'l',
                column(12,
                       h1("Introduction to the Submit Page of the Central Data Resource"),
                       br(),
                       em(submitIntroCopyPara1),
                       p(),
                       em(submitIntroCopyPara2)
                )
      
    )
  )
})

observeEvent(input$esvFields,{
  showModal(
    modalDialog(
      title = "",
      size = 'l',
      column(12,
             h3("Domains and Variable Classes"),
             em("Here the data being submitted can be tagged regarding how it may be relevant to the differing habitats across the Atlantic salmon’s lifecycle."),
             br(),
             h4("Domain"),
             em(domainsDescriptionCopy),
             h4("Variable Category"),
             em("Select whether the data being submitted is regarding biological processes (e.g. Fish Parasites), physical processes (e.g. Sea Surface Temperature) or salmon traits (e.g. Sex Ratio (smolt)). Following selection of this category the extended variable classes will become available for selection on the right, organised by their relevant domains."),
             h4("Variable Class"),
             em(esvDescriptionCopy),
      )
    )
  )
  
})

shinyBS::addPopover(session, id = 'domainNodeList',title = "Domains and Variable Classes",
                    content = HTML(paste0(strong("Domains"),em(paste0(" - ",domainsDescriptionCopy)),p(),
                                          strong("Variable Classes"),em(paste0(" - ",esvDescriptionCopy)))),
                    placement = 'top',
                    options = list(container = "body")
)



observeEvent(input$metadataFields,{
  showModal(
    modalDialog(
      title = "",
      size = 'l',
      column(12,
             h3("Data Source Details "),
             br(),
             h4("Title"),
             em("Provide a brief title for your data being inputted. Provide the major theme of the data and the species or physical factors involved, the years in which the data was collected and any specific geographical locations regarding the data e.g. The rates of adult Atlantic salmon returning to the River Bush, Northern Ireland. 1980-2021. "),
             br(),
             h4("Abstract"),
             em("Provide a brief description of your data, providing more detail than the title. Information may include the seasonal extents of when data was collected, how data was collected and potential trends in the data. "),
             br(),
             h4("Primary Contact "),
             em("Provide the name of the individual in possession of this data. This person will be recognised as the owner of the data."),
             br(),
             h4("Primary Contact Email "),
             em("Provide the email address of the primary contact. "),
             br(),
             h4("Primary Contact ORCID"),
             em("Provide the ORCID ID of the primary contact. ORCID", a(href="https://orcid.org/","(https://orcid.org/)")," provides unique dereferenceable digital IDs for researchers to share their professional information within the scientific community."),
             br(),
             h4("Organisation"),
             em("Provide the name of the organisation which holds or funded the data being submitted."),
             br(),
             h4("URL"),
             em("If the data being submitted is hosted on another website or database, please provide the URL for the location of this data."),
             #em("Note that if the data is already hosted on the Knowledge Network for Biocomplexity then the sidebar ",strong("'Load from KNB'")," option can be used to link the resources."),
             br(),
             h4("Select Update Frequency "),
             em("Provide information on how regularly the data is updated, if updated at all."),
             br(),
             h4("Embargoed Data"),
             em("Provide information on any embargo or time limit on data availability if applicable")
      )
    )
  )
  
})

observeEvent(input$geogTimeFields,{
  showModal(
    modalDialog(
      title = "",
      size = 'l',
      h3("Geographical Coverage"),
      em("Provide a brief description of the geographic location of where the data was collected e.g. The River Bush, Northern Ireland, and define the location boundaries in decimal degrees. 
      Clicking a location on the map will automatically fill in the boundaries on the left. 
      The boundary inputs can also be modified manually and accept decimal degrees to an accuracy of 4 decimal places (roughly 10 metres)."),
      hr(),
      h3("Temporal Coverage"),
      em("Provide information of the temporal extents of the data. Use the slider to define which years the data were collected over, and the buttons to identify during which months data were collected if applicable")
    )
  )
})

observeEvent(input$searchDescript,{
  showModal(
    modalDialog(
      title = "",
      size = 'l',
      column(12,
             h1("Instructions on the Central Data Resource Search Page"),
             br(),
             em(searchIntroCopyPara1),
             p(),
             em(searchIntroCopyPara2),
             h2("Descriptions of the Interface Tools"),
             br(),
             h4("The Map"),
             em("To help illustrate the spatial extents of the knowledge sources the map pinpoints the data using blue markers which can be selected to provide further information. The Atlantic Ocean has been categorised by both ICES and the NAFO into divisions and regions. Here you can toggle these on and off to see which regions data may share."),
             br(),
             h4("Date Scroller"),
             em("The year that data is collected is extremely important, as data sources sharing similar temporal extents can be compatible. Data scrollers allow you to filter the data shown on both the Map and the Table dependent on their start and end dates."),
             sliderInput("demoBegin", "Timeseries starting:", min = 1900, max = 2020, value = 2000),
             br(),
             h4("Month Scroller"),
             em("The position of Atlantic salmon varies with the changing months of the year. Data recorded in the known salmon migration routes may not be as relevant as initially thought due to it being taken at months where Atlantic salmon are not present. Filtering the data regarding its month will help to focus your search."),
             br(),
             h4("Ecoregion Selection"),
             em("ICES have categorised the North-East Atlantic in Ecoregions. This is to help with the development of an ecosystem-based approach in European waters. This ecoregions are based on biogeographic and oceanographic features. Selection of Ecoregions using the dropdown tool we provide will help focus the metadata in the table to only return relevant data."),
             selectInput("demoEcoRegion", "Choose an ICES Eco Region", choices = c(uniqueICESEcoRegions),multiple = FALSE),
             br(),
             h4("NAFO Selection"),
             em("NAFO have categorised the North-West Atlantic into Divisions. Selection of NAFO Divisions using the dropdown tool we provide will help focus the metadata in the table to only return relevant data."),
             # filter_select("Chose_NAFO", "Choose your NAFO Division", sd, group = ~nafo),
             br(),
             h4("Domains"),
             em(domainsDescriptionCopy),
             br(),
             h4("Variable Classes"),
             em(esvDescriptionCopy)
      )
    )
  )
})