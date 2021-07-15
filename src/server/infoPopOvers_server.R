####################
# Information pop-over messages found throughout the site
####################

# Search Page Pop overs

shinyBS::addPopover(session, id = 'searchRefreshUI',title = "Refresh The Map",
           content = "Refresh the Map",
           placement = 'top',
           options = list(container = "body")
)


shinyBS::addPopover(session, id = 'icesStockUnitFilter',title = "Stock Unit Filter",
           content = "The Stock Unit Filter is not yet active. Work is ongoing to link stock units to data sources",
           placement = 'top',
           options = list(container = "body")
)

shinyBS::addPopover(session, id = 'icesIndexRiverFilter',title = "Index River Filter",
           content = "The Index River Filter is not yet active. Work is ongoing to link index rivers to data sources",
           placement = 'top',
           options = list(container = "body")
)


# Submit page pop overs

shinyBS::addPopover(session, id = 'clearSubmitFormUI',title = "Reset Form Data",
           content = "Clear all of the fields/reset to defaults",
           placement = 'top',
           options = list(container = "body")
)

shinyBS::addPopover(session, id = 'sourceKNBURIUI',title = "Enter a KNB URN or DOI",
           content = HTML(paste0(em("If the data you would like to register is already within the "),a(href="https://knb.ecoinformatics.org/","Knowledge Network for Biocomplexity"),em(" (KNB) then this can be loaded into the form via the URN or DOI of the KNB data package."),
                                 p(),em("This will automatically populate the fields within 'Data Source Details', 'Temporal Coverage' and 'Geographic Coverage', as far as the equivalent information exists in KNB."),
                                 p(),em("For example enter a URN:UUID:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx or DOI:10.5063/xxxxxx and press 'Load...'"),
                                 p(),em("If the KNB object is set to private, you may use your KNB access token below."))),
           placement = 'top',
           options = list(container = "body")
)

# shinyBS::addPopover(session, id = 'domainNodeList',title = "Domains and Variable Classes",
#            content = HTML(paste0(strong("Domains"),em(paste0(" - ",domainsDescriptionCopy)),p(),
#                                  strong("Variable Classes"),em(paste0(" - ",esvDescriptionCopy)))),
#            placement = 'top',
#            options = list(container = "body")
# )


shinyBS::addPopover(session, id = 'sourceMaintenance',title = "Update Frequency",
           content = "Indicate how often the dataset is expected to be updated.",
           placement = 'top',
           options = list(container = "body")
           
)

shinyBS::addPopover(session, id = 'sourceCreatorORCID',title = "ORCID URL",
           content = HTML(paste0(p("ORCID is a way to create unique dereferenceable identifiers for individuals. This is an optional field, however ORCID ID's can be easily created at ",
                                   a(href="https://orcid.org/","https://orcid.org/")))),
           placement = 'top',
           options = list(container = "body")
           
)

shinyBS::addPopover(session, id = 'embargoEnd',title = "Embargo End Date",
           content = "If the primary data being described is under an embargo please enter the expected end date or more information here.",
           placement = 'top',
           options = list(container = "body")
           
)

shinyBS::addPopover(session, id = 'uploadDataFileUI',title = "File Drop",
                    content = "Maximum File Size 25Mb. To upload a larger file please contact the Data Manager: data.admin@missingsalmonalliance.org",
                    placement = 'top',
                    options = list(container = "body")
                    
)
