############################
#introductionTab_ui START
############################

tabItem('introduction',
        fluidPage(
          fluidRow(
            h1("Welcome to the Central Data Resource for Atlantic Salmon"),
            br(),
            h4("Brought to you by the ",
               a(href="https://missingsalmonalliance.org/","Missing Salmon Alliance",target="_blank"),
               "as part of the",
               a(href="https://missingsalmonalliance.org/likely-suspects-framework","Likely Suspects Framework,",target="_blank"),
               "this portal aims to guide researchers to data sources for testing hypotheses, 
                                        informing management activities, and ultimately improving Atlantic salmon populations."),
            
            tags$b("This resource is ready for use but may change in appearance and functionality as our user base and knowledge base grows. 
                                    Development suggestions are always welcome and can be directed to the team via",a(href="mailto: data.admin@missingsalmonalliance.org","email."),"It is best viewed in a modern browser, at a zoom level of 100% or less and is not designed to be viewed on a mobile device.")
          ),
          fluidRow(
            hr(style="border-color: purple")
          ),
          fluidRow(
            carousel(
              id = "mycarousel",
              width = 12,
              carouselItem(
                column(
                  width = 7,
                  img(src = 'msa-logo.png', height = '350vh')
                ),
                box(
                  width = 5,
                  title = "The Missing Salmon Alliance",
                  status = 'primary',
                  em("A group of conservation focused organisations working together to drive action and save our wild Atlantic salmon from extinction by combining expertise, coordinating activities and advocating effective management solutions.")
                )
              ),
              carouselItem(
                box(
                  width = 7,
                  title = "The Central Data Resource",
                  status = 'success',
                  solidHeader = FALSE,
                  em(carouselGeneralIntro),
                  br(),
                  column(
                    2
                  ),
                  column(
                    10,
                    tags$ul(
                      tags$li(em("Primary Observation")),
                      tags$li(em("Derived Output")),
                      tags$li(em("Model Output"))
                    )
                  )
                ),
                column(
                  width = 5,
                  img(src = 'MSA_WebsiteLayout_lowres.png', height = '350vh')
                )
              ), # carousel item end
              carouselItem(
                box(
                  width = 5,
                  title = "Search The Framework",
                  status = 'primary',
                  solidHeader = FALSE,
                  em(searchIntroCopy)
                ),
                column(
                  width = 7,
                  img(src = 'images/screenshot_map_carousel.PNG', height = '350vh')
                )
              ), # carousel item end
              carouselItem(
                box(
                  width = 6,
                  title = "Submit Data to The Central Data Resource",
                  status = 'primary',
                  solidHeader = FALSE,
                  em(submitIntroCopy)
                ),
                column(
                  width = 6,
                  img(src = 'images/screenshot_submit_carousel.PNG', height = '350vh')
                )
              ), # carousel item end
              carouselItem(
                box(
                  width = 7,
                  title = "The Metadata",
                  status = 'danger',
                  solidHeader = FALSE,
                  em("The data source descriptions are grounded in a common language,",a(href="https://www.dcc.ac.uk/resources/metadata-standards/eml-ecological-metadata-language","Ecological Metadata Language (EML).",target="_blank"),"All efforts put in to describing the data thoroughly via this interface are interoperable with popular online metadata catalogues such as:"),
                  br(),
                  column(
                    2
                  ),
                  column(
                    10,
                    tags$ul(
                      tags$li(em(a(href="https://knb.ecoinformatics.org/","Knowledge Network for Biocomplexity",target="_blank"))),
                      tags$li(em(a(href="https://www.gbif.org/","Global Biodiversity Information Facility",target="_blank"))),
                      tags$li(em(a(href="https://lternet.edu/","Long Term Ecological Research Network",target="_blank")))
                    )
                  )
                ),
                column(
                  width = 5,
                  img(src = 'images/screenshot_searchmap.PNG', height = '350vh')
                )
              ), # carousel item end
              carouselItem(
                column(
                  width = 5,
                  img(src = 'images/screenshot_searchmap.PNG', height = '350vh')
                ),
                box(
                  width = 7,
                  title = "Data Citation",
                  status = 'success',
                  solidHeader = FALSE,
                  em("The creation of static and citable data sources with an associated Digital Object
                     Identifier (DOI) promotes more open and reproducible research,
                     as well as adding provenance to new syntheses that can otherwise
                     be difficult to trace. Data citation also provides a way for data owners to leverage more
                     value from their used data resources. The Central Data Resource assigns locally unique
                     identifiers and can facilitate the creation of globally unique and derefenceable DOI's via
                     the",a(href="https://knb.ecoinformatics.org","Knowledge Network for Biocomplexity.",target = "_blank")),
                  br(),
                  em("All members are expected to abide by academic norms for citation. Data Citation conventions have been
                     developed by the FORCE11 community and can be found here:",
                     a(href="https://www.force11.org/datacitationprinciples","force11.org",target="_blank"))
                )
              ) # carousel item end
              # carouselItem(
              #   box(
              #     width = 7,
              #     title = "Blank Item 2",
              #     status = 'danger',
              #     solidHeader = FALSE,
              #     em("Holding area for more information")
              #   ),
              #   column(
              #     width = 5,
              #     img(src = 'images/screenshot_searchmap.PNG', height = '350vh')
              #   )
              # ) # carousel item end
            ) # carousel end
          ) # fluid row end
          # fluidRow(
          #   box(
          #     width = 12,
          #     status = 'warning',
          #     solidHeader = FALSE,
          #     title = "Variable Classes",
          #     h4("Variable classes are used to index data sources within the Central Data Resource. You may like to explore the classes here prior to searching for or submitting data sources"),
          #     column(
          #       width = 6,
          #       selectInput('varClassesDomain',"Domain",choices = LSFDomainTibble$domainTitle)
          #     ),
          #     column(
          #       width = 6,
          #       selectInput('varClassesCats',"Category",choices = unique(LSFEssentialSalmonVariableTibble$esvCategory))
          #     ),
          #     DT::DTOutput('varClassesFull'),
          #     br(),
          #     br(),
          #     br()
          #   )
          # ) # fluid row end
        )
)

############################
#introductionTab_ui END
############################