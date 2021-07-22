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
               "this portal aims to guide researchers to data sources for addressing hypotheses, 
                                        informing management activities, and ultimately improving salmon populations around 
                                        the Atlantic Ocean."),
            em("Access to the Central Data Resource is restricted to persons providing and/or making direct use of data sources for research. To sign up to this group click",actionLink('introRegisterLink',"here."),"You will be asked to abide 
                                        by the Data Sharing Group terms and guiding principles. To view and download a copy of these terms click",a(href='MemorandumofAgreement.pdf',"here.",target="_blank"))
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
                  width = 12,
                  img(src = 'msa-logo.png', height = '350vh')
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
                  em("The data source descriptions are grounded in a common language (Ecological Metadata Language -",a(href="https://www.dcc.ac.uk/resources/metadata-standards/eml-ecological-metadata-language","EML",target="_blank"),"), all effort put in to describing the data thoroughly via this interface is interoperable with popular online data portals such as:"),
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
              ) # carousel item end
              # carouselItem(
              #   column(
              #     width = 5,
              #     img(src = 'images/screenshot_searchmap.PNG', height = '350vh')
              #   ),
              #   box(
              #     width = 7,
              #     title = "Blank Item 1",
              #     status = 'danger',
              #     solidHeader = FALSE,
              #     em("Holding area for more information")
              #   )
              # ), # carousel item end
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