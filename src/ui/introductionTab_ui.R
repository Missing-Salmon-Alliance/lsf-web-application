############################
#introductionTab_ui START
############################

tabItem('introduction',
        fluidPage(
          fluidRow(
            h1("Welcome to the Salmon Ecosystem Data Hub (SalHub)"),
            br(),
            h3("Enhancing data mobilisation through a centralised data repository for Atlantic salmon -",a(href="https://doi.org/10.1016/j.ecoinf.2022.101746","https://doi.org/10.1016/j.ecoinf.2022.101746",target = "_blank")),
            tags$b("SalHub is a web application that aims to guide researchers to data sources for testing hypotheses,
               informing management activities, and ultimately improving Atlantic salmon populations."),
            p(),
            tags$i("This resource is ready for use but may change in appearance and functionality as our user base and knowledge base grows.
                   Development suggestions are always welcome and can be directed to the team via",a(href="mailto: data.admin@missingsalmonalliance.org","email."),"It is best viewed in a modern browser, at a zoom level of 100% or less and is not designed to be viewed on a mobile device.")
          ),
          fluidRow(
            hr(style="border-color: purple")
          ),
          fluidRow(
            box(
              width = 7,
              title = "The Salmon Ecosystem Data Hub",
              status = 'success',
              solidHeader = FALSE,
              em(carouselGeneralIntro),
              br(),
              br(),
              em("Further background information and instructional videos on using SalHub are available via the ",a(href='https://missingsalmonalliance.org/the-central-data-resource',"Missing Salmon Alliance website.",target="_blank"))
            ),
            column(
              width = 5,
              img(src = 'images/MSA_map_salmonRange.PNG', height = '350vh')
            )
          ),
          fluidRow(
            column(
              width = 5,
              img(src = 'images/screenshot_map_carousel.PNG', height = '350vh')
            ),
            box(
              width = 7,
              title = "Search The Salmon Ecosystem Data Hub",
              status = 'primary',
              solidHeader = FALSE,
              em(searchIntroCopy)
            )
          ),
          fluidRow(
            box(
              width = 7,
              title = "Submit Data to The Salmon Ecosystem Data Hub",
              status = 'primary',
              solidHeader = FALSE,
              em(submitIntroCopy)
            ),
            column(
              width = 5,
              img(src = 'images/screenshot_submit_carousel.PNG', height = '350vh')
            )
          ),
          fluidRow(
            column(
              width = 5,
              img(src = 'images/eml_logo.png', height = '350vh')
            ),
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
            )
          ),
          fluidRow(
            box(
              width = 7,
              title = "Data Citation",
              status = 'success',
              solidHeader = FALSE,
              #height = '350vh',
              em("The creation of static and citable data sources with an associated Digital Object
                 Identifier (DOI) promotes more open and reproducible research,
                 as well as adding provenance to new syntheses that can otherwise
                 be difficult to trace. Data citation also provides a way for data owners to leverage more
                 value from their used data resources. The Salmon Ecosystem Data Hub assigns locally unique
                 identifiers and can facilitate the creation of globally unique and dereferenceable DOI's via
                 the",a(href="https://knb.ecoinformatics.org","Knowledge Network for Biocomplexity.",target = "_blank")),
              br(),
              em("All members are expected to abide by academic norms for citation. Data Citation conventions have been
                 developed by the FORCE11 community and can be found here:",
                 a(href="https://www.force11.org/datacitationprinciples","force11.org",target="_blank"))
            ),
            column(
              width = 5,
              img(src = 'images/sitemap-solid.svg', height = '350vh', alt = "https://fontawesome.com/license")
            )
          )
        )
      )

############################
#introductionTab_ui END
############################