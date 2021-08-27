############################
#introductionTab_Sidebar_ui.R START
############################
conditionalPanel(
  condition = "input.menu1 == 'introduction' && !output.logonTrue",
  column(
    width = 12,
    hr(),
    p("Access to the Central Data Resource is currently limited to members of a Data Sharing Group. To sign up to this group click here:"),
    actionButton('introRegisterLink',"Register"),
    br(),
    p("You will be asked to abide by terms and guiding principles. To view and download a copy of these terms click here:"),
    a(href='MemorandumofAgreement.pdf',"Memorandum of Agreement",target="_blank"),
    hr(),
    actionButton('introSideLogonButton',"Login",icon = icon("sign-in-alt"))
  )
)

############################
#introductionTab_Sidebar_ui.R END
############################