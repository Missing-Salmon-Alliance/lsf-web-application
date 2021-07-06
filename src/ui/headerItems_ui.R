############################
#headerItems_ui.R START
############################

tags$li(
  #Set height of dashboardHeader so that it fits MSA logo
  tags$style(".main-header {max-height: 100px}"),
  tags$style(".main-header .logo {height: 100px}"),
  # LOGON/LOGOUT BUTTON
  #########################################################
  #https://community.rstudio.com/t/how-to-create-this-user-panel-on-top-right-corner-of-shiny/7806/4
  actionLink("loginModal", label = "Login", icon = icon("sign-out-alt"),style='padding:5px; font-size:120%;float:right;'),
  shinyjs::hidden(actionLink("logoutModal", label = "Logout", icon = icon("sign-in-alt"),style='padding:5px; font-size:120%;float:right;')),
  
  # The following renderUI actionLinks don't pick up the default actionLink CSS for some reason
  # So using a customised hover background color to match the above actionLink color scheme
  
  # #337ab7 close to dashboard default hover background color
  # #3c8dbc dashboard default header background
  # #333 ?
  
  uiOutput("userpanel"),
  tags$style("#header-user-info:hover{background-color:#337ab7;}"),
  uiOutput("basketUi"),
  tags$style("#basket:hover{background-color:#337ab7;}"),
  # set class
  class = "dropdown"
)

############################
#headerItems_ui.R END
############################