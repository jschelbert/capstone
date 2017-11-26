#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = "green",
                      dashboardHeader(title = "PredWordR"),
                      ## Sidebar content
                      dashboardSidebar(
                          sidebarMenu(
                              #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                              menuItem("Prediction", icon = icon("commenting"), tabName = "widgets", badgeLabel = "try me", badgeColor = "green"),
                              menuItem("Random Babble", icon = icon("comments-o"), tabName = "random"),
                              menuItem("Background", icon = icon("mortar-board"), startExpanded = TRUE, tabName = "background",
                                       menuSubItem("Data & Preprocessing", icon = icon("wrench"), tabName = "preprocessing"),
                                       menuSubItem("Prediction Algorithms", icon = icon("search"), tabName = "predictionalgos")),
                              menuItem("Source Code", icon = icon("file-code-o"),
                                       href = "https://github.com/jschelbert/capstone/tree/master/app/"),
                              menuItem("Contact", icon = icon("address-card-o"), tabName = "contact")
                          )
                      ),
                      ## Body content
                      dashboardBody(
                          tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                              tags$script(src = "functions.js")
                          ),
                          tabItems(
                              #------------------------------------------------------------------------
                              tabItem(tabName = "widgets",
                                      h2("Predicting words with n-gram models"),
                                      p("This app predicts the next most likely word based on your input. For more info see the", a("background", onclick = "openTab('predictionalgos')"), "tab."),
                                      p("You can try out the prediction algorithm with two different languages."),
                                      br(),
                                      radioButtons("language", "First, select a language:", c("English", "German")),
                                      checkboxInput("smearwordfilter", "Check to filter offensive words.", FALSE),
                                      textInput("usertext", label = "Then enter some text to predict the next word:",
                                                placeholder = "Your text here..."),
                                      p('The following table features the predicted words along with their score/probability. Both stupid-back-off (sb_score) and modfied Kneser-Ney (mkn) values are reported.'),
                                      tableOutput("predictionvalue_sb")
                              ),
                              #------------------------------------------------------------------------
                              tabItem(tabName = "random",
                                      h2("Random babble based on the language model"),
                                      p("Since the prediction algorithm outputs several words along with their estimated probability, we can utilize this to obtain some randomly generated text based on some initial words. You can try this by typing some words and the algorith will extend the text. Please note that our language model does not include punctuation marks, thus, we get only one long sentence."),
                                      p("You can change the language in the", a("Prediction", onclick = "openTab('widgets')"), "tab."),
                                      p("Note that longer random babbles might take some time to compute."),
                                      br(),
                                      #radioButtons("language", "First, select a language", c("English", "German")),
                                      sliderInput("numwordsbabble", "Select the length  of the random babble:",
                                                  min = 100, max = 1000, value = 250, step = 50,
                                                  ticks = FALSE),
                                      textInput("randomseed", label = "Then enter some text to start the random babble:",
                                                placeholder = "Your text here..."),
                                      textOutput("random_babble")
                              ),
                              #------------------------------------------------------------------------
                              tabItem("preprocessing",
                                      h1("Background - Data & Preprocessing"),
                                      includeHTML("www/preprocessing.html")
                              ),
                              tabItem("predictionalgos",
                                      h1("Background - Prediction algorithms"),
                                      includeHTML("www/kneser-ney.html")
                              ),

                              #------------------------------------------------------------------------
                              tabItem(tabName = "contact",
                                      h1("Contact"),
                                      h2("Dr. Jakob Schelbert"),
                                      p("I'm a mathematician with an eagerness to learn new things."),
                                      p("Comming from a mathematical optimization and OR background, I've discovered in the last years the exciting field or machine learning, neural networks and deep learning. I strive to apply my knowledge to make the world a better place and help others to find solutions to their problems with the help of solid mathematical approaches."),
                                      h3("Short CV"),
                                      p("I've studied", a(href="http://www3.mathematik.tu-darmstadt.de", "mathematics at TU Darmstadt"), ". Did my PhD at the", a(href="http://mso.math.fau.de/edom/", "chair of EDOM at FAU Erlangen-Nuremberg"), "where I developed mathematical models to optimize routings of high-pressure pipes in power plants."),
                                      p("Afterwards, I worked at", a(href="https://www.capgemini.com/consulting-de/service/supply-chain-management", "Capgemini Consulting in the Supply Chain Management group"), "for two years. Among other things I developed a procedure for material segmentation with the help of a shiny app to enable business users to directly try out new settings."),
                                      p("Now I'm with", a(href="http://www.deutschebahn.com/db-analytics", "DB Analytics"), "- we are service providers for prediction, simulation and optimization for the Deutsche Bahn - where I'm developing algorithms, prototypes and IT systems primarily for the freight transportation division."),
                                      br(),
                                      p("You can find my professional profile on", a(href="https://www.linkedin.com/in/jakob-schelbert-0b077ba9/", "LinkedIn"), "or", a(href="https://www.xing.com/profile/Jakob_Schelbert", "Xing.")))
                          )
                      )
)
)
