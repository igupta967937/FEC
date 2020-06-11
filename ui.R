library(rsconnect)
library(tidyverse)
library(jsonlite)
library(shiny)

# API DATA
ie.JB <- fromJSON("https://api.open.fec.gov/v1/schedules/schedule_e/by_candidate/?cycle=2020&sort_null_only=true&sort_hide_null=true&election_full=true&api_key=LucFdMRObYBldIpH2P4UEXa8kwGX6cRXYE8M1Vx0&page=1&candidate_id=P80000722&per_page=100&sort_nulls_last=false")
ie.DT <- fromJSON("https://api.open.fec.gov/v1/schedules/schedule_e/by_candidate/?cycle=2020&sort_null_only=true&sort_hide_null=true&election_full=true&api_key=LucFdMRObYBldIpH2P4UEXa8kwGX6cRXYE8M1Vx0&page=1&candidate_id=P80001571&per_page=100&sort=-total&sort_nulls_last=false")
ie.BOTH <- full_join(ie.JB$results,ie.DT$results)

fs.TOTAL <- fromJSON("https://api.open.fec.gov/v1/presidential/financial_summary/?sort_null_only=true&sort_hide_null=true&election_year=2020&api_key=LucFdMRObYBldIpH2P4UEXa8kwGX6cRXYE8M1Vx0&page=1&candidate_id=P80000722&candidate_id=P80001571&per_page=100&sort=-net_receipts&sort_nulls_last=false")

cbs.DT_api <- fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?sort_null_only=false&election_year=2020&candidate_id=P80001571&sort=-contribution_receipt_amount&sort_hide_null=false&api_key=LucFdMRObYBldIpH2P4UEXa8kwGX6cRXYE8M1Vx0&sort_nulls_last=false&page=1&per_page=100")
cbs.JB_api <- fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?sort_null_only=false&election_year=2020&candidate_id=P80000722&sort=-contribution_receipt_amount&sort_hide_null=false&api_key=LucFdMRObYBldIpH2P4UEXa8kwGX6cRXYE8M1Vx0&sort_nulls_last=false&page=1&per_page=100")

cbs.DT <- cbs.DT_api$results %>% select(contribution_state, contribution_receipt_amount)
cbs.JB <- cbs.JB_api$results %>% select(contribution_state, contribution_receipt_amount)



ui <- fluidPage(tabsetPanel(
  tabPanel("DATA",
           
           titlePanel("2020 Presidential Candidate Financial Information"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "President", label = "Choose a Presidential Candidate", choices = fs.TOTAL$results$candidate_name),
               selectInput(inputId = "State", label =  "Choose a State", choices = cbs.DT$contribution_state),
               selectInput(inputId = "Committees", label = "Choose a committee", "-"  )
             ),
             mainPanel(
               plotOutput(outputId = "contribution_plot"),
               tableOutput(outputId = "fSummary"),
               verbatimTextOutput("state_data"),
               tableOutput(outputId = "committee_info")
             )
           )
  ),
  
  tabPanel("Definitions",
           flowLayout(
             p(strong("FEC")),
             textOutput("FEC"),
             p(strong("Joe Bidem")),
             textOutput("JOE"),
             p(strong("Donald Trump")),
             textOutput("DONALD"),
             p(strong("Contributions Per State")),
             textOutput("CPS"),
             p(strong("Total")),
             textOutput("TOTAL"),
             p(strong("Operational Cost")),
             textOutput("OPERATIONS"),
             p(strong("Availalbe Cash")),
             textOutput("AVAILABLE"),
             p(strong("Committee Cash")),
             textOutput("COMMITTEE"),
             p(strong("Counts")),
             textOutput("COUNT"),
             p(strong("Support, or Oppose")),
             textOutput("SOP")
           )
           
  )
)
)
