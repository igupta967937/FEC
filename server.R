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



server <- function(input, output,session) {
  
  
  output$fSummary <- renderTable({
    # PRINTS Total Finance information    
    fs.TOTAL$results %>% 
      select(candidate_id,candidate_name,net_receipts,operating_expenditures,cash_on_hand_end) %>% 
      filter(candidate_name == input$President) %>% 
      rename(Name = candidate_name, Total = net_receipts, Operations = operating_expenditures, Available =  cash_on_hand_end) %>% 
      select(Name, Total,Operations,Available)
  })
  
  # PRININT STATE DATA  
  output$state_data <- renderPrint({
    if (input$President == "BIDEN, JOSEPH R JR") {
      cbs.JB %>% 
        filter(contribution_state == input$State ) %>%  
        select(contribution_state,contribution_receipt_amount) %>%  
        rename(State = contribution_state, Contribution = contribution_receipt_amount)
    } else {
      cbs.DT %>% 
        filter(contribution_state == input$State ) %>%  
        select(contribution_state,contribution_receipt_amount) %>%  
        rename(State = contribution_state, Contribution = contribution_receipt_amount)
    }
  })
  
  # PRINTS THE GRAPH 
  output$contribution_plot <- renderPlot({
    title <- "Contributions Per States"
    
    
    if (input$President == "BIDEN, JOSEPH R JR") {
      ggplot(data = cbs.JB, aes(x=contribution_state, y=as.numeric(contribution_receipt_amount))) +
        geom_bar(stat="identity", color="blue",fill ="white") +
        xlab("States") +
        ylab("Contribution Amount") +
        geom_text(aes(label=contribution_state), vjust=-0.3, size=3) +
        scale_y_continuous(labels = scales::comma) +
        ggtitle(title) +
        theme_minimal() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    } else {
      ggplot(data = cbs.DT, aes(x=contribution_state, y=as.numeric(contribution_receipt_amount))) +
        geom_bar(stat="identity", color="blue",fill ="white") +
        xlab("States") +
        ylab("Contribution Amount") +
        geom_text(aes(label=contribution_state), vjust=-0.3, size=3) +
        scale_y_continuous(labels = scales::comma) +
        ggtitle(title) +
        theme_minimal() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    }
    
  })  
  
  #### UPDATES THE NAMES OF THE COMMITTEES
  observeEvent(input$President, {
    if (input$President == "BIDEN, JOSEPH R JR") {
      Committees <- ie.JB$results %>%
        select(committee_name) %>% rename(Committee = committee_name)
      updateSelectInput(session, "Committees", choices = c("-", ie.JB$results %>% select(committee_name) %>% rename(Committee = committee_name)))
      
    } else {
      Committees <- ie.DT$results %>% 
        select(committee_name) %>% rename(Committee = committee_name)
      updateSelectInput(session, "Committees", choices = c("-", ie.DT$results %>% select(committee_name) %>% rename(Committee = committee_name)))
    }
  })
  
  ### OUTPUTS THE COMMITTEE INFORMATION       
  output$committee_info <- renderTable({
    ie.BOTH %>% 
      select(committee_name,count,total,candidate_name,support_oppose_indicator) %>% 
      filter(committee_name == input$Committees) %>% 
      filter(candidate_name == input$President) %>% 
      select(committee_name,count,total, support_oppose_indicator) %>% 
      rename(Committee = committee_name, Count = count, Donations = total, Support_or_Oppose=support_oppose_indicator )
  })
  
  output$FEC <- renderText("The Federal Election Commission (FEC) is the independent regulatory agency charged with administering and enforcing the federal campaign finance law. The FEC has jurisdiction over the financing of campaigns for the U.S. House, Senate, Presidency and the Vice Presidency.")
  output$JOE <- renderText("Joseph Robinette Biden Jr. is an American politician who served as the 47th vice president of the United States from 2009 to 2017 and represented Delaware in the U.S. Senate from 1973 to 2009. He is running for the Presidency of 2020-2024.")
  output$DONALD <- renderText("Donald John Trump is the 45th and current president of the United States. Before entering politics, he was a businessman and television personality. Trump was born and raised in Queens, a borough of New York City, and received a bachelor's degree in economics from the Wharton School.")
  output$CPS <- renderText("Contribution receipts by state per candidate. These values indicate how much money was contributed to a Campaign per state.")
  output$TOTAL <- renderText("The sum of all contributions and other receipts received by a committee during a filing period.")
  output$OPERATIONS <- renderText("A committee's day-to-day expenditures for items such as rent, overhead, administration, personnel, equipment, travel, advertising and fundraising.")
  output$AVAILABLE <- renderText("Cash-on-hand includes funds held in checking and savings accounts, certificates of deposit, petty cash funds, traveler’s checks, treasury bills and other investments valued at cost. 11 CFR 104.3(a)(1).")
  output$COMMITTEE <- renderText("Any club, association or other group of persons that receives contributions or makes expenditures, either of which aggregate over $1,000 during a calendar year")
  output$COUNT <- renderText("How many times a committee has filed a donation towards a particulate political campaign.")
  output$SOP <- renderText("Wether a campaign contribution was based on the basis of supporting, or opposing a particulate political candidate.")
}