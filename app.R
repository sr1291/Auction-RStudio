setwd("/Users/sramasub/Documents/Personal/Reco-Auction/codes/Set-Wise/")

library("shiny")
library("shinydashboard")
library("lubridate")
library("shinyjs")
library("bslib")
library("tableHTML")
library("DT")

thematic::thematic_shiny(font = "auto")

source('functions.R')
source('vars.R')

path <- '/Users/sramasub/Documents/Personal/Reco-Auction/IPL/IPL-2022/Player-List/'
orig_filename <- 'Players-List-With-Retentions-exclUnsold.csv'
new_filename <- 'Players-List-With-Retentions-exclUnsold.csv'

log_filename <- 'Auction-Logs.csv'

players_list <- read_players_list(paste0(path,orig_filename))

player_sets <- unique(players_list$Set)

sold_players <- get_sold_players(players_list)
to_be_auctioned_players <- get_to_be_auctioned_players(players_list)
unsold_players <- get_unsold_players(players_list)

current_bid_global <- 0
current_bidder_global <- NA

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  # The below now creates a custum css class. 
  tags$head(
    tags$style(HTML("
      .nextPlayer .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 1.42857143;
        vertical-align: top;
        border-top: medium solid black;
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel("Auction", fluid = TRUE,
      titlePanel(h1("Reco Auction - T20 World Cup 2021", align = "center")),
      br(),
      
      fluidRow(
          column(5, #offset = 1, style='margin-top:-10px',
                 fluidRow(
                   column(2.5,# width = 4,#align='center',
                       # div(style = "margin-left:600px"),
                       actionButton("generate_next_player", "Reveal Next Player", width = '175px', 
                                    style='margin-left:270px;margin-top:30px;', icon = icon("lock-open"))
                   ),
                   column(2.5, offset = 0.5, style='padding-left:50px;', # width = 4,#align='center',
                       selectizeInput("currentPlayerSet",label = "Current Player Set:",choices = player_sets,
                                      selected = player_sets[1],#width = "70%",
                                      options = list(
                                        placeholder = 'Select Player Set',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )
                       )
                   )
                 ),
                 br(),
                 fluidRow(
                   # tableOutput("nextPlayer"),
                   h3(DT::dataTableOutput("nextPlayer"), 
                      style = "margin-left:175px; font-size: 100%; width: 100%")
                 )
                 
          ),
          column(1.5, offset = 1,# style='border-left: 1px solid black',
                 div(style = "margin-left:-5px;"),
                 numericInput('startingBid','Starting Bid:',value=NA, width = '75%'), #, style = "margin-left:550px"
                 selectizeInput("currentBidder",label = "Current Bidder:",choices = teams,
                                selected = 0,width = "75%",
                                options = list(
                                placeholder = 'Select bidder',
                                onInitialize = I('function() { this.setValue(""); }')
                                )
                              ),
                 numericInput('currentBid','Current Bid:',value=NA, width = '75%')
          ),
          column(1,
                 div(style = "margin-top:55px; margin-left:10px"),
                 actionButton("sold", "Sold", width = '100px', icon = icon("hammer"), 
                              style = "margin-top:25px; margin-left:10px;"),
                 br(),
                 br(),
                 actionButton("unsold", "Unsold", width = '100px', icon = icon("recycle"), 
                              style = "margin-top:20px; margin-left:10px;")
          ),
          
          column(3,
                 div(style = "margin-left:50px;"),
                 actionButton('start','Start', icon = icon("play-circle"),style="margin-left:50px; margin-top: 30px;"),
                 actionButton('stop','Stop', icon = icon("stop"),style="margin-top:30px;"),
                 actionButton('reset','Reset', icon = icon("redo"),style="margin-top:30px;"),
                 br(),
                 br(),
                 tags$div(id = 'time_left',class = 'error', style='margin-left:50px',
                          numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1, width = '80%')
                        ),
                 tags$div(id = 'time_left',class = 'error',textOutput('timeleft'), style='margin-left:50px')
          )
        ),
      
      tags$hr(style="border-color: black; height: 1px;"),
      
      fluidRow(
      column(6, style='border-right: 1px solid black',
             h3('Next bidder', align='center'),
             br(),
             sidebarPanel(width = 12, 
                          div(style = "margin-left:-0px"),
                          splitLayout(
                            actionButton("bidTeam1", teams[1], width = '125px'),
                            actionButton("bidTeam2", teams[2], width = '125px'),
                            actionButton("bidTeam3", teams[3], width = '125px'),
                            actionButton("bidTeam4", teams[4], width = '125px')
                          ),
                          br(),
                          
                          splitLayout(  # cellWidths = c("25%", "25%", "25%", "25%")
                            actionButton("bidTeam5", teams[5], width = '125px'),
                            actionButton("bidTeam6", teams[6], width = '125px'),
                            actionButton("bidTeam7", teams[7], width = '125px'),
                            actionButton("bidTeam8", teams[8], width = '125px')
                          )
             )
      ),
      
      column(6,style='border-right: 1px solid black',#style='margin-bottom:30px;border-left:1px solid; padding: 5px;',
             h3('Recent Players', align='center'),
             h3(DT::dataTableOutput("prevPlayer"), style = "font-size: 100%; width: 100%")
          ),
      # column(2,
      #        h3('Counter Log', align='center'),
      #        h3(DT::dataTableOutput("counterLog"), style = "font-size: 100%; width: 100%")
      # )
      
        ),
      #hr(),
      tags$hr(style="border-color: black; height: 1px;"),
      fluidRow(
        column(6,style='margin-bottom:30px;border-right:1px solid; padding: 5px;',
               h3("Team Composition", align='center'),
               br(),
               br(),
               h3(DT::dataTableOutput("teamComp"), style = "font-size: 100%; width: 100%")
        ),
        
        
        column(6,style='margin-bottom:30px;border-right:1px solid; padding: 5px;',
               fluidRow(
                 column(3, offset=2,#style='margin-left:30px;',
                        h3("Team List")
                        ),
                 column(4,             
                   selectizeInput("auctionTeam", 
                               label = "",
                               choices = teams,
                               selected = "",
                               width = "400px",
                               options = list(
                                 placeholder = 'Select team',
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
                    ),
                 )
               ),
             h3(DT::dataTableOutput("teamList"), style = "font-size: 100%; width: 100%")
          ),
        ),
      tags$hr(style="border-color: black; height: 1px;"),
        
      # fluidRow(
      #   column(8, style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
      #          h3("List of auctioned players, including unsold!"),
      #          br(),
      #          h3(DT::dataTableOutput("auctionedList"), style = "font-size: 100%; width: 100%")
      #   )
      # ),
    ),

    tabPanel("Logs", fluid = TRUE,      
      fluidRow(
        column(8, style = "scroll;overflow-x: scroll;", #height:500px; overflow-y: 
               h3("Auctioned List"),
               br(),
               h3(DT::dataTableOutput("auctionedList"), style = "font-size: 100%; width: 100%")
               # box(
               #   title = "Auctioned List",
               #   width = NULL,
               #   status = "primary",
               #   solidHeader = TRUE,
               #   collapsible = TRUE,
               #   div(style = 'overflow-x: scroll', DT::dataTableOutput('auctionedList'))
               # )
        )
      )
    ),
  )
)

server <- function(input, output, session) {

  # bs_themer()
  
  currentBidder <- reactiveValues(
    all = players_list,
    sold = sold_players,
    unsold = unsold_players,
    to_be_auctioned = to_be_auctioned_players,
    # team_comp = get_team_composition(players_list),
    team_comp = update_team_composition(players_list),
    
    next_player = data.frame(),
    starting_bid = NA,

    bidderTeamNum = 0,
    currentBid = NA,

    prev_player = data.frame(),
    prevBidderTeamNum = NA,

    player_num = 0,
    player_action_num = 0,
    auction_log = data.frame(),
    counter_log = data.frame()

  )

  hide("nextPlayer")
  hide("prevPlayer")

  lapply(sprintf("bidTeam%s", 1:length(teams)), disable)
  disable("startingBid")
  disable("currentBidder")
  disable("currentBid")

  disable("sold")
  disable("unsold")

    
  # ######################## NEXT PLAYER FOR AUCTION ########################
  observe({
    observeEvent(input$generate_next_player, {
      show("nextPlayer")
      currentBidder$next_player <- get_next_player(paste0(path,new_filename), currentBidder$currentPlayerSet)
      currentBidder$starting_bid <- currentBidder$next_player$BasePrice
      updateNumericInput(session, "startingBid", value = currentBidder$starting_bid)
      shinyjs::disable("startingBid")
      shinyjs::disable("currentPlayerSet")
      
      currentBidder$player_num <- currentBidder$player_num + 1
      currentBidder$player_action_num <- currentBidder$player_action_num + 1

      output <- add_action_to_log(currentBidder$auction_log, currentBidder$counter_log,
                                  currentBidder$player_num, currentBidder$next_player$Name,
                                  currentBidder$player_action_num, "Revealed", NA, Sys.time(), currentBidder$starting_bid)

      currentBidder$auction_log <- output[[1]]
      currentBidder$counter_log <- output[[2]]
      
      currentBidder$bidderTeamNum <- 0
      currentBidder$currentBid <- NA
      
      lapply(sprintf("bidTeam%s", 1:length(teams)), enable)
      enable("unsold")
      enable("currentBidder")
      enable("currentBid")
      
      disable("generate_next_player")
    }
    )

  })
  
  output$nextPlayer <- DT::renderDataTable(currentBidder$next_player,options=list(scrollX=T, dom = 't'), rownames= FALSE)
  output$prevPlayer <- DT::renderDataTable(currentBidder$prev_player,options=list(scrollX=T, dom = 't'), rownames= FALSE)
  
  ######################################################################
  
  ######################## BIDDING #######################################
  observe({
    lapply(sprintf("bidTeam%s", 1:length(teams)),
           function(x)
           {
             observeEvent(input[[x]],{
               newBidTeamNum <- as.numeric(sub("bidTeam", "", x))
               if (newBidTeamNum != currentBidder$bidderTeamNum){
                 currentBidder$bidderTeamNum <- newBidTeamNum
                 currentBidder$currentBid <- get_next_bid(currentBidder$currentBid, currentBidder$starting_bid)
                 
                 currentBidder$player_action_num <- currentBidder$player_action_num + 1
                 output <- add_action_to_log(currentBidder$auction_log, currentBidder$counter_log,
                                             currentBidder$player_num, currentBidder$next_player$Name,
                                             currentBidder$player_action_num, 'Bid',
                                             teams[currentBidder$bidderTeamNum], Sys.time(), currentBidder$currentBid)

                 currentBidder$auction_log <- output[[1]]
                 currentBidder$counter_log <- output[[2]]
                 
                 updateSelectInput(session, "currentBidder", selected = teams[currentBidder$bidderTeamNum])
                 updateNumericInput(session, "currentBid", value = currentBidder$currentBid)
                 
                 enable("sold")
                 disable("unsold")
               }
             }
             )
           })
  })
  
  observeEvent(input$currentBidder, {
    currentBidder$bidderTeamNum <- match(input$currentBidder, teams)
  })
  observeEvent(input$currentBid, {
    currentBidder$currentBid <- input$currentBid
  })
  observeEvent(input$currentPlayerSet, {
    currentBidder$currentPlayerSet <- input$currentPlayerSet
  })
  
  ########################################################################
  
  ######################## SOLD #######################################
  observe({
    observeEvent(input$sold, {
      playerName <- currentBidder$next_player$Name
      cols = c("Auctioned","Sold","Owner","Price")
      values = c("Yes","Yes",teams[currentBidder$bidderTeamNum],currentBidder$currentBid)
      currentBidder$all[(currentBidder$all$Name==playerName),cols] <- values
      write.csv(currentBidder$all, paste0(path,new_filename), row.names = FALSE)
      currentBidder$all <- read_players_list(paste0(path,new_filename))
      sold_players <- subset(currentBidder$all, Sold=="Yes")
      
      # currentBidder$team_comp <- update_team_composition(currentBidder$team_comp, currentBidder$next_player,
      #                                                    teams[currentBidder$bidderTeamNum], currentBidder$currentBid)
      currentBidder$team_comp <- update_team_composition(currentBidder$all)
      
      currentBidder$player_action_num <- currentBidder$player_action_num + 1
      output <- add_action_to_log(currentBidder$auction_log, currentBidder$counter_log,
                                  currentBidder$player_num, currentBidder$next_player$Name,
                                  currentBidder$player_action_num, 'Sold',
                                  teams[currentBidder$bidderTeamNum], Sys.time(), currentBidder$currentBid)
      
      currentBidder$auction_log <- output[[1]]
      currentBidder$counter_log <- output[[2]]
      
      currentBidder$player_action_num <- 0
      write.csv(currentBidder$auction_log, paste0(path,log_filename), row.names = FALSE)

      curr_player_updated <- subset(currentBidder$all, Name==playerName)
      currentBidder$prev_player <- append_prev_player(curr_player_updated, currentBidder$prev_player)
      
      currentBidder$bidderTeamNum <- 0
      currentBidder$currentBid <- NA
      
      updateTextInput(session, "startingBid", value = NA)
      updateTextInput(session, "currentBidder", value = NA)
      updateNumericInput(session, "currentBid", value = NA)
      
      hide("nextPlayer")
      show("prevPlayer")
      
      lapply(sprintf("bidTeam%s", 1:length(teams)), disable)
      disable("startingBid")
      disable("currentBidder")
      disable("currentBid")
      
      disable("sold")
      disable("unsold")
      enable("generate_next_player")
      enable("currentPlayerSet")
    }
    )
  })
  ########################################################################
  
  ######################## UNSOLD #######################################
  observe({
    observeEvent(input$unsold, {
      playerName <- currentBidder$next_player$Name
      cols = c("Auctioned","Sold")
      values = c("Yes","No")
      currentBidder$all[(currentBidder$all$Name==playerName),cols] <- values
      write.csv(currentBidder$all, paste0(path,new_filename), row.names = FALSE)
      currentBidder$all <- read_players_list(paste0(path,new_filename))
      curr_player_updated <- subset(currentBidder$all, Name==playerName)
      currentBidder$prev_player <- append_prev_player(curr_player_updated, currentBidder$prev_player)
      
      currentBidder$player_action_num <- currentBidder$player_action_num + 1
      output <- add_action_to_log(currentBidder$auction_log, currentBidder$counter_log,
                                  currentBidder$player_num, currentBidder$next_player$Name,
                                  currentBidder$player_action_num, 'Unsold',
                                  NA, Sys.time(), NA)
      currentBidder$auction_log <- output[[1]]
      currentBidder$counter_log <- output[[2]]

      currentBidder$player_action_num <- 0
      write.csv(currentBidder$auction_log, paste0(path,log_filename), row.names = FALSE)
      
      currentBidder$bidderTeamNum <- 0
      currentBidder$currentBid <- NA
      
      updateTextInput(session, "startingBid", value = NA)
      updateTextInput(session, "currentBidder", value = NA)
      updateNumericInput(session, "currentBid", value = NA)
      
      hide("nextPlayer")
      show("prevPlayer")
      
      lapply(sprintf("bidTeam%s", 1:length(teams)), disable)
      disable("startingBid")
      disable("currentBidder")
      disable("currentBid")
      
      disable("sold")
      disable("unsold")
      enable("generate_next_player")
      enable("currentPlayerSet")
    }
    )
  })
  ######################################################################
  
  ######################## OWNER'S TEAM COMPOSITION ##############################
  output$teamComp <- DT::renderDataTable(currentBidder$team_comp,options=list(scrollX=T, dom = 't'))
  ########################################################################
  
  ######################## OWNER'S TEAM LIST ##############################
  teamList <- eventReactive(c(input$sold, input$auctionTeam), {
    cols <- c("Name","Team","Country","Skillset","Price")
    subset(currentBidder$all, Owner==input$auctionTeam)[,cols]
  })
  output$teamList <- DT::renderDataTable(teamList(),options=list(scrollX=T), rownames= FALSE)
  
  ########################################################################
  
  ######################## AUCTIONED PLAYERS LIST ##############################
  auctionedListCols <- c("Name","Team","Country","Skillset","BasePrice","Sold","Owner","Price")
  output$auctionedList <- DT::renderDataTable(subset(currentBidder$all, Auctioned=="Yes")[,auctionedListCols],
                                              options=list(scrollX=T), rownames= FALSE)
  
  ########################################################################
  output$auctionLog <- DT::renderDataTable(currentBidder$auction_log,options=list(scrollX=T), rownames= FALSE)
  output$counterLog <- DT::renderDataTable(currentBidder$counter_log,options=list(scrollX=T, dom = 't'), rownames= FALSE)
  # output$auctionLog <- DT::renderDataTable({DT::datatable(currentBidder$auction_log, rownames = FALSE) %>%formatStyle(columns=colnames(currentBidder$auction_log),background = 'white',color='black')})
  
  ######################## TIMER ##########################################
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
  
  ########################################################################
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
