# Install and load necessary libraries
install.packages(c("shiny", "shinydashboard", "DT"))
library(shinydashboard)
library(shiny)
library(DT)

# Define UI
ui <- dashboardPage(
  skin = "black",  # Set the skin to black
  dashboardHeader(
    title = tags$span(
      style = "color: teal; font-weight: bold;",
      "Looking Ahead: Dashboard"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "home_page", icon = icon("home")),
      menuItem("Slider App", tabName = "slider_app", icon = icon("sliders")),
      menuItem("Bill Tracker", tabName = "bill_tracker", icon = icon("file-text"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Page Tab
      tabItem(
        tabName = "home_page",
        fluidPage(
          titlePanel("Welcome to the Looking Ahead Dashboard"),
          tags$p(
            "This dashboard provides tools to analyze and project oil consumption based on various lifestyle factors.",
            br(), br(),
            "Navigate through the tabs to access different features and functionalities."
          ),
          fluidRow(
            column(12, align = "center",
                   uiOutput("blog_button")
            )
          )
        )
      ),
      
      # Slider App Tab
      tabItem(
        tabName = "slider_app",
        fluidPage(
          options = list(height = 40),
          tags$p(
            HTML("Often forgotten is the large quantity of oil barrels that the state of Hawaii imports from abroad. According to the Hawaii State Energy Office, “the amount of fuel imported into Hawaiʻi exceeds the amount of any other product imported into the state,” even exceeding goods such as inbound air cargo, food, and manufactured goods. The sole oil refinery is located in Honolulu, and it can process 94,000 barrels of crude oil per day, which is then burned and contributes to a large portion of Hawaii's carbon emissions.<br><br>This model is intended to aid legislators and community members in “looking ahead.” The slider model projects the total barrels of oil an individual's lifestyle would contribute to, in regards to their transportation and residential choices.<br>
                 <br>In addition, this tool can help HECO and state legislators model policies, such as energy prices and investments in fuel-efficient vehicles.<br><br>")
          ),
          fluidRow(
            column(6,
                   sliderInput("commutetime", "If commute time is", min = 1, max = 150, value = 26),  # Updated value
                   verbatimTextOutput("threshold_message_commute"),
                   selectInput("car_model", "Select Car Model", 
                               choices = c("Toyota Tacoma", "Toyota 4Runner", "Toyota Rav4", "Honda CR-V", "Tesla Model Y", "Honda Civic", "Honda HR-V", "Tesla Model 3", "Toyota Camry", "Toyota Rav 4 Hybrid", "Subaru Outback", "Jeep Wrangler", "Mazda MX-30", "Chevrolet Tahoe", "Nissan Rogue", "Ford Explorer", "Kia Optima Hybrid", "Toyota Prius", "Honda Insight", "Ford Escape", "Nissan Leaf"),
                               selected = "Toyota Tacoma"),  # Initial selection for dropdown
                   HTML("<strong>then, the total amount of 42-gallon barrels of oil is</strong>"), 
                   HTML("<span style='font-size: 18px; font-weight: bold;'>"), 
                   textOutput("product1"),
                   HTML("</span>"),
                   fluidRow(
                     column(6, align = "center",
                            actionButton("reset_button_left", "Reset to Baseline (Left Sliders)", 
                                         style = "color: white; background-color: black; font-size: 16px; padding: 10px; margin-top: 20px; margin-bottom: 20px;")
                     )
                   )
            ),
            column(6,
                   sliderInput("energycost", "If energy cost is", min = 1, max = 500, value = 321),  # Updated value
                   verbatimTextOutput("threshold_message_energy"),
                   sliderInput("electricityrate", "and electricity rate per kWh is", min = 0.005, max = 5, value = 0.2874),  # Updated value
                   verbatimTextOutput("threshold_message_electricity"),
                   HTML("<strong>then, the total amount of 42-gallon barrels of oil is</strong>"), 
                   HTML("<span style='font-size: 18px; font-weight: bold;'>"), 
                   textOutput("product2"),
                   HTML("</span>"),
                   fluidRow(
                     column(6, align = "center",
                            actionButton("reset_button_right", "Reset to Baseline (Right Sliders)", 
                                         style = "color: white; background-color: black; font-size: 16px; padding: 10px; margin-top: 20px; margin-bottom: 20px;")
                     )
                   )
            )
          )
        )
      ),
      
      # Bill Tracker Tab
      tabItem(
        tabName = "bill_tracker",
        fluidPage(
          titlePanel("Bill Tracker"),
          tags$p(
            "Today, it is easy to be overwhelmed by the abundance of news articles that discuss the extinction of various 
            species and the global destruction caused by natural disasters, parallel to the political and industry-based 
            resistance to climate-friendly solutions and legislation.",
            br(), br(),
            "Therefore, it is more important than ever to wield political will to drive legislation, 
            in various areas such as food systems, climate justice, infrastructure, and economy. 
            This bill tracker tool helps citizens monitor bills in the climate sector for the 2024 Session of the 
            Hawai'i State Legislature, turning dialogue into real action.",
            br(), br()
          ),
          fluidRow(
            column(12,
                   DTOutput("bill_table")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Map car model names to numeric values
  car_model_values <- c("Toyota Tacoma" = 21, "Toyota 4Runner" = 17, "Toyota Rav4" = 30, "Honda CR-V"= 40, "Tesla Model Y"= 122, "Honda Civic"= 52, "Honda HR-V"= 30, "Tesla Model 3"= 134, "Toyota Camry"= 26, "Toyota Rav 4 Hybrid"= 40, "Subaru Outback"= 29, "Jeep Wrangler"= 24, "Mazda MX-30"= 92, "Chevrolet Tahoe"= 17, "Nissan Rogue"= 33, "Ford Explorer"= 24, "Kia Optima Hybrid"= 101, "Toyota Prius"= 57, "Honda Insight"= 52, "Ford Escape"= 105, "Nissan Leaf"= 111)
  
  output$product1 <- renderText({
    product1 <- input$commutetime * car_model_values[input$car_model] * 4.090625 * 0.00238038562
    product1
  })
  output$product2 <- renderText({
    product2 <- input$energycost * input$electricityrate * 0.741 * 0.00238038562
    product2
  })
  
  # Add threshold logic for sliders in slider_app tab
  output$threshold_message_commute <- renderText({
    if (input$commutetime > 26) {
      "Above average commute time (26.1 minutes), according to 2022 American Community Survey 1-Year Estimates."
    } else {
      ""
    }
  })
  
  output$threshold_message_commute <- renderText({
    if (input$commutetime < 26) {
      "Below average commute time (26.1 minutes), according to 2022 American Community Survey 1-Year Estimates."
    } else {
      ""
    }
  })
  
  output$threshold_message_energy <- renderText({
    if (input$energycost > 321) {
      "Above average monthly energy bill ($321), according to US Energy Information Administration."
    } else {
      ""
    }
  })
  
  output$threshold_message_energy <- renderText({
    if (input$energycost < 321) {
      "Below average monthly energy bill ($321), according to US Energy Information Administration."
    } else {
      ""
    }
  })
  
  output$threshold_message_electricity <- renderText({
    if (input$electricityrate > 0.3) {
      "Above average residential electricity rate, which is $0.2874 per kWh, according to HECO."
    } else {
      ""
    }
  })
  
  output$threshold_message_electricity <- renderText({
    if (input$electricityrate < 0.25) {
      "Above average residential electricity rate, which is $0.2874 per kWh, according to HECO."
    } else {
      ""
    }
  })
  
  # Reset button logic for left sliders
  observeEvent(input$reset_button_left, {
    updateSliderInput(session, "commutetime", value = 26)
    updateSelectInput(session, "car_model", selected = "Toyota Tacoma")  # Reset car model dropdown
  })
  
  # Reset button logic for right sliders
  observeEvent(input$reset_button_right, {
    updateSliderInput(session, "energycost", value = 321)
    updateSliderInput(session, "electricityrate", value = 0.2874)
  })
  
  # Render the "Go Back Home" button
  output$blog_button <- renderUI({
    tags$a(
      tags$button(
        "Go Back Home",
        style = "color: white; background-color: black; font-size: 18px; padding: 10px; margin-top: 20px; margin-bottom: 20px;"
      ),
      href = "https://jshslookingahead.wordpress.com/",
      target = "_blank"
    )
  })
  
  # Bill Tracker Table
  bills <- data.frame(
    Bill = c("Senate Bill 2525", "Senate Bill 430","House Bill 2408", "House Bill 248", "House Bill 250","House Bill 332", "House Bill 540","House Bill 247", 
             "House Bill 199", "Senate Bill 965", "Senate Bill 1154", "House Bill 839", "House Bill 837", "House Bill 441", "Senate Bill 657", "Senate Bill 304"),
    Status = c("Deferred", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee",
               "In Committee", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee", "In Committee"),
    Blurb = c("Carbon Cashback","Farm to Food Bank Program", "Climate Advisor","Farm to School Program", "Public School Food Programs", "Shade and Fruit Tree Program", "Universal Free School Lunches and Breakfasts", "Agriculture Benchmarks",
              "Zero Emission Vehicles", "Green Infrastructure", "Public Utilities Commission and Wheeling", "Affordable Housing", "Green Infrastructure", "Climate Equity and Databases", "Climate Equity and Databases", "Green Fee"),
    Information = c("Amends the environmental response, energy, and food security tax to address carbon emissions. Incrementally increases the tax rate over time. Establishes a refundable tax credit to mitigate the effect of a carbon emissions tax on taxpayers. Requires reports to the Legislature.",
                     "Establishes the Hawaiʻi Farm to Food Bank Program and Hawaiʻi food assistance program special fund to alleviate food shortages in the State. Appropriates funds. Effective 6/30/3000.",
                     "Establishes the senior advisor on climate position within the Office of Planning and Sustainable Development. Makes the senior advisor on climate a member of the Hawaii Climate Change Mitigation and Adaptation Commission.",
                     "Requires the department of education to fully implement the farm to school program and farm to school meals program by the 2024-2025 school year. Provides that complex area superintendents have the authority to implement the farm to school and farm to school meals programs. Requires certain school cafeteria supervisors to report directly to complex area superintendents. Establishes school cafeteria supervisor positions. Requires the office of talent management of the department of education to conduct a compensation review of school cafeteria supervisor positions. Appropriates funds. Effective 7/1/3000. (HD2)",
                     "Requires the department of education to establish rules for the procurement of goods and services related to the administration of food programs at public schools that incorporate a geographic preference for unprocessed locally grown and locally raised food products. Effective 7/1/3000. (HD1)",
                     "Establishes the shade and fruit tree program in the department of education to support educational activities and encourage propagation of native shade trees or fruit trees for planting in department schools. Appropriates funds for the program and to establish an arborist position. Effective 6/30/3000. (HD1)", 
                     "Beginning with the 2023-2024 school year, requires the department of education and public charter schools to provide free breakfast and lunch to all enrolled students. Appropriates funds. Effective 6/30/3000. (HD1)",
                     "Increases the percentages of local agricultural products that certain departments are required to purchase by certain deadlines. Expands annual reporting requirements to include the total spending by certain market channels. Requires the University of Hawaiʻi system to include a corrective action plan in its respective report to the legislature, should it not meet its benchmark. Repeals the separate benchmark for the department of education that requires thirty per cent of food served in public schools to consist of locally sourced products by 2030. Appropriates funds for the establishment of a farm-to-state liaison within the office of the governor. Effective 6/30/3000. (HD1)",
                     "Establishes a zero-emission vehicle fleet purchasing assistance program within the Hawaiʻi state energy office to support the transition of private fleets to zero-emission vehicles. Designates the Hawaiʻi state energy office as the agency responsible for developing and administering the program and requires the Hawaiʻi state energy office to work with the department of transportation to design strategies to implement the zero-emission vehicle fleet purchasing assistance program. Effective 7/1/3000. (HD1)",
                     "Establishes green infrastructure objectives and policies for transportation systems, infrastructure, and projects. Establishes the green transportation infrastructure task force to examine, evaluate, and develop policies for the design, implementation, and maintenance of green transportation infrastructure. Effective 6/30/3000. (HD1)",
                     "Requires the public utilities commission to open a docket and set a procedural schedule to determine whether and by which rules government agencies may engage in wheeling of electricity that is produced by renewable energy sources from their own facilities to another government agency's facilities, subject to certain restrictions. Effective 6/30/3000. (HD1)",
                     "Requires the Hawaiʻi Housing Finance and Development Corporation or an eligible developer to prepare an environmental impact statement for any proposed housing project on lands zoned as preservation or conservation by the applicable county.",
                     "Establishes green infrastructure objectives, policies, and priority guidelines for state facility systems, infrastructure, transit projects, and other areas in the Hawaiʻi State Planning Act to improve the quality of life for residents and visitors. Adds definition of green infrastructure. Requires the Office of Planning and Sustainable Development, in partnership with the Greenhouse Gas Sequestration Task Force, to submit a report to the Legislature making recommendations for implementing the green infrastructure objectives, policies, and priority guidelines. Establishes full-time equivalent ( .0 FTE) policy analyst positions within the Office of Planning and Sustainable Development. Appropriates funds. Effective 6/30/3000. (SD2)",
                     "Appropriates funds for the development of a database and data portal from data sources detailed in the May 2022 report on social vulnerability to climate change in Hawaiʻi, under certain conditions. Appropriates funds for the maintenance and updating of the data portal. Effective 6/30/3000. (HD1)",
                     "Appropriates funds for fiscal year 2023-2024 for the development of a database and data portal from data sources detailed in the May 2022 report on social vulnerability to climate change in Hawaiʻi to the Hawaiʻi climate change mitigation and adaptation commission and the accompanying guide. Appropriates funds for fiscal year 2024-2025 for the maintenance and updating of the data portal.",
                     "Effective 7/1/2025, establishes the visitor impact fee program within the department of land and natural resources, through which the department will collect a fee from visitors for a license to visit a state park, forest, hiking trail, or other state natural area. Establishes the visitor impact fee special fund effective 7/1/2023. Requires report to the legislature on strategic plan and timetable for objectives and implementation of the visitor impact fee program. Appropriates funds for the visitor impact fee strategic plan and positions for the visitor impact fee program. Effective 6/30/3000. (HD3)")
  )
  
  output$bill_table <- renderDT({
    datatable(
      bills,
      options = list(
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      ),
      rownames = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)

