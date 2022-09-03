library(shiny)
library(gt)
library(tidyverse)
library(dplyr)
# Create the user interface
ui <- fluidPage(
  titlePanel("Bracketology"),
  
  mainPanel(
    navbarPage("App and Analysis by Michael Egle (@deceptivespeed_ on Twitter)",
               tabPanel("Men's",
                        fluidRow(
                          numericInput("year", "Year of Tournament", min = 2011, max = 2022, value = 2022),
                          tableOutput(outputId = "mens_tournament_table")
                        )),
               tabPanel("Women's",
                        fluidRow(
                          numericInput("year", "Year of Tournament", min = 2014, max = 2022, value = 2022),
                          tableOutput(outputId = "womens_tournament_table")
                        )))
  )
)

server <- function(input, output, session) {
  # Load all of the CSV files
  m11 <- read_csv("mens_final_projection_2011.csv")
  m12 <- read_csv("mens_final_projection_2012.csv")
  m13 <- read_csv("mens_final_projection_2013.csv")
  m14 <- read_csv("mens_final_projection_2014.csv")
  m15 <- read_csv("mens_final_projection_2015.csv")
  m16 <- read_csv("mens_final_projection_2016.csv")
  m17 <- read_csv("mens_final_projection_2017.csv")
  m18 <- read_csv("mens_final_projection_2018.csv")
  m19 <- read_csv("mens_final_projection_2019.csv")
  m21 <- read_csv("mens_final_projection_2021.csv")
  m22 <- read_csv("mens_final_projection_2022.csv")
  
  w14 <- read_csv("womens_final_projection_2014.csv")
  w15 <- read_csv("womens_final_projection_2015.csv")
  w16 <- read_csv("womens_final_projection_2016.csv")
  w17 <- read_csv("womens_final_projection_2017.csv")
  w18 <- read_csv("womens_final_projection_2018.csv")
  w19 <- read_csv("womens_final_projection_2019.csv")
  w21 <- read_csv("womens_final_projection_2021.csv")
  w22 <- read_csv("womens_final_projection_2022.csv")
  
  mens_logos <- read_csv("mens_logos.csv")
  womens_logos <- read_csv("womens_logos.csv")
  
  output$mens_tournament_table <- render_gt({
    
    data <- m11 %>%
      bind_rows(m12) %>%
      bind_rows(m13) %>%
      bind_rows(m14) %>%
      bind_rows(m15) %>%
      bind_rows(m16) %>%
      bind_rows(m17) %>%
      bind_rows(m18) %>%
      bind_rows(m19) %>%
      bind_rows(m21) %>%
      bind_rows(m22) %>%
      filter(year == input$year)
    
    data <- data %>%
      inner_join(mens_logos)
    
    data <- data %>%
      dplyr::select(logo, team, rank) %>%
      mutate(seed = case_when(rank < 5 ~ 1,
                              rank >= 5 & rank < 9 ~ 2,
                              rank >= 9 & rank < 13 ~ 3,
                              rank >= 13 & rank < 17 ~ 4,
                              rank >= 17 & rank < 21 ~ 5,
                              rank >= 21 & rank < 25 ~ 6,
                              rank >= 25 & rank < 29 ~ 7,
                              rank >= 29 & rank < 33 ~ 8,
                              rank >= 33 & rank < 37 ~ 9,
                              rank >= 37 & rank < 41 ~ 10,
                              rank >= 41 & rank < 45 ~ 11,
                              rank >= 45 & rank < 51 ~ 12,
                              rank >= 51 & rank < 55 ~ 13,
                              rank >= 55 & rank < 59 ~ 14,
                              rank >= 59 & rank < 63 ~ 15,
                              rank >= 63 ~ 16))
    
    data %>%
      gt() %>%
      text_transform(locations = cells_body(c(logo)),
                     fn = function(x)
                     {
                        web_image(
                          url = x,
                          height = px(30)
                        )
                     }) %>%
      cols_label(logo = "",
                 team = "Team",
                 rank = "Overall Seed",
                 seed = "Region Seed") %>%
      tab_header(title = "Men's NCAA Tournament Projection",
                 subtitle = "Projection as of March 1st") %>%
      cols_align("center")
  })
  
  output$womens_tournament_table <- render_gt({
    
    data <- w14 %>%
      bind_rows(w15) %>%
      bind_rows(w16) %>%
      bind_rows(w17) %>%
      bind_rows(w18) %>%
      bind_rows(w19) %>%
      bind_rows(w21) %>%
      bind_rows(w22) %>%
      filter(year == input$year)
    
    data <- data %>%
      inner_join(womens_logos)
    
    if (input$year == 2022)
    {
      data <- data %>%
        dplyr::select(logo, team, rank) %>%
        mutate(seed = case_when(rank < 5 ~ 1,
                                rank >= 5 & rank < 9 ~ 2,
                                rank >= 9 & rank < 13 ~ 3,
                                rank >= 13 & rank < 17 ~ 4,
                                rank >= 17 & rank < 21 ~ 5,
                                rank >= 21 & rank < 25 ~ 6,
                                rank >= 25 & rank < 29 ~ 7,
                                rank >= 29 & rank < 33 ~ 8,
                                rank >= 33 & rank < 37 ~ 9,
                                rank >= 37 & rank < 41 ~ 10,
                                rank >= 41 & rank < 45 ~ 11,
                                rank >= 45 & rank < 51 ~ 12,
                                rank >= 51 & rank < 55 ~ 13,
                                rank >= 55 & rank < 59 ~ 14,
                                rank >= 59 & rank < 63 ~ 15,
                                rank >= 63 ~ 16))
    }
    if (input$year != 2022)
    {
      data <- data %>%
        dplyr::select(logo, team, rank) %>%
        mutate(seed = case_when(rank < 5 ~ 1,
                                rank >= 5 & rank < 9 ~ 2,
                                rank >= 9 & rank < 13 ~ 3,
                                rank >= 13 & rank < 17 ~ 4,
                                rank >= 17 & rank < 21 ~ 5,
                                rank >= 21 & rank < 25 ~ 6,
                                rank >= 25 & rank < 29 ~ 7,
                                rank >= 29 & rank < 33 ~ 8,
                                rank >= 33 & rank < 37 ~ 9,
                                rank >= 37 & rank < 41 ~ 10,
                                rank >= 41 & rank < 45 ~ 11,
                                rank >= 45 & rank < 49 ~ 12,
                                rank >= 49 & rank < 53 ~ 13,
                                rank >= 53 & rank < 57 ~ 14,
                                rank >= 57 & rank < 61 ~ 15,
                                rank >= 61 ~ 16))
    }
    
    
    data %>%
      gt() %>%
      text_transform(locations = cells_body(c(logo)),
                     fn = function(x)
                     {
                       web_image(
                         url = x,
                         height = px(30)
                       )
                     }) %>%
      cols_label(logo = "",
                 team = "Team",
                 rank = "Overall Seed",
                 seed = "Region Seed") %>%
      tab_header(title = "Women's NCAA Tournament Projection",
                 subtitle = "Projection as of March 1st") %>%
      cols_align("center")
  })
}


#shinyApp(ui = ui, server = server)
deployApp(appName = "michael-egle-bracketology")
