glibrary(shiny)
library(tidyverse)
library(ggplot2)

simulate_war <- function(num_soldiers = 500, 
                         chinese_reserve = 0, french_reserve = 0,
                         french_height_diff = -1, prob_delta_from_height = 0.01){
  
  battle_results <- tibble(chinese = num_soldiers, 
                           french = num_soldiers,
                           battle = 0, 
                           chinese_reserve = chinese_reserve,
                           french_reserve = french_reserve)
  battle_num <- 1
  
  while (TRUE) {
    # cat("\n=== Battle", battle_num, "===\n")
    # cat("Fighting:", num_soldiers, "per side\n")
    # cat("Reserves - Chinese:", chinese_reserve, "French:", french_reserve, "\n")
    
    # Determine how many soldiers fight this round
    # chinese_fighting <- num_soldiers
    # french_fighting <- num_soldiers
    
    # If no soldiers fighting but reserves exist, we need to deploy
    if (num_soldiers == 0) {
      if (chinese_reserve == 0 && french_reserve == 0) {
        #cat("Both sides eliminated!\n")
        break
      }
      
      # Deploy equal numbers from each side (up to what each has)
      deploy <- min(chinese_reserve, french_reserve)
      
      # If one side has no reserves, the other side wins
      if (deploy == 0) {
        #cat("One side has no reserves left - war over!\n")
        break
      }
      
      num_soldiers <- deploy
      chinese_reserve <- chinese_reserve - deploy
      french_reserve <- french_reserve - deploy
      #cat("Deploying from reserves:", deploy, "per side\n")
    }
    
    # Play the battle
    height_soldier <- tibble(chinese = rnorm(num_soldiers, 
                                             65, 2.4),
                             french = rnorm(num_soldiers, 
                                            65 + french_height_diff, 2.4)) %>% 
      mutate(diff = french - chinese,
             prob_french_win = 0.5 + (diff * prob_delta_from_height),
             french_win = rbinom(n(), 1, prob_french_win)) 
    
    battle <- height_soldier %>% 
      pivot_longer(cols = c(chinese, french),
                   names_to = "nationality",
                   values_to = "height") %>% 
      mutate(winner = ifelse((nationality == "french" & french_win == 1) | 
                               (nationality == "chinese" & french_win == 0), TRUE, FALSE)) %>% 
      filter(winner == TRUE)
    
    # Count survivors (with explicit 0s for missing nationalities)
    battle_stats <- battle %>% 
      group_by(nationality) %>% 
      summarize(count = n(), .groups = "drop") %>% 
      pivot_wider(names_from = nationality,
                  values_from = count,
                  values_fill = 0)
    
    # Ensure columns exist even if empty
    if (!"chinese" %in% names(battle_stats)) battle_stats$chinese <- 0
    if (!"french" %in% names(battle_stats)) battle_stats$french <- 0
    
    chinese_survivors <- battle_stats$chinese
    french_survivors <- battle_stats$french
    
    # cat("Survivors - Chinese:", chinese_survivors, "French:", french_survivors, "\n")
    
    # Add reserves and store results
    battle_stats <- battle_stats %>% 
      mutate(battle = battle_num,
             chinese_reserve = chinese_reserve,
             french_reserve = french_reserve)
    
    battle_results <- bind_rows(battle_results, battle_stats)
    
    # Determine next round's fighters
    if (chinese_survivors > french_survivors) {
      chinese_reserve <- chinese_reserve + (chinese_survivors - french_survivors)
      num_soldiers <- french_survivors
    } else if (french_survivors > chinese_survivors) {
      french_reserve <- french_reserve + (french_survivors - chinese_survivors)
      num_soldiers <- chinese_survivors
    } else {
      num_soldiers <- chinese_survivors
    }
    
    # cat("After reserve update - num_soldiers:", num_soldiers, 
    #   "Chinese reserve:", chinese_reserve, "French reserve:", french_reserve, "\n")
    
    
    battle_num <- battle_num + 1
  }
  
  # cat("\n=== FINAL STATE ===\n")
  # cat("Chinese reserve:", chinese_reserve, "\n")
  # cat("French reserve:", french_reserve, "\n")
  
  
  clean_battle_results <- battle_results %>% 
    mutate(chinese = ifelse(is.na(chinese), 0, chinese + chinese_reserve),
           french = ifelse(is.na(french), 0, french + french_reserve),
           chinese_prop = chinese/(chinese + french),
           french_prop = french/(chinese + french)) %>% 
    dplyr::select(-c(french_reserve, chinese_reserve, chinese, french)) %>% 
    pivot_longer(cols = c(chinese_prop, french_prop),
                 names_to = "nationality",
                 values_to = "soldiers")
  
  
  plot_war <- ggplot(data = clean_battle_results, aes(x = battle, y = soldiers, color = nationality)) +
    geom_line(lwd = 1.5, alpha = .7)+
    theme_minimal()+
    scale_color_manual(values = c("coral", "aquamarine2"),
                       labels = c("Chinese", "French"), name = "Army")+
    labs(x = "Battle", y = "% of Total Soldiers Alive")+
    scale_x_continuous(breaks = scales::breaks_width(1))
  
  return(plot_war)
}


ui <- fluidPage(
  titlePanel("War Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_soldiers", "Number of Soldiers:",
                  min = 1000, max = 50000, value = 10000, step = 1000),
      sliderInput("french_height_diff", "French Height Difference:",
                  min = -2, max = 2, value = -0.5, step = 0.1),
      sliderInput("prob_delta", "Increase in Battle Win Probability from a 1 inch Height Advantage:",
                  min = 0, max = 0.1, value = 0.01, step = 0.005)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    simulate_war(num_soldiers = input$num_soldiers,
                 french_height_diff = input$french_height_diff,
                 prob_delta_from_height = input$prob_delta)
  })
}

shinyApp(ui = ui, server = server)