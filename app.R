library(shiny)
library(randomForest)
library(gridExtra)

rf_model <- readRDS("TDM_Premier_Draft_Full_RF_mod.rds")
expected_features <- names(rf_model$forest$xlevels)
# models_by_quintile <- lapply(1:10, function(i) {
#     readRDS(paste0("deck_model_skill_quintile_", i, ".rds"))
# })


parse_deck_text <- function(deck_text) {
    # split into lines and remove header lines
    lines <- strsplit(deck_text, "\n")[[1]]
    lines <- trimws(lines)
    # isolate lines between "Deck" and "Sideboard"
    deck_start <- which(grepl("^Deck$", lines))
    side_start <- which(grepl("^Sideboard$", lines))
    if (length(deck_start) == 0) deck_start <- 0
    if (length(side_start) == 0) side_start <- length(lines) + 1
    lines <- lines[!grepl("^Deck|^Sideboard|^\\s*$", lines)]
    
    
    # extract card names and counts
    parsed <- strcapture("^(\\d+)\\s+(.+)$", lines, proto = data.frame(count = integer(), card = character()))
    
    # standardize card names: replace special characters and whitespace with '.'
    parsed$card <- gsub("[^A-Za-z0-9]", ".", parsed$card)
    
    aggregate(count ~ card, parsed, sum)
}

format_deck_for_model <- function(parsed_df, expected_features) {
    # initialize zero row
    row <- setNames(rep(0, length(expected_features)), expected_features)
    
    # fill in counts
    for (i in seq_len(nrow(parsed_df))) {
        card <- parsed_df$card[i]
        if (card %in% expected_features) {
            row[card] <- parsed_df$count[i]
        }
    }
    
    as.data.frame(t(row))
}

ui <- fluidPage(
    titlePanel("Tarkir Premier Draft Deck Strength Estimator"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Deck List (optional, .csv)", accept = ".csv"),
            tags$hr(),
            textAreaInput("deck_text", "Paste Your Deck List", rows = 20,
                          placeholder = "Deck\n1 Plains\n2 Island\n...\nSideboard\n1 Some Card"),
            actionButton("predict", "Estimate Strength")
        ),
        mainPanel(
            tags$h4("Model Calibration"),
            tags$p("This plot shows how closely the model's predicted win rates match observed win rates."),
            tags$p("Each red dot represents a group of similar decks. If the model predicts they’ll win 60% of the time, and they do, the dot lies on the dashed gray line."),
            tags$p("The blue curve is a smoothed average. If it hugs the dashed line, the model’s predictions are well-calibrated."),
            tags$p("Your deck's prediction is shown as a green dotted line."),
            plotOutput("calibration_plot"),
            
            tableOutput("record_table"),
            verbatimTextOutput("prediction"),
            tableOutput("deck_matrix"),
            tags$h4("Skill Quintile Predictions and Percentiles"),
            p("This table shows your deck's predicted win rate in each skill tier, and how it ranks compared to predicted win rates of decks drafted and played by players in that tier. Tiered models are trained on data from players in that tier only."),
            tableOutput("quintile_predictions"),
            tags$hr()
            
        )
    )
)

server <- function(input, output) {
    model_cache <- reactiveValues()
    
    get_model <- function(i) {
        key <- as.character(i)
        if (is.null(model_cache[[key]])) {
            model_cache[[key]] <- readRDS(paste0("deck_model_skill_quintile_", i, ".rds"))
        }
        model_cache[[key]]
    }
    
    
    parsed_deck <- eventReactive(input$predict, {
        req(input$deck_text)
        parse_deck_text(input$deck_text)
    })
    
    formatted_deck <- reactive({
        req(parsed_deck())
        format_deck_for_model(parsed_deck(), expected_features)
    })
    
    output$prediction <- renderPrint({
        req(formatted_deck())
        pred <- predict(rf_model, newdata = formatted_deck())
        paste0("Predicted Win Rate: ", round(pred, 3))
    })
    
    output$quintile_predictions <- renderTable({
        req(formatted_deck())
        
        tier_ids <- c(3, 7, 10)
        tier_labels <- c("Novice", "Intermediate", "Expert")
        
        withProgress(message = "Predicting...", value = 0, {
            preds <- numeric(length(tier_ids))
            percentiles <- character(length(tier_ids))
            meansd <- character(length(tier_ids))
            
            for (j in seq_along(tier_ids)) {
                i <- tier_ids[j]
                incProgress(1 / length(tier_ids), detail = paste("Evaluating", tier_labels[j]))
                mod <- get_model(i)
                
                # prediction
                p <- predict(mod, newdata = formatted_deck())
                preds[j] <- round(p, 3)
                
                # percentile
                percentiles[j] <- paste0(round(mean(mod$predicted < p, na.rm = TRUE) * 100), "%")
                
                # mean ± sd
                m <- mean(mod$predicted, na.rm = TRUE)
                s <- sd(mod$predicted, na.rm = TRUE)
                meansd[j] <- paste0(round(m, 3), " ± ", round(s, 3))
            }
            
            data.frame(
                `Skill Tier` = tier_labels,
                `Predicted WR` = preds,
                `Percentile` = percentiles,
                `Mean ± SD` = meansd
            )
        })
    })
    
    
    
    
    
    
    
    
    output$calibration_plot <- renderPlot({
        req(formatted_deck())
        
        # extract OOB predictions
        oob_preds <- rf_model$predicted
        oob_obs <- rf_model$y
        df <- data.frame(pred = oob_preds, obs = oob_obs)
        
        library(dplyr)
        library(ggplot2)
        library(mgcv)
        
        # bin for calibration curve
        binned <- df %>%
            mutate(bin = ntile(pred, 50)) %>%
            group_by(bin) %>%
            summarise(mean_pred = mean(pred),
                      mean_obs = mean(obs),
                      .groups = 'drop')
        
        # predicted win rate
        user_pred <- predict(rf_model, newdata = formatted_deck())
        
        # simulate theoretical record distribution (7 games)
        states <- expand.grid(W = 0:7, L = 0:3)
        states <- states[!(states$W < 7 & states$L < 3), ]  # absorbing states only
        
        states$P <- mapply(function(w, l) {
            choose(w + l, w) * user_pred^w * (1 - user_pred)^l
        }, states$W, states$L)
        
        record_df <- aggregate(P ~ W, data = states, sum)
        colnames(record_df) <- c("Record", "Probability")
        
        # plot
        p1 <- ggplot(df, aes(x = pred, y = obs)) +
            geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, color = "blue", fill = "lightblue") +
            geom_point(data = binned, aes(x = mean_pred, y = mean_obs), size = 2, color = "red") +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            geom_vline(xintercept = user_pred, color = "darkgreen", linetype = "dotted") +
            labs(x = "Predicted Win Rate", y = "Observed Win Rate", title = "Calibration Plot") +
            theme_minimal()
        
        p2 <- ggplot(record_df, aes(x = factor(Record), y = Probability)) +
            geom_col(fill = "steelblue") +
            labs(x = "Wins Accumulated", y = "Probability", title = "Theoretical Record Distribution") +
            theme_minimal()
        
        gridExtra::grid.arrange(p1, p2, nrow = 2)
    })
    
    output$record_table <- renderTable({
        req(formatted_deck())
        user_pred <- predict(rf_model, newdata = formatted_deck())
        states <- expand.grid(W = 0:7, L = 0:3)
        states <- states[!(states$W < 7 & states$L < 3), ]
        states <- states[!(states$W == 7 & states$L == 3), ]
        
        states$P <- mapply(function(w, l) {
            choose(w + l, w) * user_pred^w * (1 - user_pred)^l
        }, states$W, states$L)
        
        record_df <- states
        colnames(record_df) <- c("Wins", "Losses", "Probability")
        record_df$Probability <- record_df$Probability / sum(record_df$Probability)
        record_df$Probability <- round(record_df$Probability, 4)
        record_df <- record_df[order(-record_df$Wins, record_df$Losses), ]
        record_df
    })
}

shinyApp(ui, server)
