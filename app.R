# app.R  ----

# 1. Libraries (shinyapps.io will see these and install them)
library(shiny)
library(dplyr)
library(ggplot2)
library(broom)
library(mice)
library(Metrics)
library(NHANES)
library(naniar)

# 2. Load precomputed data & models -----------------------------

# These .rds files must be in the same folder as app.R
comp1                  <- readRDS("comp1.rds")
fit_red_1              <- readRDS("fit_red_1.rds")
tidy_reduced_table     <- readRDS("tidy_reduced_table.rds")
comparison_table_round <- readRDS("comparison_table_round.rds")

# Initial model and coefficient table used by the app
current_model_initial  <- fit_red_1
coeff_table_initial    <- tidy_reduced_table

# 3. Predictor groups for the UI -------------------------------

demography_choices <- c(
  "Education"                     = "Education",
  "Marital status"                = "MaritalStatus",
  "Household income (midpoint)"   = "HHIncomeMid",
  "Poverty ratio"                 = "Poverty",
  "Housing: own or rent"          = "HomeOwn",
  "Work status"                   = "Work"
)

health_choices <- c(
  "Resting pulse"                         = "Pulse",
  "Average systolic blood pressure"       = "BPSysAve",
  "Average diastolic blood pressure"      = "BPDiaAve",
  "Direct cholesterol"                    = "DirectChol",
  "Total cholesterol"                     = "TotChol",
  "Diabetes (yes / no)"                   = "Diabetes",
  "Days physical health was bad (last 30 days)" = "DaysPhysHlthBad",
  "Physically active (yes / no)"          = "PhysActive"
)

mental_choices <- c(
  "Days mental health was bad (last 30 days)" = "DaysMentHlthBad",
  "Little interest / pleasure score"          = "LittleInterest",
  "Depressed / hopeless score"               = "Depressed"
)

## -------------------------------------------------------------
## UI
## -------------------------------------------------------------

ui <- fluidPage(
  titlePanel("NHANES Age Prediction Model"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("1. Choose predictors\nfor the model"),
      
      # Demography group ---------------------------------------
      h4("Demography"),
      actionButton("demo_toggle", "Toggle all demography"),
      checkboxGroupInput(
        "pred_demo",
        "Include these predictors:",
        choices  = demography_choices,
        selected = demography_choices
      ),
      tags$hr(),
      
      # Health group -------------------------------------------
      h4("Health"),
      actionButton("health_toggle", "Toggle all health"),
      checkboxGroupInput(
        "pred_health",
        NULL,
        choices  = health_choices,
        selected = health_choices
      ),
      tags$hr(),
      
      # Mental health group ------------------------------------
      h4("Mental health"),
      actionButton("mental_toggle", "Toggle all mental health"),
      checkboxGroupInput(
        "pred_mental",
        NULL,
        choices  = mental_choices,
        selected = mental_choices
      ),
      
      tags$hr(),
      actionButton("update_model", "Update model"),
      
      tags$hr(),
      h3("2. Predict age for\na specific person"),
      
      selectInput("person_edu", "Education:",
                  choices = levels(comp1$Education),
                  selected = "Some College"),
      
      selectInput("person_marital", "Marital status:",
                  choices = levels(comp1$MaritalStatus),
                  selected = "NeverMarried"),
      
      sliderInput("person_income", "Household income (midpoint, dollars):",
                  min = 2500, max = 100000, value = 47500, step = 2500),
      
      sliderInput("person_poverty", "Poverty ratio:",
                  min = 0, max = 5, value = 2.5, step = 0.1),
      
      selectInput("person_home", "Housing:",
                  choices = levels(comp1$HomeOwn)),
      
      selectInput("person_work", "Work status:",
                  choices = levels(comp1$Work)),
      
      sliderInput("person_pulse", "Resting pulse (beats per minute):",
                  min = 40, max = 140, value = 72),
      
      sliderInput("person_sys", "Average systolic BP:",
                  min = 70, max = 220, value = 115),
      
      sliderInput("person_dia", "Average diastolic BP:",
                  min = 40, max = 120, value = 69),
      
      sliderInput("person_directchol", "Direct cholesterol (mmol/L):",
                  min = 0.3, max = 13, value = 4.8, step = 0.1),
      
      sliderInput("person_totchol", "Total cholesterol (mmol/L):",
                  min = 1.3, max = 13.5, value = 4.8, step = 0.1),
      
      selectInput("person_diabetes", "Diabetes:",
                  choices = levels(comp1$Diabetes)),
      
      sliderInput("person_days_physbad",
                  "Days physical health was bad (last 30 days):",
                  min = 0, max = 30, value = 2),
      
      sliderInput("person_days_mentbad",
                  "Days mental health was bad (last 30 days):",
                  min = 0, max = 30, value = 2),
      
      sliderInput("person_littleinterest",
                  "Little interest / pleasure score:",
                  min = 0, max = 3, value = 1),
      
      sliderInput("person_depressed",
                  "Depressed / hopeless score:",
                  min = 0, max = 3, value = 1),
      
      selectInput("person_physactive", "Physically active (yes / no):",
                  choices = levels(comp1$PhysActive))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Model results",
          h3("Model fit statistics"),
          tableOutput("model_fit"),
          tags$br(),
          h3("Regression coefficients"),
          tableOutput("coef_table"),
          tags$br(),
          h3("Plot 1: Predicted vs actual age"),
          plotOutput("plot_pred_actual", height = "450px")
        ),
        
        tabPanel(
          "Your age prediction",
          h3("Predicted age based on your inputs"),
          verbatimTextOutput("your_pred_age"),
          tags$br(),
          h4("What seems to drive this prediction?"),
          verbatimTextOutput("your_pred_drivers")
        ),
        
        tabPanel(
          "Random people comparison",
          h3("Compare with random NHANES participants"),
          numericInput("n_random", "How many random people to sample?",
                       value = 10, min = 1, max = 100),
          actionButton("draw_random", "Draw random sample"),
          tableOutput("random_table")
        )
      )
    )
  )
)

## -------------------------------------------------------------
## SERVER
## -------------------------------------------------------------

server <- function(input, output, session) {
  
  # current model (start from your reduced model)
  current_model <- reactiveVal(current_model_initial)
  
  ## ---- group toggle buttons ----------------------------------
  
  observeEvent(input$demo_toggle, {
    cur <- input$pred_demo
    if (length(cur) == length(demography_choices)) {
      updateCheckboxGroupInput(session, "pred_demo",
                               selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "pred_demo",
                               selected = demography_choices)
    }
  })
  
  observeEvent(input$health_toggle, {
    cur <- input$pred_health
    if (length(cur) == length(health_choices)) {
      updateCheckboxGroupInput(session, "pred_health",
                               selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "pred_health",
                               selected = health_choices)
    }
  })
  
  observeEvent(input$mental_toggle, {
    cur <- input$pred_mental
    if (length(cur) == length(mental_choices)) {
      updateCheckboxGroupInput(session, "pred_mental",
                               selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "pred_mental",
                               selected = mental_choices)
    }
  })
  
  ## ---- refit model when Update is clicked -------------------
  
  observeEvent(input$update_model, {
    selected <- c(input$pred_demo, input$pred_health, input$pred_mental)
    
    if (length(selected) == 0) {
      showNotification("Please select at least one predictor.",
                       type = "error")
      return()
    }
    
    formula_str <- paste("Age ~", paste(selected, collapse = " + "))
    new_model <- lm(as.formula(formula_str), data = comp1)
    current_model(new_model)
  })
  
  ## ---- Model fit statistics (R2 + RMSE only) ----------------
  
  output$model_fit <- renderTable({
    mod <- current_model()
    preds <- predict(mod, newdata = comp1)
    
    rmse_val <- sqrt(mean((comp1$Age - preds)^2))
    r2_val   <- summary(mod)$r.squared
    
    data.frame(
      R2   = round(r2_val, 3),
      RMSE = round(rmse_val, 2)
    )
  }, digits = 3)
  
  ## ---- Coefficient table ------------------------------------
  
  output$coef_table <- renderTable({
    mod <- current_model()
    broom::tidy(mod) %>%
      mutate(
        estimate  = round(estimate, 2),
        std.error = round(std.error, 2),
        statistic = round(statistic, 2),
        p.value   = signif(p.value, 3)
      ) %>%
      rename(term = term)
  })
  
  ## ---- Predicted vs actual plot -----------------------------
  
  output$plot_pred_actual <- renderPlot({
    mod <- current_model()
    df  <- comp1
    df$PredictedAge <- predict(mod, newdata = df)
    
    ggplot(df, aes(x = Age, y = PredictedAge)) +
      geom_point(alpha = 0.20, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE,
                  color = "darkblue", linewidth = 1) +
      geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", linewidth = 1, color = "red"
      ) +
      labs(
        title = "Predicted vs Actual Age",
        x = "Actual age",
        y = "Predicted age"
      ) +
      theme_minimal(base_size = 14)
  })
  
  ## ---- Your age prediction tab ------------------------------
  
  your_person <- reactive({
    data.frame(
      Education        = factor(input$person_edu,
                                levels = levels(comp1$Education)),
      MaritalStatus    = factor(input$person_marital,
                                levels = levels(comp1$MaritalStatus)),
      HHIncomeMid      = input$person_income,
      Poverty          = input$person_poverty,
      HomeOwn          = factor(input$person_home,
                                levels = levels(comp1$HomeOwn)),
      Work             = factor(input$person_work,
                                levels = levels(comp1$Work)),
      Pulse            = input$person_pulse,
      BPSysAve         = input$person_sys,
      BPDiaAve         = input$person_dia,
      DirectChol       = input$person_directchol,
      TotChol          = input$person_totchol,
      Diabetes         = factor(input$person_diabetes,
                                levels = levels(comp1$Diabetes)),
      DaysPhysHlthBad  = input$person_days_physbad,
      DaysMentHlthBad  = input$person_days_mentbad,
      LittleInterest   = input$person_littleinterest,
      Depressed        = input$person_depressed,
      PhysActive       = factor(input$person_physactive,
                                levels = levels(comp1$PhysActive))
    )
  })
  
  output$your_pred_age <- renderText({
    mod <- current_model()
    newdat <- your_person()
    
    pred <- tryCatch(
      as.numeric(predict(mod, newdata = newdat)),
      error = function(e) NA_real_
    )
    
    if (is.na(pred)) {
      return("Prediction failed (model/newdata mismatch). Check the R console for details.")
    }
    
    paste0("Your predicted age is approximately ",
           round(pred, 1), " years.")
  })
  
  output$your_pred_drivers <- renderText({
    mod    <- current_model()
    newdat <- your_person()
    
    # Build model matrix for this person's inputs using the current model terms
    X <- tryCatch(
      model.matrix(delete.response(terms(mod)), newdat),
      error = function(e) NULL
    )
    
    if (is.null(X)) {
      return("Could not compute drivers for this model/newdata (see console).")
    }
    
    b <- coef(mod)
    
    # Align columns & coefficients (important if lm drops aliased terms)
    common <- intersect(colnames(X), names(b))
    if (length(common) == 0) {
      return("No overlapping terms found to explain prediction.")
    }
    
    contrib <- as.numeric(X[1, common] * b[common])
    names(contrib) <- common
    
    # Drop intercept for 'drivers'
    contrib <- contrib[names(contrib) != "(Intercept)"]
    
    pos <- sort(contrib[contrib > 0], decreasing = TRUE)
    neg <- sort(contrib[contrib < 0], decreasing = FALSE)
    
    pos_show <- head(pos, 5)
    neg_show <- head(neg, 5)
    
    fmt <- function(x) paste(sprintf("  %s: %+0.2f", names(x), x), collapse = "\n")
    
    paste0(
      "Top positive contributions (push predicted age UP):\n",
      if (length(pos_show)) fmt(pos_show) else "  none",
      "\n\nTop negative contributions (push predicted age DOWN):\n",
      if (length(neg_show)) fmt(neg_show) else "  none"
    )
  })
  
  ## ---- Random people comparison tab -------------------------
  
  observeEvent(input$draw_random, {
    n <- input$n_random
    n <- max(1, min(n, nrow(comp1)))
    
    idx <- sample(seq_len(nrow(comp1)), size = n)
    df <- comp1[idx, ]
    
    mod <- current_model()
    df$PredictedAge <- predict(mod, newdata = df)
    df$AgeDiff <- df$Age - df$PredictedAge
    
    output$random_table <- renderTable({
      df %>%
        select(
          Age, PredictedAge, AgeDiff,
          Education, MaritalStatus, HHIncomeMid,
          Pulse, BPSysAve, BPDiaAve,
          DirectChol, TotChol,
          Diabetes, PhysActive
        ) %>%
        mutate(
          PredictedAge = round(PredictedAge, 1),
          AgeDiff      = round(AgeDiff, 1)
        )
    })
  })
}

## -------------------------------------------------------------
## Run the app
## -------------------------------------------------------------
shinyApp(ui = ui, server = server)
