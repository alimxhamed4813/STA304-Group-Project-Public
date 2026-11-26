# ==============================================================================
# Part-Time Work, Study Time, and Academic Performance (Fall 2026 STA304 survey)
#
# Single-file Shiny app. Needs Responses.csv in the SAME folder as this file.
# Packages: shiny, bslib, ggplot2, dplyr, tidyr, DT
# Type in console: install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT")) 
# in order to install them if you don't have them already.
#
#     Overview ................... Done
#     Distributions .............. Done
#     Relationships & Regression . Done
#     GPA ........................ Done
#     Data ....................... Done
#     About ...................... Done
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# ==============================================================================
# 1. Data (Reading the info from the CSV and cleaning it up)
# ==============================================================================
raw <- read.csv("Responses.csv", na.strings = c("", "NA", "N/A"))

gpa_levels <- c("Below 2.3", "2.69 - 2.3", "2.99 - 2.7",
                "3.29 - 3.0", "3.69 - 3.3", "4.0 - 3.7")
gpa_labels <- c("Below 2.3", "2.3-2.69", "2.7-2.99",
                "3.0-3.29", "3.3-3.69", "3.7-4.0")

df <- raw |>
  transmute(
    `Course load`            = course_load,
    Employed                 = factor(employed, levels = c(0, 1),
                                      labels = c("No", "Yes")),
    `Weekly work hours`      = work_hours,
    `Commute time (minutes)` = commute_time,
    `Work energy rating`     = work_energy,
    sector_clean = case_when(
      work_sector %in% c("food services", "waitress") ~ "Retail & Services",
      work_sector %in% c("Education", "research")     ~ "Education & Research",
      TRUE                                            ~ work_sector),
    `Weekly study hours`     = study_hrs,
    `Missed a deadline`      = factor(missed_deadline, levels = c(0, 1),
                                      labels = c("No", "Yes")),
    `GPA band`               = factor(sessional_GPA, levels = gpa_levels,
                                      labels = gpa_labels, ordered = TRUE),
    `GPA band score`         = as.integer(factor(sessional_GPA,
                                                 levels = gpa_levels))
  ) |>
  mutate(
    `Work sector` = case_when(
      Employed == "No"      ~ "Not employed",
      is.na(sector_clean)   ~ "Unspecified",
      TRUE                  ~ sector_clean)
  ) |>
  select(-sector_clean)

quant_items <- c("Course load", "Weekly study hours", "Weekly work hours",
                 "Commute time (minutes)", "Work energy rating")
work_items  <- c("Weekly work hours", "Commute time (minutes)",
                 "Work energy rating")
all_num     <- c(quant_items, "GPA band score")
all_num_choices <- setNames(
  all_num, c(quant_items, "GPA band score (1 = lowest band, 6 = highest)"))

group_choices <- c("None", "Employed", "Missed a deadline", "Work sector")
sectors <- sort(setdiff(unique(df$`Work sector`), "Not employed"))

load_range <- c(floor(min(df$`Course load`, na.rm = TRUE)),
                ceiling(max(df$`Course load`, na.rm = TRUE)))

var_range <- function(v) {
  c(floor(min(df[[v]], na.rm = TRUE)), ceiling(max(df[[v]], na.rm = TRUE)))
}

# ------------------------------------------------------------------------------
# 2. Helpers (shared by all tabs)
# ------------------------------------------------------------------------------
palettes <- list(
  "Default blue / red"        = c("#6699FF", "#FF6666", "#66CC99",
                                  "#FFB266", "#B266FF", "#999999"),
  "Colour-blind friendly"     = c("#0072B2", "#E69F00", "#009E73",
                                  "#CC79A7", "#56B4E9", "#D55E00"),
  "Pastel"                    = c("#A6CEE3", "#B2DF8A", "#FB9A99",
                                  "#FDBF6F", "#CAB2D6", "#E5D8BD"),
  "Greyscale"                 = c("#333333", "#808080", "#B3B3B3",
                                  "#4D4D4D", "#999999", "#CCCCCC")
)

# Colours for the chosen palette name, e.g. get_pal(input$palette)
get_pal <- function(name) palettes[[name]]

# Named colour vector for a set of group levels (use in scale_fill_manual)
fill_values <- function(levels_, pal_name) {
  p <- rep_len(get_pal(pal_name), length(levels_))
  names(p) <- levels_
  p
}

# Shared ggplot theme: + app_theme(input$base_size)
app_theme <- function(size) {
  theme_minimal(base_size = size) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"))
}


add_group <- function(d, g) {
  if (g == "None") {
    d$.grp <- rep("All selected students", nrow(d))
  } else {
    x <- as.character(d[[g]])
    x[is.na(x)] <- "Unknown"
    d$.grp <- x
  }
  d
}

# Formats a p-value for plain-language text
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
}

# Describes the size of a correlation in words
strength_word <- function(r) {
  a <- abs(r)
  if (a < 0.1) "negligible" else if (a < 0.3) "weak"
  else if (a < 0.5) "moderate" else "strong"
}

# ------------------------------------------------------------------------------
# 3. UI (Shared by all the tabs)
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "STA304 Work & Academic Performance Correlation",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#4C7DFF",
                   base_font = font_google("Inter"),
                   heading_font = font_google("Inter")),
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 310,
    accordion(
      open = c("Filter students", "Plot appearance"),
      accordion_panel(
        "Filter students",
        checkboxGroupInput(
          "f_emp", "Employment status",
          choices = c("Employed" = "Yes", "Not employed" = "No"),
          selected = c("Yes", "No"), inline = TRUE),
        sliderInput("f_load", "Number of courses this term",
                    min = load_range[1], max = load_range[2],
                    value = load_range, step = 1),
        checkboxGroupInput("f_sector",
                           "Work sector (only affects employed students)",
                           choices = sectors, selected = sectors),
        selectizeInput("f_gpa", "GPA band", choices = gpa_labels,
                       selected = gpa_labels, multiple = TRUE),
        actionButton("reset", "Reset filters", class = "btn-outline-primary btn-sm"),
        hr(),
        strong(textOutput("n_selected", inline = TRUE))
      ),
      accordion_panel(
        "Plot appearance",
        selectInput("palette", "Colour palette", names(palettes)),
        sliderInput("pt_size", "Point size", min = 1, max = 6,
                    value = 2.5, step = 0.5),
        sliderInput("base_size", "Text size", min = 10, max = 20,
                    value = 14, step = 1)
      )
    )
  ),
  
  # ---- Overview (Done)----
  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Students selected", textOutput("vb_n"),
                theme = value_box_theme(bg = "#4C7DFF", fg = "#FFFFFF")),
      value_box("Currently employed", textOutput("vb_emp"),
                theme = value_box_theme(bg = "#1FA37A", fg = "#FFFFFF")),
      value_box("Avg. weekly study hours", textOutput("vb_study"),
                theme = value_box_theme(bg = "#E8702A", fg = "#FFFFFF")),
      value_box("Employed students who missed a deadline due to work",
                textOutput("vb_miss"),
                theme = value_box_theme(bg = "#9B51E0", fg = "#FFFFFF"))
    ),
    card(
      card_header("What is this app about?"),
      p("Many university students work part-time while studying. This app 
        explores the relationship between part-time employment and academic 
        performance for Fall 2026 STA304 students."),
      tags$ul(
        tags$li(strong("Left panel: "), "Filter to a group of students or",
                "change the colours. "),
        tags$li(strong("Distributions: "), "See how a single measure is spread out."),
        tags$li(strong("Relationships & Regression: "), "See whether measures",
                "move together, and test whether work hours and work energy",
                "are linked to GPA."),
        tags$li(strong("GPA: "), "Compare GPA bands between groups."),
        tags$li(strong("Data: "), "Browse and search the survey responses."),
        tags$li(strong("About: "), "How the survey was run and its limits.")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Consensus"),
        p("Using all employed respondents (22 students), we found no",
          "statistically significant link between work hours or work energy",
          "and weekly study time, and no statistically significant link",
          "between either of them and GPA band. Both showed a slight",
          "downward trend with GPA in our sample, but the sample is small, so",
          "we cannot say this pattern exists in the wider student population.")
      ),
      card(
        card_header("Next Steps"),
        p("Our results come from 71 students in a single class, and only 22 of",
          "them have a job. That is too small and too specific a group to say",
          "anything reliable about students in general. If we continued this",
          "project, we would:"),
        tags$ul(
          tags$li(strong("Survey the whole school. "),
                  "A much larger sample from many programs would give more",
                  "precise estimates and a better chance of detecting a real",
                  "relationship if one exists."),
          tags$li(strong("Compare majors. "),
                  "With enough responses we could filter by major and see",
                  "which programs show the strongest link between work and",
                  "academic performance."),
          tags$li(strong("Collect more detailed information. "),
                  "Exact GPA instead of bands, year of study, job type, and",
                  "how demanding each student's courses are.")
        )
      )
    )
  ),
  
  # ---- Distributions (DONE) ----
  nav_panel(
    "Distributions",
    card(
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("dist_var", "Variable to plot", quant_items,
                      selected = "Weekly study hours"),
          selectInput("dist_type", "Plot type",
                      c("Histogram", "Density", "Boxplot")),
          selectInput("dist_group", "Compare groups by", group_choices),
          conditionalPanel("input.dist_type == 'Histogram'",
                           sliderInput("dist_bins", "Number of bins",
                                       min = 3, max = 30, value = 12, step = 1)),
          sliderInput("dist_lim", "Value-axis range",
                      min = var_range("Weekly study hours")[1],
                      max = var_range("Weekly study hours")[2],
                      value = var_range("Weekly study hours"), step = 1),
          checkboxInput("dist_log", "Log scale", FALSE),
          checkboxInput("dist_emp_only",
                        "Employed students only (recommended for work variables)",
                        FALSE)
        )
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE, plotOutput("dist_plot", height = "450px")),
      card(card_header("How to read this"), uiOutput("dist_text"))
    ),
    card(card_header("Summary statistics for the plotted students"),
         tableOutput("dist_tbl"))
  ),
  
  # ---- Relationships & Regression (DONE) ----
  nav_panel(
    "Relationships & Regression",
    card(
      card_header("Relationships"),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("sc_x", "X-axis variable", all_num_choices,
                      selected = "Weekly work hours"),
          selectInput("sc_y", "Y-axis variable", all_num_choices,
                      selected = "Weekly study hours"),
          selectInput("sc_group", "Colour points by", group_choices),
          div(
            checkboxInput("sc_trend", "Show trend line", TRUE),
            checkboxInput("sc_jitter", "Jitter points (helps with ties)", TRUE),
            checkboxInput("sc_emp_only",
                          "Employed students only (recommended for work variables)",
                          TRUE)
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE, plotOutput("sc_plot", height = "450px")),
      card(card_header("Correlation test (Pearson)"), uiOutput("sc_text"))
    ),
    card(
      card_header("Regression"),
      p("Multiple linear regression of GPA band score on weekly work hours",
        "and work energy rating (employed students only, as in the report)."),
      card_body(
        layout_columns(
          col_widths = c(4, 8),
          selectInput("reg_plot_type", "Plot",
                      c("Coefficient estimates (95% CI)" = "coef",
                        "Observed vs fitted" = "fit",
                        "Residuals vs fitted" = "resid")),
          p(class = "text-muted",
            "Uses the students selected in the left panel who are employed",
            "and have an answer for all three variables.")
        )
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE, plotOutput("reg_plot", height = "380px")),
      card(card_header("How to read this"), uiOutput("reg_text"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Coefficients"), tableOutput("reg_tbl")),
      card(card_header("Overall model fit"), tableOutput("reg_fit_tbl"))
    )
  ),
  
  # ---- GPA (DONE) ----
  nav_panel(
    "GPA",
    card(
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("gpa_group", "Compare groups by", group_choices, selected = "Employed"),
          selectInput("gpa_pos", "Bar chart style",
                      c("Dodge (Side-by-side)" = "dodge",
                        "Stacked" = "stack",
                        "Fill (Proportions)" = "filled")),
          checkboxInput("gpa_drop_na", "Exclude students without reported GPA", TRUE)
        )
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE, plotOutput("gpa_plot", height = "450px")),
      card(card_header("How to read this"), uiOutput("gpa_text"))
    ),
    card(card_header("GPA band frequency table"),
         tableOutput("gpa_tbl"))
  ),
  
   # ---- Data (DONE) ----
  nav_panel(
    "Data",
    card(
      card_header("Survey Responses"),
      p("These are the survey responses used in our analysis. Use the ",
        "filters on the left or search the table to explore the data."),
      p(class = "text-muted",
        "Student IDs and timestamps are not shown. Some questions were ",
        "only asked of employed students, so blank cells are expected ",
        "for students without jobs."),
      strong(textOutput("data_count", inline = TRUE)),
      DTOutput("data_tbl")
    )
  ),
  
  # ---- About (DONE) ----
  nav_panel(
    "About",
    card(
      card_header("About Our Survey"),
      p(
        "Our project examines the relationship between part-time ",
        "employment, study time, and academic performance among Fall 2026 ",
        "STA304 students. We want to see if students with jobs have ",
        "different study habits and GPA ranges compared to those without ",
        "jobs. We also looked at how work hours and work energy related to ",
        "study time and academic performance among employed students. We ",
        "collected 71 responses, with 22 employed students (31%) and 49 ",
        "unemployed students (69%)."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Collection and Sampling"),
        p(
          "We collected data using a Google Forms survey shared on the STA304 ",
          "Piazza page. Students could choose to complete the survey using ",
          "the link posted on Piazza, so this was a voluntary-response ",
          "sample."
        )
      ),
      card(
        card_header("What We Asked"),
        tags$ul(
          tags$li(strong("Study and GPA: Number of courses taken this semester, weekly study hours outside of class, and last sessional GPA in six ranges.")),
          tags$li(strong("Employment: Current employment status, weekly work hours, and work sector.")),
          tags$li(strong("Work experience: One-way commute time, physical and mental effort during a typical work shift (rated from 1 to 10), and missed coursework deadlines due to work."))
        )
      )
    ),
    card(
      card_header("How We Analyzed the Data"),
      p(
        "We started with descriptive statistics and graphs to summarize the ",
        "survey responses, then compared study hours and GPA ranges between ",
        "employed and unemployed students."
      ),
      p(
        "For the 22 employed students, we calculated Pearson correlations ",
        "to explore how weekly work hours and work energy rating related to ",
        "study hours. We also used multiple linear regression to examine ",
        "how these two variables together related to GPA band score."
      ),
      p(
        "Since the survey collected GPA in six ranges, we assigned each ",
        "range a score from 1 (lowest) to 6 (highest) for the regression. ",
        "These scores represent GPA categories, not exact GPA values."
      )
    ),
    card(
      card_header("Limitations"),
      p(
        "Although our analysis shows some patterns between employment and ",
        "academic performance, there are a few factors to consider when ",
        "interpreting the results. Only 22 students reported having jobs, ",
        "so the work-related analyses are based on a relatively small ",
        "sample."
      ),
      p(
        "Since participation was voluntary and the sample focused on STA304 ",
        "students, the findings may not represent the wider university ",
        "student population. Also, because this was an observational study, ",
        "we can examine relationships between employment and academic ",
        "performance, but cannot conclude that having a job directly ",
        "affects students' GPA or study time."
      )
    ),
    card(
      card_header("Team Members"),
      p(
        "Khadija Benza, Abdirashid Einte, Giulian Giannone, Jiayi Guo, Ali ",
        "Mohamed, and Munieba Zia"
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 4. Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- Filters (shared: every tab should start from filtered()) ----
  filtered <- reactive({
    df |>
      filter(
        Employed %in% input$f_emp,
        is.na(`Course load`) |
          (`Course load` >= input$f_load[1] & `Course load` <= input$f_load[2]),
        `Work sector` %in% c(input$f_sector, "Not employed"),
        is.na(`GPA band`) | `GPA band` %in% input$f_gpa
      )
  })
  
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "f_emp", selected = c("Yes", "No"))
    updateSliderInput(session, "f_load", value = load_range)
    updateCheckboxGroupInput(session, "f_sector", selected = sectors)
    updateSelectizeInput(session, "f_gpa", selected = gpa_labels)
  })
  
  output$n_selected <- renderText({
    sprintf("%d of %d students selected", nrow(filtered()), nrow(df))
  })
  
  # ---- Overview boxes (DONE) ----
  output$vb_n <- renderText(as.character(nrow(filtered())))
  output$vb_emp <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("-")
    sprintf("%.0f%%", 100 * mean(d$Employed == "Yes"))
  })
  output$vb_study <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("-")
    sprintf("%.1f", mean(d$`Weekly study hours`, na.rm = TRUE))
  })
  output$vb_miss <- renderText({
    d <- filtered() |> filter(Employed == "Yes", !is.na(`Missed a deadline`))
    if (nrow(d) == 0) return("-")
    sprintf("%.0f%%", 100 * mean(d$`Missed a deadline` == "Yes"))
  })
  
# ---- Distributions (DONE) ----
  observeEvent(input$dist_var, {
    r <- var_range(input$dist_var)
    updateSliderInput(session, "dist_lim", min = r[1], max = r[2], value = r)
  })
  
  dist_data <- reactive({
    v <- input$dist_var
    d <- filtered()
    if (isTRUE(input$dist_emp_only)) d <- d |> filter(Employed == "Yes")
    d <- d |> filter(!is.na(.data[[v]]))
    add_group(d, input$dist_group)
  })
  
  output$dist_plot <- renderPlot({
    d <- dist_data()
    v <- input$dist_var
    validate(need(nrow(d) > 1, "Not enough students match the current filters."))
    
    lv <- sort(unique(d$.grp))
    d$.grp <- factor(d$.grp, levels = lv)
    cols <- fill_values(lv, input$palette)
    type <- input$dist_type
    horizontal <- type %in% c("Histogram", "Density")
    tr <- if (isTRUE(input$dist_log)) "pseudo_log" else "identity"
    
    p <- ggplot(d)
    if (type == "Histogram") {
      p <- p + geom_histogram(aes(x = .data[[v]], fill = .grp),
                              bins = input$dist_bins, colour = "white",
                              alpha = 0.75, position = "identity")
    } else if (type == "Density") {
      p <- p + geom_density(aes(x = .data[[v]], fill = .grp, colour = .grp),
                            alpha = 0.4)
    } else if (type == "Boxplot") {
      p <- p +
        geom_boxplot(aes(x = .grp, y = .data[[v]], fill = .grp),
                     alpha = 0.7, outlier.shape = NA) +
        geom_jitter(aes(x = .grp, y = .data[[v]]), width = 0.15,
                    alpha = 0.5, size = input$pt_size)
    } else {
      p <- p +
        geom_violin(aes(x = .grp, y = .data[[v]], fill = .grp), alpha = 0.7) +
        geom_boxplot(aes(x = .grp, y = .data[[v]]), width = 0.12,
                     fill = "white", outlier.shape = NA)
    }
    
    legend_title <- input$dist_group
    
    p <- p +
      (if (horizontal) scale_x_continuous(limits = input$dist_lim, transform = tr)
       else scale_y_continuous(limits = input$dist_lim, transform = tr)) +
      scale_fill_manual(values = cols) +
      scale_colour_manual(values = cols) +
      labs(title = paste(v, "among the selected students"),
           subtitle = if (isTRUE(input$dist_log)) "Log scale" else NULL,
           x = if (horizontal) v else if (input$dist_group == "None") NULL
           else input$dist_group,
           y = if (horizontal) "Number of students" else v,
           fill = legend_title,
           colour = legend_title) +
      app_theme(input$base_size)
    
    if (type == "Density") p <- p + labs(y = "Density (relative frequency)")
    if (length(lv) == 1 || !horizontal) {
      p <- p + theme(legend.position = "none")
    }
    suppressWarnings(print(p))
  })
  
  output$dist_tbl <- renderTable({
    d <- dist_data()
    v <- input$dist_var
    req(nrow(d) > 0)
    d |>
      group_by(Group = .grp) |>
      summarise(Students = n(),
                Mean = mean(.data[[v]]),
                Median = median(.data[[v]]),
                `Std. deviation` = sd(.data[[v]]),
                Minimum = min(.data[[v]]),
                Maximum = max(.data[[v]]),
                .groups = "drop")
  }, digits = 2)
  
  output$dist_text <- renderUI({
    d <- dist_data()
    v <- input$dist_var
    validate(need(nrow(d) > 1, "Not enough students match the current filters."))
    x <- d[[v]]
    m <- mean(x); md <- median(x); s <- sd(x)
    shape <- if (s == 0) "all the same value" else {
      k <- (m - md) / s
      if (k > 0.1) paste("skewed to the right: a few large values pull the",
                         "average above the typical student")
      else if (k < -0.1) paste("skewed to the left: a few small values pull the",
                               "average below the typical student")
      else "fairly balanced around the middle"
    }
    note <- if (v %in% work_items && !isTRUE(input$dist_emp_only) &&
                any(d$Employed == "No")) {
      p(class = "text-muted",
        "Students without a job are recorded as 0 for work questions, which",
        "drags these numbers down. Tick \"Employed students only\" for a",
        "clearer picture.")
    }
    tagList(
      p(sprintf("Across the %d students shown, the typical (median) value of",
                length(x)),
        tolower(v), sprintf("is %.1f, and the average (mean) is %.1f.", md, m)),
      p(paste0("The distribution is ", shape, ".")),
      p(sprintf("The standard deviation is %.1f: most students are within about",
                s), "that distance of the average."),
      note
    )
  })

  # ---- Relationships & Regression (DONE) ----
  
  # Scatterplot data: two chosen variables, complete cases only
  sc_data <- reactive({
    d <- filtered()
    if (isTRUE(input$sc_emp_only)) d <- d |> filter(Employed == "Yes")
    d <- d |> filter(!is.na(.data[[input$sc_x]]), !is.na(.data[[input$sc_y]]))
    add_group(d, input$sc_group)
  })
  
  # Pearson correlation test on the plotted students (NULL if not computable)
  sc_test <- reactive({
    d <- sc_data()
    x <- d[[input$sc_x]]; y <- d[[input$sc_y]]
    if (length(x) < 4 || sd(x) == 0 || sd(y) == 0) return(NULL)
    cor.test(x, y, method = "pearson", conf.level = 0.95)
  })
  
  sc_checks <- function() {
    validate(
      need(input$sc_x != input$sc_y, "Choose two different variables."),
      need(nrow(sc_data()) >= 4,
           "Need at least 4 students with answers for both variables."),
      need(!is.null(sc_test()),
           "One of the variables has no variation among the selected students.")
    )
  }
  
  output$sc_plot <- renderPlot({
    sc_checks()
    d <- sc_data()
    x <- input$sc_x; y <- input$sc_y
    lv <- sort(unique(d$.grp))
    d$.grp <- factor(d$.grp, levels = lv)
    cols <- fill_values(lv, input$palette)
    jit <- if (isTRUE(input$sc_jitter)) {
      position_jitter(width = 0.15, height = 0.15, seed = 1)
    } else "identity"
    
    p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(aes(colour = .grp), size = input$pt_size, alpha = 0.8,
                 position = jit)
    if (isTRUE(input$sc_trend)) {
      # One overall line, matching the single correlation test
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                           colour = "grey25", fill = "grey60",
                           linewidth = 1, alpha = 0.25)
    }
    p <- p +
      scale_colour_manual(values = cols) +
      labs(title = paste(y, "vs.", x),
           subtitle = sprintf("r = %.2f, n = %d", sc_test()$estimate, nrow(d)),
           x = x, y = y, colour = input$sc_group) +
      app_theme(input$base_size)
    if (length(lv) == 1) p <- p + theme(legend.position = "none")
    suppressWarnings(print(p))
  })
  
  output$sc_text <- renderUI({
    sc_checks()
    d <- sc_data()
    ct <- sc_test()
    r <- unname(ct$estimate)
    ci <- ct$conf.int
    direction <- if (r < 0) "negative" else "positive"
    sig <- ct$p.value < 0.05
    note <- if ((input$sc_x %in% work_items || input$sc_y %in% work_items) &&
                !isTRUE(input$sc_emp_only) && any(d$Employed == "No")) {
      p(class = "text-muted",
        "Students without a job are recorded as 0 for work questions, which",
        "distorts the correlation. Tick \"Employed students only\" for a",
        "clearer picture.")
    }
    tagList(
      tags$table(
        class = "table table-sm",
        tags$tr(tags$td("Correlation (r)"), tags$td(strong(sprintf("%.3f", r)))),
        tags$tr(tags$td("95% confidence interval"),
                tags$td(sprintf("[%.3f, %.3f]", ci[1], ci[2]))),
        tags$tr(tags$td("p-value"), tags$td(fmt_p(ct$p.value))),
        tags$tr(tags$td("Students (n)"), tags$td(nrow(d)))
      ),
      p(sprintf("The relationship is %s and %s (r = %.2f).",
                strength_word(r), direction, r),
        if (sig) {
          "It is statistically significant at the 5% level."
        } else {
          paste("It is not statistically significant at the 5% level, so we",
                "cannot rule out that the true correlation is zero.")
        }),
      p(class = "text-muted",
        "Correlation does not show that one variable causes the other.",
        "GPA band score is a 1-6 code for GPA ranges, not an exact GPA."),
      note
    )
  })
  
  # Regression: GPA band score ~ work hours + work energy (employed only)
  reg_data <- reactive({
    filtered() |>
      filter(Employed == "Yes") |>
      transmute(gpa    = `GPA band score`,
                hours  = `Weekly work hours`,
                energy = `Work energy rating`) |>
      tidyr::drop_na()
  })
  
  reg_model <- reactive({
    d <- reg_data()
    validate(
      need(nrow(d) >= 5,
           "Need at least 5 employed students with complete answers."),
      need(sd(d$hours) > 0 && sd(d$energy) > 0 && sd(d$gpa) > 0,
           "One of the variables has no variation among the selected students.")
    )
    lm(gpa ~ hours + energy, data = d)
  })
  
  reg_terms <- c("(Intercept)" = "Intercept",
                 hours  = "Weekly work hours",
                 energy = "Work energy rating")
  
  output$reg_plot <- renderPlot({
    fit <- reg_model()
    d <- reg_data()
    col1 <- get_pal(input$palette)[1]
    type <- input$reg_plot_type
    
    if (type == "coef") {
      est <- summary(fit)$coefficients
      ci <- confint(fit)
      cd <- data.frame(term = reg_terms[rownames(est)],
                       est = est[, "Estimate"],
                       lo = ci[, 1], hi = ci[, 2])
      cd <- cd[cd$term != "Intercept", ]
      p <- ggplot(cd, aes(x = est, y = term)) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
        geom_pointrange(aes(xmin = lo, xmax = hi), colour = col1,
                        size = input$pt_size / 3, linewidth = 1) +
        labs(title = "Effect on GPA band score (95% confidence intervals)",
             subtitle = "Change in band score per one-unit increase, other variable held fixed",
             x = "Estimated coefficient", y = NULL)
    } else if (type == "fit") {
      pd <- data.frame(fitted = fitted(fit), observed = d$gpa)
      p <- ggplot(pd, aes(x = fitted, y = observed)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    colour = "grey40") +
        geom_jitter(width = 0, height = 0.1, size = input$pt_size,
                    alpha = 0.8, colour = col1) +
        scale_y_continuous(breaks = 1:6) +
        labs(title = "Observed vs fitted GPA band score",
             subtitle = "Points on the dashed line would be predicted perfectly",
             x = "Fitted GPA band score", y = "Observed GPA band score")
    } else {
      pd <- data.frame(fitted = fitted(fit), resid = residuals(fit))
      p <- ggplot(pd, aes(x = fitted, y = resid)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
        geom_point(size = input$pt_size, alpha = 0.8, colour = col1) +
        labs(title = "Residuals vs fitted values",
             subtitle = "Look for curvature or a funnel shape, which would suggest a poor fit",
             x = "Fitted GPA band score", y = "Residual")
    }
    suppressWarnings(print(p + app_theme(input$base_size)))
  })
  
  output$reg_tbl <- renderTable({
    fit <- reg_model()
    est <- summary(fit)$coefficients
    ci <- confint(fit)
    data.frame(
      Term = unname(reg_terms[rownames(est)]),
      Estimate = est[, "Estimate"],
      `Std. error` = est[, "Std. Error"],
      `95% CI lower` = ci[, 1],
      `95% CI upper` = ci[, 2],
      `t value` = est[, "t value"],
      `p-value` = vapply(est[, "Pr(>|t|)"], fmt_p, character(1)),
      check.names = FALSE
    )
  }, digits = 3)
  
  output$reg_fit_tbl <- renderTable({
    fit <- reg_model()
    s <- summary(fit)
    f <- s$fstatistic
    p_f <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    data.frame(
      Statistic = c("Students (n)", "R-squared", "Adjusted R-squared",
                    "F-statistic", "F-test p-value", "Residual std. error"),
      Value = c(sprintf("%d", nobs(fit)),
                sprintf("%.3f", s$r.squared),
                sprintf("%.3f", s$adj.r.squared),
                sprintf("%.2f (df = %d, %d)", f[1], f[2], f[3]),
                fmt_p(p_f),
                sprintf("%.3f", s$sigma))
    )
  })
  
  output$reg_text <- renderUI({
    fit <- reg_model()
    s <- summary(fit)
    est <- s$coefficients
    f <- s$fstatistic
    p_f <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    
    describe <- function(term, unit) {
      b <- est[term, "Estimate"]; pv <- est[term, "Pr(>|t|)"]
      p(sprintf("%s: each additional %s is linked to a %s of %.3f GPA band points (p %s%s), holding the other variable fixed. This is %s.",
                reg_terms[[term]], unit,
                if (b < 0) "drop" else "rise", abs(b),
                if (pv < 0.001) "" else "= ", fmt_p(pv),
                if (pv < 0.05) "statistically significant at the 5% level"
                else "not statistically significant at the 5% level"))
    }
    
    tagList(
      p(sprintf("Based on %d employed students, work hours and work energy together explain %.0f%% of the variation in GPA band score (adjusted R-squared: %.0f%%).",
                nobs(fit), 100 * s$r.squared, 100 * s$adj.r.squared)),
      p(sprintf("The model as a whole is %s (F-test p %s%s).",
                if (p_f < 0.05) "statistically significant"
                else "not statistically significant",
                if (p_f < 0.001) "" else "= ", fmt_p(p_f))),
      describe("hours", "weekly work hour"),
      describe("energy", "point of work energy rating"),
      p(class = "text-muted",
        "Small sample and observational data: these are associations, not",
        "proof that work changes GPA. The outcome is a 1-6 band code, not an",
        "exact GPA.")
    )
  })
  
# ---- GPA (DONE) ----
  gpa_data <- reactive({
    d <- filtered()
    if (isTRUE(input$gpa_drop_na)) d <- d |> filter(!is.na(`GPA band`))
    add_group(d, input$gpa_group)
  })
  
  output$gpa_plot <- renderPlot({
    d <- gpa_data()
    validate(need(nrow(d) > 1, "Not enough students match the current filters."))
    
    lv <- sort(unique(d$.grp))
    d$.grp <- factor(d$.grp, levels = lv)
    cols <- fill_values(lv, input$palette)
    pos <- switch(input$gpa_pos,
                  "dodge" = "dodge",
                  "stack" = "stack",
                  "filled" = "fill")
    
    p <- ggplot(d, aes(x = `GPA band`, fill = .grp)) +
      geom_bar(position = pos, colour = "white", alpha = 0.85) +
      scale_fill_manual(values = cols) +
      labs(title = "Sessional GPA bands by group",
           subtitle = if (pos == "fill") "Proportions (100% stacked)" else "Number of students",
           x = "GPA band",
           y = if (pos == "fill") "Proportion" else "Number of students",
           fill = input$gpa_group) +
      app_theme(input$base_size) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
    if (length(lv) == 1) {
      p <- p + theme(legend.position = "none")
    }
    suppressWarnings(print(p))
  })
  
  output$gpa_tbl <- renderTable({
    d <- gpa_data()
    req(nrow(d) > 0)
    d |>
      count(Group = .grp, `GPA band`) |>
      tidyr::pivot_wider(names_from = `GPA band`, values_from = n, values_fill = 0)
  }, digits = 0)
  
  output$gpa_text <- renderUI({
    d <- gpa_data()
    validate(need(nrow(d) > 1, "Not enough students match the current filters."))
    
    total_n <- nrow(d)
    tagList(
      p(sprintf("This chart compares the distribution of sessional GPA bands across %d selected students grouped by %s.",
                total_n, tolower(input$gpa_group))),
      p("GPA was collected in six standardized bands ranging from below 2.3 up to 3.7–4.0, mirroring the analysis categories from the survey report."),
      p(class = "text-muted",
        "Switch between 'Dodge' and 'Fill (Proportions)' in the controls above to compare absolute student counts or relative percentages across groups.")
    )
  })
  
# ---- Data Table (DONE) ----
  output$data_count <- renderText({
    sprintf("%d of %d responses shown", nrow(filtered()), nrow(df))
  })
  
  output$data_tbl <- DT::renderDT({
    d <- filtered() |>
      mutate(
        # Students without jobs skipped these questions.
        `Commute time (minutes)` = if_else(
          Employed == "No", NA_real_, as.numeric(`Commute time (minutes)`)
        ),
        `Work energy rating` = if_else(
          Employed == "No", NA_real_, as.numeric(`Work energy rating`)
        )
      ) |>
      transmute(
        `Employment status` = as.character(Employed),
        `Number of courses` = `Course load`,
        `Weekly study hours` = `Weekly study hours`,
        `GPA range` = as.character(`GPA band`),
        `Weekly work hours` = `Weekly work hours`,
        `One-way commute to campus (min)` = `Commute time (minutes)`,
        `Work effort rating (1-10)` = `Work energy rating`,
        `Work sector` = `Work sector`,
        `Missed coursework deadline due to work` = as.character(`Missed a deadline`)
      )
    
    DT::datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  }, server = TRUE)

# ---- About ----
# Realistically might not need any server code for this tab because its just text
# but if we do, it would go here.
}

shinyApp(ui, server)
