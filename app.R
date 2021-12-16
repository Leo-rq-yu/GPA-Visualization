#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(bslib)


gpa = read.csv("data/gpa_sub.csv")



# Define UI for application that draws a histogram
ui = navbarPage(
    theme = bs_theme(bootswatch = "united", primary = "#13294b", secondary = "#E84A27"),
    title = span("UIUC Course GPA 2010 - 2021", style = "color:#E84A27"),
    tabPanel(
        title = "App", 
        titlePanel("Mean GPA Visualization"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId = "year", label = "Year: ", min = 2010, max = 2021, value = 2015, step = 1),
                sliderInput(inputId = "ma", label = "Expect GPA: ", min = 0.0, max = 4.0, value = 2.0, step = 0.1),
                textInput(inputId = "c", label = "Find Course: (Course list changed each year)", value = 'cs'),
                width = 30
            ),
            mainPanel(
                plotOutput("plot"),
                width = 10,
                height = 30
                )
        )
    ),
    tabPanel(
        title = "Table", 
        titlePanel("All Course Information"),
        mainPanel(
            dataTableOutput("table"),
            width = 20
            )
    ),
    tabPanel(title = "About", includeMarkdown("about.Rmd"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #observeEvent(eventExpr = input$c, handlerExpr = {updateNumericInput(inputId = "w", value = 0.0)}, suspended = TRUE)
    
    gpa_div = reactive({
        
        gpa_div = gpa %>%
            filter(Year >= input$year) %>%
            group_by(Course) %>%
            mutate(total = sum(`A.` + A + `A..1` + `B.` + B + `B..1` + `C.` + C + `C..1` + `D.` + D + `D..1` + F + W),
                   mgpa = (4.0*sum(`A.`+A) + 3.67*sum(`A..1`) + 3.33*sum(`B.`) + 3.0*sum(B) + 2.67*sum(`B..1`) + 2.33*sum(`C.`) + 2.0*sum(C) + 1.67*sum(`C..1`) + 1.33*sum(`D.`) + 1.0*sum(D) + 0.67*sum(`D..1`)) / total,
                   aper =  sum(`A.` + A) / total)
        
        if (input$ma != 4.0){
            if (toupper(input$c) %in% gpa_div$Course || toupper(input$c) %in% gpa_div$Subject || toupper(input$c) %in% gpa_div$Number) {
                gpa_div = gpa_div %>%
                    filter(toupper(input$c) == Course || toupper(input$c) == Subject || toupper(input$c) == Number) %>%
                    filter(mgpa >= input$ma) %>%
                    filter(!duplicated(Course)) %>%
                    select(Course, Subject, Number, Course.Title, mgpa, total, aper)
            } else {
                gpa_div = gpa_div %>%
                    filter(mgpa >= input$ma) %>%
                    filter(!duplicated(Course)) %>%
                    select(Course, Subject, Number, Course.Title, mgpa, total, aper)
            }

        } else {
            if (toupper(input$c) == gpa_div$Course || toupper(input$c) == gpa_div$Subject || toupper(input$c) == gpa_div$Number) {
                gpa_div = gpa_div %>%
                    filter(toupper(input$c) == Course || toupper(input$c) == Subject || toupper(input$c) == Number) %>%
                    filter(!duplicated(Course)) %>%
                    select(Course, Subject, Number, Course.Title, mgpa, total, aper)
            } else {
                gpa_div = gpa_div %>%
                    filter(!duplicated(Course)) %>%
                    select(Course, Subject, Number, Course.Title, mgpa, total, aper)
            }
        }
        gpa_div
    })
    
    
    gpa_tab = reactive({
        gpa %>%
            filter(Year >= input$year) %>%
            group_by(Course) %>%
            mutate(total = sum(`A.` + A + `A..1` + `B.` + B + `B..1` + `C.` + C + `C..1` + `D.` + D + `D..1` + F + W),
                   mgpa = (4.0*sum(`A.`+A) + 3.67*sum(`A..1`) + 3.33*sum(`B.`) + 3.0*sum(B) + 2.67*sum(`B..1`) + 2.33*sum(`C.`) + 2.0*sum(C) + 1.67*sum(`C..1`) + 1.33*sum(`D.`) + 1.0*sum(D) + 0.67*sum(`D..1`)) / total,
                   aper =  sum(`A.` + A) / total) %>%
            mutate(across(mgpa:aper, rd)) %>%
            filter(!duplicated(Course)) %>%
            rename(`Year and Term` = YearTerm, `Course Subject` = Subject, `Course Number` = Number, `Course Title` = Course.Title, `Course Instructor` = Primary.Instructor, `Mean GPA` = mgpa, `Student Number` = total, `A percent` = aper) %>%
            select(`Year and Term`, `Course Subject`, `Course Number`, `Course Title`, `Course Instructor`, `Mean GPA`, `Student Number`, `A percent`)
    })
    output$table = renderDataTable({gpa_tab()
            })
    output$plot = renderPlot({
        ggplot(data = gpa_div(), aes(x = aper, y = mgpa)) +
            geom_count(aes(shape = 'circle', color = aper, alpha = 0.7, size = total)) +
            scale_size_area(max_size = 10) + 
            xlim(0,1) +
            ylim(0.0,4.0) +
            labs(
                x = 'Percent of students get "A" in this course',
                y = 'Mean GPA of this course',
                color = '"A" Percent in this course',
                size = '# of students taken this course',
                title = 'Course Mean GPA'
                ) +
            theme(plot.background = element_rect(size = 15),
                  plot.margin = margin(3,3,3,3),
                  legend.background = element_rect(fill = "lightblue", size = 5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
