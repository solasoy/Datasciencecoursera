library(shiny)
shinyUI(pageWithSidebar(
        headerPanel('Lock-In Amplifier Simulation'),
        sidebarPanel(
                sliderInput("signal", "Expected Signal:", min = 0.1, max = 100, value = 1.5,
                            step = 0.5),
                
                sliderInput("rate", "Sample Frequency (Hz):", min = 0, max = 10000, value = 5000,
                                 step = 50),
                sliderInput("period", "Sample Period (s):", min = 0.1, max = 10, value = 0.5,
                                step = 0.1),
                sliderInput("mfreq", "Modulation Frequency (Hz):", min = 10, max = 200, value = 50,
                                step = 1),
                numericInput('signalref', 'Reference Signal (Lock-in Detector)', 2.5,
                             min = 0.1, max = 100),
                includeText("includeText.txt")
        ),
        mainPanel(
                plotOutput('plot1'),
                h3("Recovered Signal (Average):"),
                verbatimTextOutput("average_signal")
        )
))