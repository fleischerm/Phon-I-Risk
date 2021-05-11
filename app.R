#
# This is a Shiny web application for calculation of 
# the infection risk during several phonatory activities
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

library(shiny)
library(ggplot2)
library(shinydashboard)


ui <- dashboardPage(
    skin="black",
    dashboardHeader(title = "Phon-I-Risk (v0.0.1)"),
    dashboardSidebar(width=150,
        sidebarMenu(id="tabs",
                    menuItem("Calculator", tabName = "Calculator", icon = icon("laptop")),
                    menuItem("References", tabName = "References", icon = icon("book-reader")),
                    menuItem("About", tabName = "About", icon = icon("user-secret"))
                    )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "About",
                    tags$h2("Phon-I-Risk"),
                    tags$h3("Assessing the infection risk of airborne viral transmission during phonatory activities"),
                    tags$div("Version: 0.0.1"),
                    tags$hr(style="border-color: black;"),
                    tags$h3("Benefits"),
                    tags$div("Phon-I-Risk is a simple online calculator assessing the infection risk of viral-airborne transmission caused by several phonatory activities such as speaking, singing, and shouting. This calculator combines findings as published by Lelieveld et al. and Buonanno et al. (see ",tags$b("References"),"). The calculation is based on several assumptions (among others):"),
                    tags$div(
                        tags$ul(
                            tags$li("Well mixed air in the room (quantities does not depend on spatial coordinates)"),
                            tags$li("No social distancing is (therefore) considered"),
                            tags$li("Emission rates are considered directly, which means that both activity levels and airflow of the infectious people(s) must not be taken into account"),
                            )
                        ),
                    tags$h3("Usage"),
                    tags$div(
                        tags$ul(
                            tags$li("Press",tags$b("Calculator"),"on the left panel"),
                            tags$li("Vary",tags$b("Emissionrates"),"for breathing and the chosen phonatory activity (of the infectious person(s)"),
                            tags$li("Adapt",tags$b("Room dimension"),"and",tags$b("Air exchange rate"),"(0.35 represents closed windows)"),
                            tags$li("Vary parameters of the susceptile person(s) in the room"),
                            tags$li("Editing of",tags$b("virus parameters"),"is only useful for experienced users (default values represents data for SARS-CoV-2)")
                            )
                        ),
                    tags$h3("Results"),
                    tags$div(
                        tags$ul(
                            tags$li("Risk(s) in blue color: ",tags$b("Quasi-static condition"),"- Infectious person(s) already stay in the room for a very long time"),
                            tags$li("Risk(s) in red color: ",tags$b("Transient condition"),"- Infectious person(s) entered the room immediately")
                            )
                        ),
                    tags$hr(style="border-color: black;"),
                    tags$h3("Contact"),
                    tags$div("Charité - Universitätsmedizin Berlin"),
                    tags$div("Department of Audiology and Phoniatrics"),
                    tags$div("Charitéplatz 1"),
                    tags$div("10117 Berlin"),
                    tags$a(href='https://audiologie-phoniatrie.charite.de/','https://audiologie-phoniatrie.charite.de/'),
                    tags$div(tags$b("Dr.-Ing. Mario Fleischer")),
                    tags$div("mario.fleischer[AT]charite.de"),
                    tags$hr(style="border-color: black;"),
                    tags$h3("Disclaimer & Terms of condition"),
                    tags$div("Phon-I-Risk is build with the packages",tags$a(href="https://shiny.rstudio.com/","shiny, "), tags$a(href="https://ggplot2.tidyverse.org/reference/ggplot.html","ggplot, "), "and ",tags$a(href="https://rstudio.github.io/shinydashboard/","shinydashboard "),"within the ",tags$a(href="https://www.r-project.org/","R programming language.")),
                    tags$div("This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details."),
                    ),
                    
            tabItem(tabName = "References",
                        tags$h2("References:"),
                        tags$div(
                            "Lelieveld et al, 2020, Int. J. Environ. Res. Public Health (17):8114 ",
                            tags$a(href="https://doi.org/doi:10.3390/ijerph17218114", 
                                   "https://doi.org/doi:10.3390/ijerph17218114")
                        ),
                        tags$div(
                            "Buonanno et al., 2020, Env Int (141):105794 ",
                            tags$a(href="https://doi.org/10.1016/j.envint.2020.105794", 
                                   "https://doi.org/10.1016/j.envint.2020.105794")
                        ),
                        tags$div(
                            "Salomoni, S. et al., 2016, PLoS ONE (11):1 ",
                            tags$a(href="https://doi.org/10.1371/journal.pone.0155084", 
                                   "https://doi.org/10.1371/journal.pone.0155084")
                        ),
                        tags$div(
                            "Jacot et al., 2020, Microbes and infection (22):617 ",
                            tags$a(href="https://doi.org/10.1016/j.micinf.2020.08.004", 
                                   "https://doi.org/10.1016/j.micinf.2020.08.004")
                        ),
                        tags$div(
                            "Wölfel et al., 2020, Nature (581):465 ",
                            tags$a(href="https://doi.org/10.1038/s41586-020-2196-x", 
                                   "https://doi.org/10.1038/s41586-020-2196-x")
                        ),
                        tags$div(
                            "van Doremalen et al., 2020, New England Journal of Medicine (382):1564 ",
                            tags$a(href="https://doi.org/10.1056/NEJMc2004973", 
                                   "https://doi.org/10.1056/NEJMc2004973")
                        ),
                        tags$div(
                            "Adams, W., 1993, Sacramento: California Environmental Protection Agency, Air Resources Board, Research Division ",
                            tags$a(href="https://ww2.arb.ca.gov/sites/default/files/classic//research/apr/past/a033-205.pdf", 
                                   "https://ww2.arb.ca.gov/sites/default/files/classic//research/apr/past/a033-205.pdf")
                        ),
                ),
            tabItem(tabName = "Calculator",
                        fluidPage(
                            tags$h2("Infection risk based on particle emission rates"),
                            
                            fluidRow(
                                column(3,h3("Parameters of the infectious person(s)")),
                                column(3,h3("Room parameter")),
                                column(3,h3("Parameters of susceptile person(s)")),
                                column(3,h3("Virus parameter")),
                            ),
                            
                            column(3,
                                   numericInput("PM.br", "Emissionrate for breathing [P/s]:",0, min = 0, max = 10000, step = 100),
                                   numericInput("PM.ph", "Emissionrate for phonation [P/s]:",118, min = 0, max = 100000, step = 100),
                                   sliderInput("ratio.spbr", label = "Ratio phonation to breathing:", min = 0,
                                               max = 1, value = 0.1),
                                   sliderInput("mask.eff.exhaled", label = "Mask efficiency (0 - no mask):", min = 0,
                                               max = 1, value = 0.0)
                            ),
                            column(3, 
                                   sliderInput("H.room", label = "Room height [m]:", value = 2.8, min = 2, max = 4, step = 0.1),
                                   sliderInput("A.room", "Room area [m^2]:", value = 30, min = 10, max = 100, step = 5),
                                   numericInput("V.rate", "Air exchange rate:", 0.35, min = 0.35, max = 1000),
                                   numericInput("inf.episode", "Duration of rehearsal [h]:", 1, min = 0, max = 12, step = 0.1)
                            ),  
                            column(3, 
                                   numericInput("No.susc", "Number of susceptile persons in room:", 24, min = 1, max = 100, step = 1),
                                   numericInput("resp", "Inhalatory respiration rate [l/min] (Salomoni et al. (2016), Adams (1993)):",8, min = 1, max = 20, step = 1),
                                   sliderInput("mask.eff.inhaled", label = "Mask efficiency (0 - no mask):", min = 0,
                                               max = 1, value = 0.0)
                            ),
                            column(3,
                                   numericInput("D50", "RNA for 50% infection probability (D50) (Lelieveld et al. (2020)):", 316, min = 1, max = 1000),
                                   numericInput("dp", "Deposition probability (Lelieveld et al. (2020):", 0.5, min = 0.1, max = 100),
                                   numericInput("RNA.conc", "Viral RNA in sputum [RNA/ml] (Jacot et al. (2020), Wölfel et al. (2020)):", 5e8, min = 1e0, max = 1e12),
                                   numericInput("viral.gamma", "Inactivation rate [1/h] (Doremalen et al. (2020):", 1/1.1, min = 0.1, max = 1., step = 0.1),
                                   numericInput("d.aero", "Mean wet aerosol diameter [um]:", 5, min = 0.3, max = 50, step = 1)
                            ),
                        ),
                    tags$hr(style="border-color: black;"),
                        fluidRow(
                            column(4,h3("RNA accumulation")),
                            column(3,h3("Infection risk (quasi-static conditions)")),
                            column(3,h3("Infection risk (transient conditions)")),
                            column(2,h3("Derived parameter")),
                        ),
                        
                        fluidRow(
                            column(4,
                                   mainPanel(plotOutput("plot2"))
                            ),
                            column(3,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("Ri"), style="color:blue"),
                                       mainPanel("Infection risk in % for an individual person ")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("R"), style="color:blue"),
                                       mainPanel("Infection risk in % for one person in room being infected")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("Anz"), style="color:blue"),
                                       mainPanel("Mean no. of persons that will be infected"))
                            ),
                            column(3,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("R.i.buonanno"), style="color:red"),
                                       mainPanel("Infection risk in % for an individual person")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("R.buonanno"), style="color:red"),
                                       mainPanel("Infection risk in % for one person in room being infected")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("Inf.anz.buonanno"), style="color:red"),
                                       mainPanel("Infected people")),
                            ),
                            column(2,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("D63.21")),
                                       mainPanel("RNA for 63.21% infection probability (D63.21)"))
                            ),
                        )
                    )
            
            )
        )
    )
    
server <- function(input, output) {
    D50 <- reactive({input$D50}) # #RNA for 50% infection probability (D50)
    D63.21 <- reactive({D50()*log10(exp(1))/log10(0.5)})
    RNA.conc <- reactive({input$RNA.conc}/1e-6) # [RNA/m^3] in sputum
    viral.lt <- reactive({1/input$viral.gamma*3600}) # [s] viral lifetime in aerosols (1/viral.lt can be interpreted as inactivation rate)
    P.RNA <- reactive({1-10^(log10(0.5)/D50())}) # infection risk of a single viral RNA copy (Eq. 1 in article)
    V.aero <- reactive({pi/6*(input$d.aero*1e-6)^3}) # [m^3] aerosol volume
    RNA.content <- reactive({RNA.conc()*V.aero()}) # [RNA/particle]
    
    PM.our <- reactive({(input$PM.br*(1-input$ratio.spbr)+input$PM.ph*input$ratio.spbr)*(1-input$mask.eff.exhaled)}) # [Particle/s] emission rate, our data
    Aero.conc.our <- reactive(PM.our()/(input$A.room*input$H.room)) # [particle/(m^3*s)] aerosol conc. rate in room, our data
    
    RNA.conc.aero.our <- reactive(Aero.conc.our()*RNA.content()) # [RNA/(m^3*s)] RNA conc. rate in room, our data
    RNA.dosis.our <- reactive((input$resp*1e-3/60)*RNA.conc.aero.our()*input$dp) # [RNA/s^2] RNA dosis inhaled, our data
    
    dosis.s.our <- reactive(RNA.dosis.our()/(input$V.rate/3600+1/viral.lt())*(1-input$mask.eff.inhaled)) # [RNA/s], our data
    dosis.episode.our <- reactive({dosis.s.our()*input$inf.episode*3600}) # [RNA] episode, our data
    
    R.i <- reactive({(1-(1-P.RNA())^dosis.episode.our())}) # Infection risk in % for an individual person
    R.our <- reactive({(1-(1-P.RNA())^(dosis.episode.our()*input$No.susc))}) # Infection risk in % for one person in room being infected
    Inf.anz <- reactive({round(R.i()*input$No.susc,0)})
    
    
    IVVR <- reactive({input$V.rate/3600+1/viral.lt()})
    V <- reactive({input$A.room*input$H.room})
    IR <- reactive({(input$resp*1e-3/60)*(1-input$mask.eff.inhaled)*input$dp})
    ERq <- reactive({-RNA.content()*PM.our()/D63.21()})
    n.static <- reactive({ERq()/(V()*IVVR())})
    t.static <- reactive({input$inf.episode*3600})
    dt <- reactive({5})
    t.h <- reactive({1/3600*seq(from = 0, to = t.static(), by = dt())})
    n.t <- reactive({n.static()*(1-exp(-IVVR()*t.h()*3600))})
    RNA.t <- reactive({-n.t()*D63.21()*V()})
    RNA.static <- reactive({rep(c(-n.static()*D63.21()*V()),each=length(n.t()))})
    class.n.t<- reactive({data.frame(t.h())})
    
    R.i.buonanno <- reactive({
        (1-exp(1)^(-IR()*dt()*sum(n.t())))
        }) # infection risk in % for an individual person (transient conditions, after Buonanno et al.)
    R.buonanno <- reactive({
        (1-exp(1)^(-IR()*dt()*sum(n.t()*input$No.susc)))
    }) # Infection risk in % for one person in room being infected (transient conditions, after Buonanno et al.)
    Inf.anz.buonanno <- reactive({round(R.i.buonanno()*input$No.susc,0)}) # Infected people
    
    
    output$Ri <- renderText({
        R.i <- round(R.i()*100,1)
        })
    output$R <- renderText({
        R.our <- round(R.our()*100,1)
    })
    output$Anz <- renderText({
        Inf.anz <- Inf.anz()
    })
    
    
    output$R.i.buonanno <- renderText({
        R.i.buonanno <- round(R.i.buonanno()*100,1)
    })  
    output$R.buonanno <- renderText({
        R.buonanno <- round(R.buonanno()*100,1)
    })  
    output$Inf.anz.buonanno <- renderText({
        Inf.anz.buonanno <- Inf.anz.buonanno()
    }) 
    
    
    
    
    output$D63.21 <- renderText({
        D63.21 <- round(-D63.21(),0)
    }) 
    output$plot2<-renderPlot({
        ggplot(class.n.t(),
               aes(x=t.h()))+
        geom_line(aes(y=RNA.static()),colour='blue',size=2)+
        geom_line(aes(y=RNA.t()),colour='red',size=2)+
        xlab('Duration in h')+
        ylab('viral RNA in room')},
        height = 350,width = 500
        )
    output$tmp <- renderText({
        RNA.t()
    })
}



shinyApp(ui = ui, server = server)




