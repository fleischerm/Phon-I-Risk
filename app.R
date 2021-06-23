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

library(ggplot2)
library(shinydashboard)
library(shinyMatrix) 

x <- c(0.5,1.0,3.0,5.0,10.0,25.0)

children_breathing <- c(0     ,0     ,0     ,0,0,0)
children_shouting <-  c(588.58,235.43,188.34,0,0,0)
children_singing <-   c(70.63 ,31.39 ,23.54 ,0,0,0)
children_speaking <-  c(15.7  ,0     ,0     ,0,0,0)
children <- matrix(c(children_breathing,children_speaking,children_singing,children_shouting),6,4)


adolescents_breathing <- c(7.85   ,0     ,0     ,0,0,0)
adolescents_shouting <-  c(1306.64,741.61,447.32,0,0,0)
adolescents_singing <-   c(196.19 ,121.64,74.555,0,0,0)
adolescents_speaking <-  c(51.01  ,15.7  ,7.85  ,0,0,0)
adolescents <- matrix(c(adolescents_breathing,adolescents_speaking,adolescents_singing,adolescents_shouting),6,4)


adults_breathing <- c(7.85   ,0     ,0     ,0,0,0)
adults_shouting <-  c(729.84 ,400.23,188.34,0,0,0)
adults_singing <-   c(774.565,439.47,188.34,0,0,0)
adults_speaking <-  c(94.17  ,32.96 ,15.7  ,0,0,0)
adults <- matrix(c(adults_breathing,adults_speaking,adults_singing,adults_shouting),6,4)



m <- matrix(c(x,
              children,
              adolescents,
              adults), 13, 6,
              byrow=TRUE, 
              dimnames = list(NULL, c("0.5e-6 m", "1.0e-6 m","3e-6 m","5e-6 m","10e-6 m","25e-6 m"))) 

rownames(m) <- c('diameter in eq. state',
                 'breathing','speaking','singing','shouting',
                 'breathing','speaking','singing','shouting',
                 'breathing','speaking','singing','shouting')

V.H2O <- 0.995 # volume percentage of water in saliva, see Netz, 2020
Phi.0 <- (1-V.H2O)/V.H2O # initial volume fraction of solutes, see Netz, 2020
RH <- 0.46 # relativ humidity measured at HRI
shrink.10 <- (Phi.0/(1-RH))^(-1/3) # for fully evaporated particles
shrink.25 <- 1.96 # for NOT fully evaporated particles (10-25 microns)

ui <- dashboardPage(
    skin="black",
    dashboardHeader(title = "Phon-I-Risk (v0.0.2)"),
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
                    tags$div("Version: 0.0.2"),
                    tags$hr(style="border-color: black;"),
                    tags$h3("Benefits"),
                    tags$div("Phon-I-Risk is a simple online calculator assessing the infection risk of viral-airborne transmission caused by several phonatory activities such as speaking, singing, and shouting. This calculator combines findings as published by Lelieveld et al. and Buonanno et al. (see ",tags$b("References"),"). The calculation is based on several assumptions (among others):"),
                    tags$div(
                        tags$ul(
                            tags$li("Well mixed air in the room (quantities does not depend on spatial coordinates)"),
                            tags$li("No social distancing is (therefore) considered"),
                            tags$li("Gravitational effects on particles are not considered"),
                            tags$li("Emission rates are considered directly, which means that both activity levels and airflow of the infectious people(s) must not be taken into account"),
                            )
                        ),
                    tags$h3("Usage"),
                    tags$div(
                        tags$ul(
                            tags$li("Press",tags$b("Calculator"),"on the left panel"),
                            tags$li("Vary",tags$b("Emissionrates"),"for breathing and the chosen phonatory activity (of the infectious person(s)) - use presets if needed"),
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
                    tags$h3("Change log"),
                    tags$div(
                        tags$ul(
                            tags$li("v.0.0.2:"),
                                    tags$ul(
                                        tags$li("Source code re-written"),
                                        tags$li("Emitted quanta calculated"),
                                        tags$li("Time increment can now be adjusted"),
                                        tags$li("Attack rate added"),
                                        tags$li("References updated"),
                                        tags$li("Size distribution considered"),
                                        tags$li("Presets for several groups and conditions were added"),
                                        tags$li("Particle evaporation (shrinking) considered explicitly according to Netz, 2020"),
                                    ),
                            tags$li("v.0.0.1:"),
                                    tags$ul(
                                        tags$li("Initial submission"),
                                    ),
                        ),
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
                    tags$div("Phon-I-Risk is build with the packages", tags$a(href="https://ggplot2.tidyverse.org/reference/ggplot.html","ggplot, "), tags$a(href="https://cran.r-project.org/web/packages/shinyMatrix/index.html","shinyMatrix, "), "and ",tags$a(href="https://rstudio.github.io/shinydashboard/","shinydashboard "),"within the ",tags$a(href="https://www.r-project.org/","R programming language.")),
                    tags$div("This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details."),
                    tags$div("The source code is hosted at gitlab: ",tags$a(href="https://github.com/fleischerm/Phon-I-Risk","https://github.com/fleischerm/Phon-I-Risk")),
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
                            "Netz, 2020, J. Phys. Chem. B 2020 (124):7093",
                            tags$a(href="https://dx.doi.org/10.1021/acs.jpcb.0c05229", 
                                   "https://dx.doi.org/10.1021/acs.jpcb.0c05229")
                        ),   
                    
                        tags$div(
                            "Adams, W., 1993, Sacramento: California Environmental Protection Agency, Air Resources Board, Research Division ",
                            tags$a(href="https://ww2.arb.ca.gov/sites/default/files/classic//research/apr/past/a033-205.pdf", 
                                   "https://ww2.arb.ca.gov/sites/default/files/classic//research/apr/past/a033-205.pdf")
                        ),
                        tags$div(
                            "Mürbe et al., 2021a, PLoS ONE (16):1",
                            tags$a(href="https://doi.org/10.1371/journal.pone.0246819", 
                                   "https://doi.org/10.1371/journal.pone.0246819")
                        ),
                        tags$div(
                            "Mürbe et al., 2021b, pre-print",
                            tags$a(href="https://doi.org/10.5281/zenodo.4770776", 
                                   "https://doi.org/10.5281/zenodo.4770776")
                        ),    
                        tags$div(
                            "Mürbe et al., 2020, pre-print",
                            tags$a(href="https://doi.org/10.31219/osf.io/znjeh", 
                                   "https://doi.org/10.31219/osf.io/znjeh")
                        ),                    
                ),
            tabItem(tabName = "Calculator",
                        fluidPage(
                            tags$h2("Infection risk based on particle emission rates"),
                            
                            fluidRow(
                                column(4,h3("Parameters of the infectious person(s)")),
                                column(2,h3("Room parameter")),
                                column(2,h3("Parameters of susceptile person(s)")),
                                column(2,h3("Virus parameter")),
                                column(2,h3("Numerical parameter")),
                            ),
                            
                            column(4,
                                   column(6,
                                   selectInput("age", "Age",
                                               c("Children" = 1,
                                                 "Adolescents" = 2,
                                                 "Adults" = 3))),
                                   column(6,
                                   selectInput("condition", "Condition",
                                               c("Speaking" = 2,
                                                 "Singing" = 3,
                                                 "Shouting" = 4
                                               ))),
                                   tags$b("Emission rates [P/s] for size classes (mostly evaporated) after Mürbe et al., 2020, 2021a, 2021b):"),
                                   matrixInput("sample",
                                               value = as.matrix(m[2:3,]),
                                   ),
                                   column(6,
                                   sliderInput("t.phon.breath", label = "Ratio phonation to breathing:", min = 0,
                                               max = 1, value = 1)), # 0.1
                                   column(6,
                                   sliderInput("kappa.exhal", label = "Mask efficiency (0 - no mask):", min = 0,
                                               max = 1, value = 0.0),
                                    ),
                                   numericInput("t.episode", "Exposure time [h]:", 1, min = 0, max = 12, step = 0.1)
                            ),
                            column(2, 
                                   sliderInput("H.room", label = "Room height [m]:", value = 2.8, min = 2, max = 10, step = 0.1),
                                   sliderInput("A.room", "Room area [m^2]:", value = 30, min = 10, max = 300, step = 5), # 100
                                   numericInput("AER", "Air exchange rate:", 0.35, min = 0.35, max = 1000),
                            ),  
                            column(2, 
                                   numericInput("N.susc", "Number of susceptile persons in room:", 24, min = 1, max = 100, step = 1),
                                   numericInput("V.inhal.resp", "Inhalatory respiration rate [l/min] (Salomoni et al. (2016), Adams (1993)):",8, min = 1, max = 20, step = 1),
                                   sliderInput("kappa.inhal", label = "Mask efficiency (0 - no mask):", min = 0,
                                               max = 1, value = 0.0)
                            ),
                            column(2,
                                   numericInput("D50", "RNA for 50% infection probability (D50) (Lelieveld et al. (2020)):", 316, min = 1, max = 1000),
                                   numericInput("k.lung", "Deposition probability (Lelieveld et al. (2020)):", 0.5, min = 0.1, max = 100),
                                   numericInput("RNA.conc", "Viral RNA in sputum [RNA/ml] (Jacot et al. (2020), Wölfel et al. (2020)):", 5e8, min = 1e0, max = 1e12),
                                   numericInput("lambda", "Viral half time [h] (Doremalen et al. (2020)):", 1.1, min = 0.1, max = 10., step = 0.1),
                            ),
                            column(2,
                                   numericInput("dt", "Time increment for transient analysis [s]:", 600, min = 1, max = 3600, step = 1)
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
                                       sidebarPanel(textOutput("Pi.static"), style="color:blue"),
                                       mainPanel("Infection risk in % for an individual person (attack rate)")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("PN.static"), style="color:blue"),
                                       mainPanel("Infection risk in % for one person in room being infected")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("N.inf.static"), style="color:blue"),
                                       mainPanel("Mean no. of persons that will be infected")),
                            ),
                            column(3,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("Pi.transient"), style="color:red"),
                                       mainPanel("Infection risk in % for an individual person (attack rate)")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("PN.transient"), style="color:red"),
                                       mainPanel("Infection risk in % for one person in room being infected")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("N.inf.transient"), style="color:red"),
                                       mainPanel("Mean no. of persons that will be infected")),
                            ),
                            column(2,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("D63.21")),
                                       mainPanel("RNA for 63.21% infection probability (D63.21)")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("quanta.emitted")),
                                       mainPanel("Emitted quanta of infectious person(s) in [q/h]")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("V.room")),
                                       mainPanel("Room volume in [m^3]")),
                            ),
                        )
                    )
            )
        )
    )
    
server <- function(input, output,session) {
    # build subset
    index_breath <- reactive((as.numeric(input$age)-1)*dim(adults)[2]+2)
    index_phon <- reactive((as.numeric(input$age)-1)*dim(adults)[2]+as.numeric(input$condition)+1)
    
    # update input matrix
    observe({
        updateMatrixInput(session,
                          "sample",
                          as.matrix(m[c(index_breath(),index_phon()),])
                          )
    })
    p.data.phon <- reactive(as.numeric(as.matrix(input$sample)[2,])) # read (updated) PM data
    p.data.breath <- reactive(as.numeric(as.matrix(input$sample)[1,])) # read (updated) PM data
    
    shrink <- reactive((c(shrink.10,shrink.10,shrink.10,shrink.10,shrink.10,shrink.25)))
    diameter <- reactive(m[1,]*1e-6*shrink()) # [m] diameter of particles (un-evaporated)
    Volume <- reactive(pi/6*diameter()^3) # [m^3] volume of spheres with diameter d

    Volume.rate.phon <- reactive(sum(Volume()*p.data.phon())) # [m^3/s] Volume rate phonation
    Volume.rate.breath <- reactive(sum(Volume()*p.data.breath())) # [m^3/s] Volume rate breathing
    Volume.rate <- reactive({(Volume.rate.breath()*(1-input$t.phon.breath)+
                              Volume.rate.phon()*input$t.phon.breath)*(1-input$kappa.exhal)}) # [m^3/s] Volume rate total
    
    RNA.conc <- reactive({input$RNA.conc}/1e-6) # [RNA/m^3] in sputum
    
    D50 <- reactive({input$D50}) # #RNA for 50% infection probability (D50)
    D63.21 <- reactive(-{D50()*log10(exp(1))/log10(0.5)})
    
    RNA.exhal <- reactive({RNA.conc()*Volume.rate()*(1-input$kappa.exhal)}) # [RNA/s] # new
    
    AER <- reactive({input$AER/3600}) # [1/s] air exchange rate
    lambda <- reactive({1/(3600*input$lambda)}) # [1/s] viral inactivation rate in aerosols
    IVVR <- reactive({AER()+lambda()}) # [1/s] infectious removal rate
    V.room <- reactive({input$A.room*input$H.room}) # room volume
    t.static <- reactive({input$t.episode*3600})
    dt <- reactive({input$dt}) # [s] time increment
    t.transient <- reactive({1/3600*seq(from = 0, to = t.static(), by = dt())})
    RNA.room.static <- reactive({RNA.exhal()/(V.room()*(AER()+lambda()))}) # [RNA/m^3] RNA concentration in room
    RNA.room.transient <- reactive({RNA.room.static()*(1-exp(-IVVR()*t.transient()*3600))}) # [RNA/m^3] RNA concentration in room
    
    V.inhal.resp <- reactive({input$V.inhal.resp*1e-3/60}) # [m^3/s] inhalation rate
    k.lung <- reactive({input$k.lung})
    RNA.inhal.transient <- reactive({RNA.room.transient()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.transient <- reactive({dt()*sum(RNA.inhal.transient()*k.lung())}) # [RNA] RNA deposition per episode
    RNA.inhal.static <- reactive({RNA.room.static()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.static <- reactive({t.static()*sum(RNA.inhal.static()*k.lung())}) # [RNA] RNA deposition per episode

    P.RNA <- reactive({1-10^(log10(0.5)/D50())}) # infection risk of a single viral RNA copy (Eq. 1 in article)
    P.i.transient <- reactive({(1-(1-P.RNA())^RNA.transient())}) # Infection risk in % for an individual person
    P.N.transient <- reactive({(1-(1-P.RNA())^(RNA.transient()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.inf.transient <- reactive({P.i.transient()*input$N.susc})
    
    P.i.static <- reactive({(1-(1-P.RNA())^RNA.static())}) # Infection risk in % for an individual person
    P.N.static <- reactive({(1-(1-P.RNA())^(RNA.static()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.inf.static <- reactive({P.i.static()*input$N.susc})    
    
    quanta.emitted <- reactive({RNA.exhal()/D63.21()*3600}) # [q/h] emitted quanta
    class.t.transient<- reactive({data.frame(t.transient())}) # needed for diagram

    
    
    output$Pi.transient <- renderText({
        P.i.transient <- round(P.i.transient()*100,1)
        })
    output$PN.transient <- renderText({
        P.N.transient <- round(P.N.transient()*100,1)
    })
    output$N.inf.transient <- renderText({
        N.inf.transient <- round(N.inf.transient(),0)
    })
 
    
    output$Pi.static <- renderText({
        P.i.static <- round(P.i.static()*100,1)
    })
    output$PN.static <- renderText({
        P.N.static <- round(P.N.static()*100,1)
    })
    output$N.inf.static <- renderText({
        N.inf.static <- round(N.inf.static(),0)
    })


    output$D63.21 <- renderText({
        D63.21 <- round(D63.21(),0)
    }) 
    output$quanta.emitted <- renderText({
        quanta.emitted <- round(quanta.emitted(),0)
    })
    output$V.room <- renderText({
        V.room <- round(V.room(),0)
    })
    output$V.aero <- renderText({
        V.aero <- quanta.emitted()*D63.21()/3600
    })    
    
    output$plot2<-renderPlot({
        ggplot(class.t.transient(),
               aes(x=t.transient()))+
        geom_line(aes(y=V.room()*RNA.room.static()),colour='blue',size=2)+
        geom_line(aes(y=V.room()*RNA.room.transient()),colour='red',size=2)+
        geom_point(aes(y=V.room()*RNA.room.static()),colour='blue',size=5)+
        geom_point(aes(y=V.room()*RNA.room.transient()),colour='red',size=5)+
        xlab('Duration in h')+
        scale_y_continuous(
            name = 'viral RNA in room',
            sec.axis = sec_axis( trans=~.*(1/round(D63.21(),0)), name='Quanta in room')
            )
        },
        height = 250,width = 500
        )
}



shinyApp(ui = ui, server = server)





