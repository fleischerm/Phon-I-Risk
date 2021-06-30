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


source("references.R")
source("about.R")
source("presets.R")


# Parameters according to Netz, 2020
V.H2O <- 0.995 # volume percentage of water in saliva
Phi.0 <- (1-V.H2O)/V.H2O # initial volume fraction of solutes
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
            about,
            references,
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
                                   column(12,
                                          sliderInput("d.min", label = "Minimal particle diameter, best case [1]:", min = 0,
                                                      max = 1, value = 1),
                                          
                                          ),
                                   # column(6,
                                   #        sidebarPanel(uiOutput("diameter.min.evap")),
                                   # ),

                                   column(4,
                                   sliderInput("t.phon.breath", label = "Ratio phonation time to exposure time:", min = 0,
                                               max = 1, value = 1)),
                                   column(4,
                                   sliderInput("kappa.exhal", label = "Mask efficiency (0 - no mask):", min = 0,
                                               max = 1, value = 0.0),
                                    ),
                                   column(4,
                                          numericInput("t.episode", "Exposure time [h]:", 1, min = 0, max = 12, step = 0.1)
                                          ),

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
                                   numericInput("D50", "RNA for 50% infection probability (D50) (Karimzadeh et al. (2021)):", 69, min = 1, max = 1000),
                                   numericInput("k.lung", "Deposition probability (Lelieveld et al. (2020)):", 0.5, min = 0.1, max = 100),
                                   numericInput("RNA.conc", "Viral RNA in sputum [RNA/ml] (Jacot et al. (2020), Wölfel et al. (2020), Jones et al. (2021)):", 1e7, min = 1e0, max = 1e12),
                                   numericInput("lambda", "Viral half time [h] (Doremalen et al. (2020)):", 1.1, min = 0.1, max = 10., step = 0.1),
                            ),
                            column(2,
                                   numericInput("dt", "Time increment for transient analysis [s]:", 60, min = 1, max = 3600, step = 1)
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
                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("P.min.i.static"), style="color:blue",width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                          sidebarLayout(
                                            sidebarPanel(textOutput("P.max.i.static"), style="color:blue"),
                                            mainPanel("Infection risk in % for an individual person (attack rate)")),
                                   ),

                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("P.min.N.static"), style="color:blue",width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                            sidebarLayout(
                                                sidebarPanel(textOutput("P.max.N.static"), style="color:blue"),
                                                mainPanel("Infection risk in % for one person in room being infected")),
                                   ),

                                   column(5,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("N.min.inf.static"), style="color:blue",width=6),
                                       mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("N.max.inf.static"), style="color:blue"),
                                              mainPanel("Mean no. of persons that will be infected")),
                                   ),
                            ),
                            column(3,
                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("P.min.i.transient"), style="color:red",width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                            sidebarLayout(
                                            sidebarPanel(textOutput("P.max.i.transient"), style="color:red"),
                                            mainPanel("Infection risk in % for an individual person (attack rate)")),
                                   ),

                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("P.min.N.transient"), style="color:red",width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("P.max.N.transient"), style="color:red"),
                                       mainPanel("Infection risk in % for one person in room being infected")),
                                   ),

                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("N.min.inf.transient"), style="color:red",width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("N.max.inf.transient"), style="color:red"),
                                       mainPanel("Mean no. of persons that will be infected")),
                                   ),
                            ),
                            column(2,
                                   sidebarLayout(
                                       sidebarPanel(textOutput("D63.21")),
                                       mainPanel("RNA for 63.21% infection probability (D63.21)")),

                                   column(5,
                                          sidebarLayout(
                                              sidebarPanel(textOutput("quanta.min.emitted"),width=6),
                                              mainPanel("to",width=4)),
                                   ),
                                   column(7,
                                        sidebarLayout(
                                            sidebarPanel(textOutput("quanta.max.emitted")),
                                            mainPanel("Emitted quanta of infectious person(s) in [q/h]")),
                                   ),

                                   sidebarLayout(
                                       sidebarPanel(textOutput("V.room")),
                                       mainPanel("Room volume in [m^3]")),
                                   sidebarLayout(
                                       sidebarPanel(textOutput("AVF")),
                                       mainPanel("Air volume flow in [m^3/h]")),
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
    
    shrink.max <- reactive((c(shrink.10,shrink.10,shrink.10,shrink.10,shrink.10,shrink.25))) # worst case scenario, all particles are on the UPPER limits regarding their size
    diameter.max <- reactive(m[1,]*1e-6*shrink.max()) # [m] maximal diameter of particles (un-evaporated)
    Volume.max <- reactive(pi/6*diameter.max()^3) # [m^3] maximal volume of spheres with diameter d
    
    shrink.min <- reactive((c(shrink.10,shrink.10,shrink.10,shrink.10,shrink.10,shrink.10))) # best case scenario, all particles are on the LOWER limits regarding their size
    diameter.min.evap <- reactive((x.max-input$d.min*x.diff)) # [m] minimal diameter of particles (evaporated)
    diameter.min <- reactive(diameter.min.evap()*1e-6*shrink.min()) # [m] minimal diameter of particles (un-evaporated)
    Volume.min <- reactive(pi/6*diameter.min()^3) # [m^3] minimal volume of spheres with diameter d

    Volume.max.rate.phon <- reactive(sum(Volume.max()*p.data.phon())) # [m^3/s] Volume rate phonation
    Volume.max.rate.breath <- reactive(sum(Volume.max()*p.data.breath())) # [m^3/s] Volume rate breathing
    Volume.max.rate <- reactive({(Volume.max.rate.breath()*(1-input$t.phon.breath)+
                              Volume.max.rate.phon()*input$t.phon.breath)*(1-input$kappa.exhal)}) # [m^3/s] Volume rate total
    Volume.min.rate.phon <- reactive(sum(Volume.min()*p.data.phon())) # [m^3/s] Volume rate phonation
    Volume.min.rate.breath <- reactive(sum(Volume.min()*p.data.breath())) # [m^3/s] Volume rate breathing
    Volume.min.rate <- reactive({(Volume.min.rate.breath()*(1-input$t.phon.breath)+
                                      Volume.min.rate.phon()*input$t.phon.breath)*(1-input$kappa.exhal)}) # [m^3/s] Volume rate total
    
    RNA.conc <- reactive({input$RNA.conc}/1e-6) # [RNA/m^3] in sputum
    D50 <- reactive({input$D50}) # #RNA for 50% infection probability (D50)
    D63.21 <- reactive(-{D50()*log10(exp(1))/log10(0.5)})
    
    RNA.max.exhal <- reactive({RNA.conc()*Volume.max.rate()*(1-input$kappa.exhal)}) # [RNA/s]
    RNA.min.exhal <- reactive({RNA.conc()*Volume.min.rate()*(1-input$kappa.exhal)}) # [RNA/s]
    
    AER <- reactive({input$AER/3600}) # [1/s] air exchange rate
    lambda <- reactive({1/(3600*input$lambda)}) # [1/s] viral inactivation rate in aerosols
    IVVR <- reactive({AER()+lambda()}) # [1/s] infectious removal rate
    V.room <- reactive({input$A.room*input$H.room}) # room volume
    t.static <- reactive({input$t.episode*3600})
    dt <- reactive({input$dt}) # [s] time increment
    t.transient <- reactive({1/3600*seq(from = 0, to = t.static(), by = dt())})
    
    RNA.max.room.static <- reactive({RNA.max.exhal()/(V.room()*(AER()+lambda()))}) # [RNA/m^3] RNA concentration in room
    RNA.max.room.transient <- reactive({RNA.max.room.static()*(1-exp(-IVVR()*t.transient()*3600))}) # [RNA/m^3] RNA concentration in room
    RNA.min.room.static <- reactive({RNA.min.exhal()/(V.room()*(AER()+lambda()))}) # [RNA/m^3] RNA concentration in room
    RNA.min.room.transient <- reactive({RNA.min.room.static()*(1-exp(-IVVR()*t.transient()*3600))}) # [RNA/m^3] RNA concentration in room
    
    V.inhal.resp <- reactive({input$V.inhal.resp*1e-3/60}) # [m^3/s] inhalation rate
    k.lung <- reactive({input$k.lung})
    
    RNA.max.inhal.transient <- reactive({RNA.max.room.transient()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.max.transient <- reactive({dt()*sum(RNA.max.inhal.transient()*k.lung())}) # [RNA] RNA deposition per episode
    RNA.max.inhal.static <- reactive({RNA.max.room.static()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.max.static <- reactive({t.static()*sum(RNA.max.inhal.static()*k.lung())}) # [RNA] RNA deposition per episode
    RNA.min.inhal.transient <- reactive({RNA.min.room.transient()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.min.transient <- reactive({dt()*sum(RNA.min.inhal.transient()*k.lung())}) # [RNA] RNA deposition per episode
    RNA.min.inhal.static <- reactive({RNA.min.room.static()*V.inhal.resp()*(1-input$kappa.inhal)}) # [RNA/s] RNA inhalation rate
    RNA.min.static <- reactive({t.static()*sum(RNA.min.inhal.static()*k.lung())}) # [RNA] RNA deposition per episode

    P.RNA <- reactive({1-10^(log10(0.5)/D50())}) # infection risk of a single viral RNA copy (Eq. 1 in article)
    
    P.max.i.transient <- reactive({(1-(1-P.RNA())^RNA.max.transient())}) # Infection risk in % for an individual person
    P.max.N.transient <- reactive({(1-(1-P.RNA())^(RNA.max.transient()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.max.inf.transient <- reactive({P.max.i.transient()*input$N.susc})
    P.min.i.transient <- reactive({(1-(1-P.RNA())^RNA.min.transient())}) # Infection risk in % for an individual person
    P.min.N.transient <- reactive({(1-(1-P.RNA())^(RNA.min.transient()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.min.inf.transient <- reactive({P.min.i.transient()*input$N.susc})
    
    P.max.i.static <- reactive({(1-(1-P.RNA())^RNA.max.static())}) # Infection risk in % for an individual person
    P.max.N.static <- reactive({(1-(1-P.RNA())^(RNA.max.static()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.max.inf.static <- reactive({P.max.i.static()*input$N.susc})    
    quanta.max.emitted <- reactive({RNA.max.exhal()/D63.21()*3600}) # [q/h] emitted quanta
    P.min.i.static <- reactive({(1-(1-P.RNA())^RNA.min.static())}) # Infection risk in % for an individual person
    P.min.N.static <- reactive({(1-(1-P.RNA())^(RNA.min.static()*input$N.susc))}) # Infection risk in % for one person in room being infected
    N.min.inf.static <- reactive({P.min.i.static()*input$N.susc})    
    quanta.min.emitted <- reactive({RNA.min.exhal()/D63.21()*3600}) # [q/h] emitted quanta
    
    class.t.transient<- reactive({data.frame(t.transient())}) # needed for diagram

    
    
    output$P.max.i.transient <- renderText({
        P.max.i.transient <- round(P.max.i.transient()*100,1)
        })
    output$P.max.N.transient <- renderText({
        P.max.N.transient <- round(P.max.N.transient()*100,1)
    })
    output$N.max.inf.transient <- renderText({
        N.max.inf.transient <- round(N.max.inf.transient(),0)
    })
    output$P.min.i.transient <- renderText({
        P.min.i.transient <- round(P.min.i.transient()*100,1)
    })
    output$P.min.N.transient <- renderText({
        P.min.N.transient <- round(P.min.N.transient()*100,1)
    })
    output$N.min.inf.transient <- renderText({
        N.min.inf.transient <- round(N.min.inf.transient(),0)
    })
 
    
    output$P.max.i.static <- renderText({
        P.max.i.static <- round(P.max.i.static()*100,1)
    })
    output$P.max.N.static <- renderText({
        P.max.N.static <- round(P.max.N.static()*100,1)
    })
    output$N.max.inf.static <- renderText({
        N.max.inf.static <- round(N.max.inf.static(),0)
    })
    output$P.min.i.static <- renderText({
        P.min.i.static <- round(P.min.i.static()*100,1)
    })
    output$P.min.N.static <- renderText({
        P.min.N.static <- round(P.min.N.static()*100,1)
    })
    output$N.min.inf.static <- renderText({
        N.min.inf.static <- round(N.min.inf.static(),0)
    })


    output$D63.21 <- renderText({
        D63.21 <- round(D63.21(),0)
    }) 
    
    output$quanta.max.emitted <- renderText({
        quanta.max.emitted <- round(quanta.max.emitted(),0)
    })
    output$quanta.min.emitted <- renderText({
        quanta.min.emitted <- round(quanta.min.emitted(),0)
    })
    
    
    output$V.room <- renderText({
        V.room <- round(V.room(),0)
    })
    output$V.max.aero <- renderText({
        V.max.aero <- quanta.max.emitted()*D63.21()/3600
    })   
    output$V.min.aero <- renderText({
        V.min.aero <- quanta.min.emitted()*D63.21()/3600
    }) 
    output$AVF <- renderText({
        AVF <- round(input$AER*V.room())
    }) 
    
    output$diameter.min.evap <- renderTable({
        diameter.min.evap <- matrix(diameter.min.evap(),nrow=1)
    }) 
    
    output$plot2<-renderPlot({
        ggplot(class.t.transient(),
               aes(x=t.transient()))+
        geom_line(aes(y=V.room()*RNA.max.room.static()),colour='blue',size=2)+
            geom_ribbon(aes(ymax=V.room()*RNA.max.room.static(), ymin=V.room()*RNA.min.room.static()), fill="blue", alpha=.1)+
        geom_line(aes(y=V.room()*RNA.max.room.transient()),colour='red',size=2)+
            geom_ribbon(aes(ymax=V.room()*RNA.max.room.transient(), ymin=V.room()*RNA.min.room.transient()), fill="red", alpha=.1)+
        geom_point(aes(y=V.room()*RNA.max.room.static()),colour='blue',size=2)+
        geom_point(aes(y=V.room()*RNA.max.room.transient()),colour='red',size=2)+
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





