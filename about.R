about <- tabItem(tabName = "About",
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
            tags$li("Use slider for determination of minimal particle size for determine the ",tags$b("lower"),"limits of infection risks"),
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
              tags$li("References updated"),
              tags$li("Size distribution considered"),
              tags$li("Presets for several groups and conditions were added"),
              tags$li("Particle evaporation (shrinking) considered explicitly according to Netz, 2020"),
              tags$li("Lower and upper limits of infection risks were incorporated (caused by unsharp determined sizes of the particles caused by LPC usage)")
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
)