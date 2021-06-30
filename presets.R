x.max <- c(0.5,1.0,3.0,5.0,10.0,25.0)
x.diff <- x.max-c(0.3,x.max[1:5])
# x.min <- c(0.3,x.max[1:5])
# x.min <- c(0.4,0.75,1.5,4.0,7.5,17.5)
# x.min <- x.max*.9

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



m <- matrix(c(x.max,
              children,
              adolescents,
              adults), 13, 6,
            byrow=TRUE, 
            dimnames = list(NULL, c(">0.3-0.5 mum", ">0.5-1.0 mum",">1-3 mum",">3-5 mum",">5-10 mum",">10-25 mum"))) 

rownames(m) <- c('diameter in eq. state',
                 'breathing','speaking','singing','shouting',
                 'breathing','speaking','singing','shouting',
                 'breathing','speaking','singing','shouting')