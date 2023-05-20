library(shiny)
library(shinylogs)
library(data.table)
library(ggplot2)
library(pwr)

# dnorm_dt returns a data table representing the normal distribution using h and 
# size to calculate the mean
dnorm_dt <- function(h, size, label) {
  x <- qnorm(seq(0.001, 0.999, 0.001), mean=h*sqrt(size/2))
  data.table(x=x, y=dnorm(x, mean=h*sqrt(size/2)), label=label)
}

dbinom_dt <- function(size, prob, label) {
  x <- qbinom(seq(0.001, 0.999, 0.001), size, prob)
  dt <- data.table(x=x, y=dbinom(x, size, prob), label=label)
  dt$x <- dt$x / size
  dt
}

# sample_size calculates the sample size using the provided parameters
sample_size <- function(cr, mde, alpha, power, alternative) {
  h <- ifelse(alternative == "less", ES.h(cr*(1-mde), cr), ES.h(cr*(1+mde), cr))
  pwr.2p.test(h=h, sig.level=alpha, power=power, alternative=alternative)
}

# calc_power calculates the statistical power given the provided sample size
calc_power <- function(h, size, alpha, sides) {
  round(100*(1-pnorm(qnorm(1-alpha/sides), mean=abs(h*sqrt(size/2)))), digits=2)
}



# server logic
function(input, output, session) {
  track_usage(storage_mode = store_null())
  
  observeEvent(input$go, {
    # inputs
    mde <- input$mde/100
    probA <- input$prob/100
    alpha <- input$alpha/100
    power <- input$power/100
    alternative <- input$alternative
    progress <- input$progress/100
    
    # calculations
    probB <- ifelse(alternative == "less", probA*(1-mde), probA*(1+mde))
    sides <- ifelse(alternative == "two.sided", 2, 1)
    ss <- sample_size(probA, mde, alpha, power, alternative)
    required_size <- ceiling(ss$n)
    h <- ss$h
    size <- ceiling(required_size * progress)
    current_power <- calc_power(h, size, alpha, sides)
    
    # outputs
    output$size <- renderText(paste("Sample Size: ", size,
                                    "\nRequired Sample Size: ", required_size, 
                                    "\nPower: ", current_power, "%", sep=""))
    
    output$distPlot <- renderPlot({
      dt <- rbindlist(list(dnorm_dt(0, size, "control") ,dnorm_dt(h, size, "variation")))
      z <- qnorm(1-alpha/sides)
      
      if(!input$transform) {
        dt <- rbindlist(list(dbinom_dt(size, probA, "control"), dbinom_dt(size, probB, "variation")))
        z <- qbinom(1-alpha/sides, size, probA) / size
      }
      
      plot <- ggplot(dt, aes(x,y,colour=label)) + theme(legend.position="none")
      
      # draw power
      if(input$show_power && input$transform) {
        if(alternative == "two.sided" || alternative == "greater") {
          plot <- plot + geom_area(data=dt[label == "variation" & x > z], fill="chartreuse", alpha=0.75)
        } else {
          plot <- plot + geom_area(data=dt[label == "variation" & x < -z], fill="chartreuse", alpha=0.75)
        }
        plot <- plot + annotate("text", label = paste("Power: ", current_power, "%", sep=""), x=2+progress*2, y=0.4)
      }

      # draw significance
      if(input$show_sig && input$transform) {
        # upper tail
        if(alternative == "two.sided" || alternative == "greater") {
          plot <- plot + geom_area(data=dt[label == "control" & x > z], fill="coral", alpha=0.75)
        }
        
        # lower tail
        if(alternative == "two.sided" || alternative == "less") {
          plot <- plot + geom_area(data=dt[label == "control" & x < -z], fill="coral", alpha=0.75)
        }
      }

      plot + geom_line(linewidth=2)
    })
  })
}
