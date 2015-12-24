# Lock-in Amplifier Simulator Function: 
lockinamp <- function(Asig,Aref,sampfreq,sampleperiod,modfreq) {
        
        h <- seq(from = 1, to = sampfreq*sampleperiod, by = 1)
        t <- h/sampfreq # time (sec)
        
        pref <- 0.6 # Reference phase
        psig <- 0.5 # signal phase
        
        # Noisy Signal (simulated)
        rsig <- rnorm(length(t),mean=Asig,sd=Asig*3) + 
                2.54*sin(120*pi*t) + 3.48*sin(240*pi*t) 
        
        # Modulated Signal (simulated)
        msig <- rsig * sin(2*modfreq*pi*t + psig*pi/180)
        
        
        # Phase sensitive detection
        ref1 <- Aref*sin(2*modfreq*pi*t + pref*pi/180)
        ref2 <- Aref*sin(2*modfreq*pi*t + (pref+90)*pi/180)
        v1 <- msig*ref1
        v2 <- msig*ref2
        
        #apply low pass filter to orthogal phase-shifted psd outputs
        fc <- 0.5
        flag <- 0
        while (flag==0){
                tau <- 1/(2*pi*fc)
                dt <- 1/sampfreq
                a <- dt/(dt+tau)
                x <- as.numeric(array(0,c(1,length(t))))
                y <- x
                x[1] <- a*v1[1]
                y[1] <- a*v2[1]
                for (k in 2:length(t)) {
                        x[k] = a*v1[k] + (1-a)*x[k-1]
                        y[k] = a*v2[k] + (1-a)*y[k-1]
                }
                
                if ((sum(t >= 6*tau)) >0 & length(t[t >= 6*tau]) >= 5){
                        flag=1
                        break
                } else {
                        fc <- fc + 0.1
                }  
        }
        
        n <- which(t >= 5*tau)
        c <- 2/Aref*sqrt(x^2 + y^2) # Estimated signal
        d <- atan2(y,x)   # Estimated phase
        
        return(list("rsig"=rsig,"c"=c,"n"=n,"t"=t))
}

set.seed(1234)
library(shiny)
shinyServer(function(input, output) {
            
                result <- reactive({
                        lockinamp(input$signal, input$signalref,input$rate,
                                  input$period,input$mfreq)
                })
            
                
                
                # Plot Noisy signal versus recovered signal
                output$plot1 <- renderPlot({
                        tm <- result()$t
                        n <- result()$n
                        cr <- result()$c
                        rsig <- result()$rsig
                        par(mar=c(5,4,4,4))
                        plot(tm,rsig,pch=1,type="l",cex=1.5,
                             xlim=c(tm[1],tm[length(tm)]),
                             xlab="Time (s)",ylab="",
                             main="Noisy Signal: Expected Signal (blue line) + white noise + 60&120Hz Interference \nFiltered Signal: red line") 
                        mtext("Raw (noisy) Signal",side=2,line=2)
                        
                        
                        par(new=T)
                        plot(tm[n[1]+1 : length(n)-1],cr[n[1]+1 : length(n)-1],
                             axes=F,xlab="",ylab="",type="l",col="red",lwd=2,
                             ,col.axis="red",ylim=c(round(min(rsig/4)),round(max(rsig/4))))
                        axis(side=4)
                        mtext("Filtered Signal",side=4,line=2,col="red")
                        abline(h=input$signal,col="blue",lwd=2)
                    
                        })
                
                output$average_signal <- renderPrint({
                        n <- result()$n
                        cr <- result()$c
                        toString(mean(cr[n[1]+1 : length(n)-1]))
                })
        
        
})