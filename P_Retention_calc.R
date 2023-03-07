


### calculate P retention (%)






#### Calculate P retention % from run-off

########### Inputs

#' Designated wetland area
#' Designated catchment area
#' Runoff data -- single month, or a year of monthly totals
#' Equation for turning HLR into percent retention (lit derrived)


###########  Calculate P retention % from run-off -- single month
###

WA  <- 0.25*10000   # Wetland Area in m2
CA10 <- WA*10       # Catchment area for small catchment (m2)
CA25 <- WA*25       # Catchment area for medium catchment (m2)
CA100 <- WA*100     # Catchment area for large catchment (m2)


ro <- 5/1000        # runoff in m/month
ro.v <- ro*CA10     # runoff volume in m3/month
HLR <- ro.v/WA      # Hydraulic loading rate in m/month
PR <- 6.77-27.8*log10(HLR)    # P retention (%), based off empirical 
# relationship derived from literature



###########  Calculate P retention % from run-off -- 12 month
###

y.ro <- c(4,4,20,18,15,6,3,3,10,11,6,2)     # one year of runoff est. in mm
y.ro <- y.ro/1000                           # runoff in m
month <- c("Jan", "Feb", "March", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
           "Oct", "Nov", "Dec")
y.ro.v <- y.ro*CA10                         # runoff volume in m3/month
y.HLR <- y.ro.v/WA                          # Hydraulic loading rate in m/month
PR <- 6.77-27.8*log10(y.HLR)                # P retention (%), based off empirical 


#output data table
data <- data.frame(month, PR)





