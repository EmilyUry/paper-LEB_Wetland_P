


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

y.ro <- c(0.000014,0.56,74.4,111,5.8,4.98,4.28,4.23,4.69,6.54,17.5,5)     # 30-year median LEB runoff in mm
y.ro <- y.ro/1000                           # runoff in m
y.ro.v <- y.ro*CA10                         # runoff volume in m3/month
y.HLR <- y.ro.v/WA                          # Hydraulic loading rate in m/month
PR <- 6.77-27.8*log10(y.HLR)                # P retention (%), based off empirical 
PR <- ifelse(PR > 100, 100, PR)             # set cap so PR cannot exceed 100 %

#output data table
month <- factor(c("Jan", "Feb", "March", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                  "Oct", "Nov", "Dec"), levels = c("Jan", "Feb", "March", "Apr", 
                  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") )
data <- data.frame(month, PR)



lab2 <- c("Jan", " ", " ", "Apr", " ", " ", "Jul", " ", " ", 
          "Oct", " ", " ")

small <- ggplot(data, aes(x = month, y = PR))+
  geom_bar(stat = 'identity', fill = "#a6d854", color = "black") +
  xlab(" ") +
  ylab("P Retention (%)") +
  theme_bw() +
  scale_x_discrete(labels = lab2)


medium <- ggplot(data, aes(x = month, y = PR))+
  geom_bar(stat = 'identity', fill = "#66c2a5", color = "black") +
  xlab(" ") +
  ylab("P Retention (%)") +
  theme_bw() +
  scale_x_discrete(labels = lab2)



large <- ggplot(data, aes(x = month, y = PR))+
  geom_bar(stat = 'identity', fill = "#8da0cb", color = "black") +
  xlab(" ") +
  ylab("P Retention (%)") +
  theme_bw() +
  scale_x_discrete(labels = lab2)




small
medium
large







