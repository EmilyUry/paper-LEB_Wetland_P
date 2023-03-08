

library(ggplot2)
library(cowplot)
library(tidyverse) 
library(ggpmisc)

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/LEB_Wetland_P/Datasets")

### P retention modeled using HLR from lit synthesis


x <- read.csv("Monthly_P_Lit.csv")
x <- read.csv("Lit_wetland_P_monthly.csv")
head(x)


x$Short_ID <- as.factor(x$Short_ID)
x$Short_Ref <- as.factor(x$Short_Ref)
levels(x$Short_Ref)
x$HLR_m_mo <- x$Monthly_Inflow_m3_month/x$SA_m2
hist(x$HLR_m_mo)



x %>%
  ggplot(aes(x=  HLR_m_mo, y = TP_Retention_percent ))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab("HLR (m/month)") +
  ylab("TP Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 4, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank()) 


x %>%
  ggplot(aes(x=  HLR_m_mo, y = SRP_Retention_percent ))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab("HLR (m/month)") +
  ylab("TP Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 4, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank()) 





x$mod.TPret <- -27.5*(log(x$HLR_m_mo))+7.38
x$mod.SRPret <- -23.9*(log(x$HLR_m_mo))+22.6
# hist(x$mod.TPret, breaks = 20)
# abline(v=0, lwd = 3, col = 2)
# 
# x %>%
#   ggplot(aes(x=  TP_Retention_percent, y = mod.TPret )) +
#   geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
#   theme_classic(base_size = 8) +
#   xlim(-200,100) +
#   ylim(-200,300)
# 
# 
# x$mod.TPout <- x$TP_IN_g_m2_mo - (x$TP_IN_g_m2_mo*x$mod.TPret/100) 
# 
# 
# x %>%
#   ggplot(aes(x=  TP_OUT_g_m2_mo, y = mod.TPout )) +
#   geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
#   theme_classic(base_size = 8) +
#   ylim(0, 1) +
#   geom_abline(slope = 1)
#   



### put cap on so retention cannot be over 100 percent

x$mod.TPret.cap <- ifelse(x$mod.TPret > 100, 100, x$mod.TPret)
hist(x$mod.TPret.cap, breaks = 20)
hist(x$TP_Retention_percent, xlim = c(-100,100), breaks = 100)
abline(v=0, lwd = 3, col = 2)


### frequency distribution of measured vs modelled TP retention
lab <- c(rep("measured", 344), rep("modeled", 344))
TP_retention <- c(x$TP_Retention_percent, x$mod.TPret.cap)
df <- data.frame(lab, TP_retention)
ggplot(df, aes(x = TP_retention, color = lab, fill = lab)) +
  geom_density(alpha = 0.3, lwd = 1) +
  xlim(-100,100) +
  theme_classic(base_size = 10) +
  scale_fill_manual(values = c("#bababa55", "#ff000025" )) +
  scale_color_manual(values = c("black", "red" )) +
  xlab("TP Retention (%)")


x$mod.SRPret.cap <- ifelse(x$mod.SRPret > 100, 100, x$mod.TPret)
hist(x$mod.SRPret.cap, breaks = 20)
hist(x$SRP_Retention_percent, xlim = c(-100,100), breaks = 100)
abline(v=0, lwd = 3, col = 2)


### frequency distribution of measured vs modelled TP retention
lab <- c(rep("measured", 344), rep("modeled", 344))
SRP_retention <- c(x$SRP_Retention_percent, x$mod.SRPret.cap)
df <- data.frame(lab, SRP_retention)
ggplot(df, aes(x = SRP_retention, color = lab, fill = lab)) +
  geom_density(alpha = 0.3, lwd = 1) +
  xlim(-100,100) +
  theme_classic(base_size = 10) +
  scale_fill_manual(values = c("#bababa55", "#ff000025" )) +
  scale_color_manual(values = c("black", "red" )) +
  xlab("PO4 Retention (%)")





### plot measured vs modeled

x %>%
  ggplot(aes(x=  TP_Retention_percent, y = mod.TPret.cap )) +
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
  theme_classic(base_size = 8) +
  xlim(-200,100) +
  ylim(-200,100) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") 



### calculated modeled TP out
x$mod.TPout <- x$TP_IN_g_m2_mo - (x$TP_IN_g_m2_mo*x$mod.TPret.cap/100) 
x$mod.SRPout <- x$SRP_IN_g_m2_mo - (x$SRP_IN_g_m2_mo*x$mod.SRPret.cap/100) 

## compare modeled out verse measured
x %>%
  ggplot(aes(x=  TP_OUT_g_m2_mo, y = mod.TPout )) +
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
  theme_classic(base_size = 8) +
  ylim(0, 1) +
  geom_abline(slope = 1)
cor.test(x$TP_OUT_g_m2_mo, x$mod.TPout, method = "pearson")


x %>%
  ggplot(aes(x=  SRP_OUT_g_m2_mo, y = mod.SRPout )) +
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
  theme_classic(base_size = 8) +
  ylim(0, 1) +
  geom_abline(slope = 1)
cor.test(x$SRP_OUT_g_m2_mo, x$mod.SRPout, method = "pearson")





### calculated modeled TP retention (g_m2)
x$mod.TPret_g_m2 <- (x$TP_IN_g_m2_mo*(x$mod.TPret.cap/100))
x$mod.SRPret_g_m2 <- (x$SRP_IN_g_m2_mo*x$mod.SRPret.cap/100) 

x$TP_retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo


hist(x$mod.TPret_g_m2)
hist(x$TP_retention)


## compare modeled retention verse measured
x %>%
  ggplot(aes(x=  TP_retention, y = mod.TPret_g_m2 )) +
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
  theme_classic(base_size = 8) +
  #ylim(0, 1) +
  geom_abline(slope = 1)
cor.test(x$TP_retention, x$mod.TPret_g_m2, method = "pearson")


x %>%
  ggplot(aes(x=  SRP_retention, y = mod.SRPret_g_m2 )) +
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3) +
  theme_classic(base_size = 8) +
  #ylim(0, 1) +
  geom_abline(slope = 1)
cor.test(x$SRP_retention, x$mod.SRPret_g_m2, method = "pearson")





















##### old stuff

plot(x$HLR_m_mo, x$TP_Retention_percent, log = "x", pch = 21,
     bg = x$Short_Ref)
legend("bottomleft", legend = levels(x$Short_Ref), pch = 21, 
       pt.bg = c(1:4))
abline(h=0)

plot(x$HLR_m_mo, x$TP_Retention_percent, log = "x", pch = 21,
     bg = x$Short_ID, col = x$Short_Ref, cex = 2)
legend("bottomleft", legend = levels(x$Short_ID), pch = 21, 
       pt.bg = c(1:20))
legend("bottomright", legend = levels(x$Short_Ref), pch = 21, 
       col = c(1:4))
# ON <- x[which(x$Short_Ref == 18),]
# ON$Short_ID <- as.factor(ON$Short_ID)
levels(ON$Short_ID)

ON$Wetland_ID <- as.factor(ON$Wetland_ID)
plot(ON$HLR_m_mo, ON$TP_Retention_percent, log = "x", pch = 16,
     col = c("red", "black", "blue", "purple", "orange", "green", "cyan", "pink")[ON$Wetland_ID])
legend("bottomleft", legend = levels(ON$Wetland_ID), pch = 16, 
       col = c("red", "black", "blue", "purple", "orange", "green", "cyan", "pink"))

ON$CAWA <- ON$Catchment_area/ON$SA_m2*10000
hist(ON$CAWA)
table(ON$Wetland_ID, ON$CAWA)


x$CAWA <- x$Catchment_area/x$SA_m2*10000
x %>%
  group_by(Short_ID) %>%
  summarise(mean.CAWA = mean(CAWA))
