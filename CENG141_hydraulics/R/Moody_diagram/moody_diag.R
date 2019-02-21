#' ---
#' title: "Moody Diagram"
#' author: "Ed Maurer, SCU"
#' date: "February 21, 2019"
#' ---

# Create a moody diagram, inspired by the script moody.py
# This is designed to allow the plotting of lab measurements on the Moody diagram

#required libraries
library(ggplot2)
library(reshape2)
library(grid)

#' Lab measurement points - arrays correspond to points measured in lab
Repoints <- c(10000,1e5)
fpoints <- c(0.05,0.045)
dfpts <- as.data.frame(cbind(Repoints,fpoints))

# Use approximation of the Swamee-Jain equation for turbulent conditions
#Swamee,P.K.,Jain, A.K. (1976).Journal of the Hydraulics Division 102(5):657–664.
jain <- function (ksoverD,Re){
  f <- 0.25/((log10((ksoverD)/3.7+5.74/(Re^0.9)))^2)
  return(f)
}

# Reynold's number ranges (x-axis)
xmin <- 600 
xmax <- 10^8
#Friction factor ranges (primary y-axis)
ymin <- 0.008
ymax <- 0.10
#friction factor lines to label
ytickloc <- c(0.008,0.009,0.01,0.015,0.02,0.025,0.03,0.04,0.05,0.06,0.07,0.08,0.08,0.1)

#relative roughness (lines to be drawn in turbulent region)
kd <- c(0.00001,0.00005,0.001,0.0002,0.0004,0.0006,0.0008,0.001,0.002,0.004,0.006,0.008,0.01,0.015,0.02,0.03,0.04,0.05)

# Laminar Portion
Relam <- seq(from = 600, to = 3000, by = 100)
flam <- 64.0/Relam
dflam <- as.data.frame(cbind(Relam,flam))

#Turbulent Regions
#Re values from 3000 to 1E8
Returb <- tail(c(2:10 %o% 10^(3:7)),-1)
fsmooth = 0.316 * Returb^(-0.25) # Blausis formula for smooth turbulent pipe flow
dfsmooth <- as.data.frame(cbind(Returb,fsmooth))

#set up empty array for lines for various values of relative roughness
f_rough <- array(numeric(),c(length(Returb),length(kd))) 
colnames(f_rough) <- kd
#calculate f values for each Re value
for (i in 1:length(kd)) {
  f_rough[,i] <- jain(kd[i],Returb)
}

#rearrange data frame for plotting
dfrough <- as.data.frame(f_rough, row.names = Returb,  col.names = kd)
dfrough$Re <- Returb
dfrough2 <- melt(dfrough, id = "Re", value.name = 'f')

#add grid lines at specific intervals 
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
yminor_breaks <- c(seq(from=0.008,to=0.05,by=0.001),seq(from=0.055,to=0.1,by=0.005))
ybreaks <- ytickloc

#plot lines for turbulent region
p1 <- ggplot(dfrough2, aes(x=Re, y=f, group = variable)) +  geom_line() +
  scale_y_log10(limits = c(ymin,ymax), expand = c(0, 0), breaks = ybreaks, minor_breaks = yminor_breaks) +
  scale_x_log10(limits = c(xmin,xmax), expand = c(0, 0), breaks = breaks, minor_breaks = minor_breaks) +
  scale_colour_discrete(guide = 'none')  +
  geom_text(data = subset(dfrough2, Re == 1e8 ), 
            aes(label = variable, x = 1e8, y = f), hjust = -.3) +
  theme_bw() +
  theme(plot.margin = unit(c(1,3,1,1), "lines"))

#add the smooth, turbulent line
p2 <- p1 + geom_line(aes(x=Returb,y=fsmooth),data=dfsmooth,inherit.aes = FALSE)

#add the laminar line
p3 <- p2 + geom_line(aes(x=Relam,y=flam),data=dflam,linetype = "dashed",inherit.aes = FALSE)

#add points, if there are any
if(length(dfpts[1])==0) {
  p4 <- p3
} else {
  p4 <- p3 + geom_point(aes(x=Repoints,y=fpoints),data=dfpts,inherit.aes=FALSE,shape=21,fill="blue",size=3)
}
  
# Code to turn off clipping -- allows labels of relative roughness outside plot region
gt1 <- ggplotGrob(p4)
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
grid.draw(gt1)
