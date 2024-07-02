
####################################################################
################## DESCRIPTIVES ##############################
####################################################################

####################################################################
################## FIGURE 1: HEXBIN CHART ##############################
####################################################################

# Reshape to wide at the family level
wide<-reshape(as.data.frame(df),direction="wide", idvar="randomfamid", timevar="random")

# Plot
hexbin<-ggplot(wide, aes(x=risk1.0, y=risk1.1) ) +
  geom_hex(bins = 80) +
  geom_abline(color="darkgoldenrod1", linetype="dashed", size=0.3) +
  theme_bw() +
  scale_x_continuous(breaks=c(-2, -1, 0, 1,2,3,4,5,6))+
  scale_y_continuous(breaks=c(-2, -1, 0, 1,2,3,4,5,6))+
  scale_fill_continuous(type = "viridis")+
  xlab("Twin 1") +
  ylab ("Twin 2")+
  theme(axis.title.y = element_text(size = 10, angle = 90)) +
  theme(axis.title.x = element_text(size = 10, angle = 00)) +
  theme(legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 7)) +
  ggtitle("Hexbin chart: twins' medical risk scale") +
  theme(plot.title = element_text(size=12, face="bold")) 

hexbin +  labs(fill = "Count")
