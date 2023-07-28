###=============================================================================
### Bar plots for tapir relative abundance index using ggplot
### Cate Geiman
### June, 2023
### Output: .tif
###=============================================================================

#load required packages
require(ggplot2)
require(readr)

#import dataset
OrderedRAI1 <- read.csv("RAI data/OrderedRAI2.csv")



#------------------------- Regular-sized bar plot ------------------------------

#create .tif file
tiff(file = "regularRAIbarplot.tif", width=13500, height=11000, res=1200)

#create plot
ggplot(OrderedRAI1, aes(x = Site, y = RAI, 
                       fill = Species, 
                       group = factor(Species))) +
  labs(x = expression(bold("Site")), y= expression(bold("RAI")), 
       fill = expression(bold("Species")), 
       title = expression(bold("Tapir Relative Abundance Index (RAI) by Site")))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("#B7E6A5", "#46AEA0", "#00718B", "#003147")) +  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, size = 11, vjust = 0.7), axis.text.y = element_text(size = 11), #axis text adjustments
        axis.title.x = element_text(size = 15, vjust = 0.1), axis.title.y = element_text(size = 15, vjust = 0.1), #axis title adjustments
        plot.title = element_text(hjust = 0.5, size = 20), #plot title adjustment
        legend.text = element_text(size = 15), legend.title = element_text(size = 18), #legend adjustments
        axis.line = element_line(colour = "black"), #axis line adjustment
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #removing background grid
        panel.border = element_blank(), #removing plot border
        axis.ticks.x = element_line(lineend = 2), axis.ticks.length.x = unit(0.1, "inch"))+ #adjusting axis ticks
  ylim(0, 200)+
  scale_x_discrete(limits = 
                     c("CBQTC","PILA-K","PNTMM","RFRM","PILA-V","CBS","El Copal","PNCH","Savegre",
                       "PNC","EES", "CSP", "GBR","RDSA", "RBSP","TFR","TNNS")) #x-axis text labels
#.tif output
par(xpd=TRUE)
dev.off()



#------------------ Small bar plot (for formatting purposes) -------------------
#same bar plot but with adjusted text/title/label sizes, etc.

#create .tif file
jpeg(file = "smallRAIbarplot1.jpeg", width=6000, height=5000, res=600)

#create plot
ggplot(OrderedRAI1, aes(x = Site, y = RAI, 
                        fill = Species, 
                        group = factor(Species))) +
  labs(x = expression(bold("Site")), y= expression(bold("RAI")), 
       fill = expression(bold("Species")), 
       title = expression(bold(" ")))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("#B7E6A5", "#46AEA0", "#00718B", "#003147")) +  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 20, vjust = 0.7, hjust = 0.95), axis.text.y = element_text(size = 20), #axis text adjustments
        axis.title.x = element_text(size = 25, vjust = 0.1), axis.title.y = element_text(size = 25, vjust = 0.1), #axis title adjustments
        plot.title = element_text(hjust = 0.25, size = 35), #plot title adjustment
        legend.text = element_text(size = 25), legend.title = element_text(size = 30), #legend adjustments
        axis.line = element_line(colour = "black"), #axis line adjustment
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #removing background grid
        panel.border = element_blank(), #removing plot border
        axis.ticks.x = element_line(lineend = 2), axis.ticks.length.x = unit(0.1, "inch"))+ #adjusting axis ticks
  ylim(0, 200)+
  scale_x_discrete(limits = 
                     c("CBQTC","PILA-K","PNTMM","RFRM","PILA-V","CBS","El Copal","PNCH","Savegre",
                       "PNC","EES", "CSP", "GBR","RDSA", "RBSP","TFR","TNNS")) #x-axis text labels
#.tif output
par(xpd=TRUE)
dev.off()
