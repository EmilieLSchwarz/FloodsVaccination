
# maps

mcv = district_propvax12mo%>%
  ggplot () +
  geom_sf(aes(fill=MCV), # This is the average MCV vaccinated (no weights as of now & including only over 12 month old 
          color="dim grey", size=.1) + 
  scale_fill_continuous(high ="#ffd966", low = "#c01f1f", na.value = "grey50", breaks= c(60,70,80,90,100),labels= c("60%","70%","80%", "90%","100%"),
                      name="Percent fully vaccinated")+#, breaks=c(60,70,80,90,100)), 
                      #)+
  labs(#title = "MCV",
       #subtitle = "hello",
       caption="*These results are weighted", 
       fill = NULL) +
  theme_blank() + 
  guides(fill = guide_legend(reverse=T))

#mcv = annotate_figure(mcv, top =text_grob("MCV", color = "black", face = "bold", size=16))
mcv = annotate_figure(mcv, fig.lab ="MCV", fig.lab.pos = "top", fig.lab.size= 16, fig.lab.face= "bold")



dpt = district_propvax6mo%>%
  ggplot () +
  geom_sf(aes(fill=DTP), # This is the average MCV vaccinated (no weights as of now & including only over 12 month old 
          color="dim grey", size=.1) + 
  scale_fill_continuous(high ="#ffd966", low = "#c01f1f", na.value = "grey50",breaks= c(60,65,70,75,80,85,90,95,100),labels= c("60%","65%","70%","75%","80%", "85%","90%","95%","100%"),
                        name="Percent fully vaccinated")+#, breaks=c(60,70,80,90,100)), 
  #)+
  labs(
    #subtitle = "hello",
    caption="*These results are weighted", 
    fill = NULL) +
  theme_blank() + 
  guides(fill = guide_legend(reverse=T))

#dpt = annotate_figure(dpt, top =text_grob("DPT", color = "black", face = "bold", size=16))

dpt = annotate_figure(dpt, fig.lab ="DPT", fig.lab.pos = "top", fig.lab.size= 16, fig.lab.face= "bold")



opv = district_propvax6mo%>%
  ggplot () +
  geom_sf(aes(fill=OPV), # This is the average MCV vaccinated (no weights as of now & including only over 12 month old 
          color="dim grey", size=.1) + 
  scale_fill_continuous(high ="#ffd966", low = "#c01f1f", na.value = "grey50", breaks= c(80,85,90,95,100),labels= c("80%", "85%","90%","95%","100%"),
                        name="Percent fully vaccinated")+#, breaks=c(60,70,80,90,100)), 
  #)+
  labs(
    #subtitle = "hello",
    caption="*These results are weighted", 
    fill = NULL) +
  theme_blank() + 
  guides(fill = guide_legend(reverse=T))

#opv = annotate_figure(opv, top =text_grob("OPV", color = "black", face = "bold", size=16))
opv = annotate_figure(opv, fig.lab ="OPV", fig.lab.pos = "top", fig.lab.size= 16, fig.lab.face= "bold")




bcg = district_propvax%>%
  ggplot () +
  geom_sf(aes(fill=BCG), # This is the average MCV vaccinated (no weights as of now & including only over 12 month old 
          color="dim grey", size=.1) + 
  scale_fill_continuous(high ="#ffd966", low = "#c01f1f", na.value = "grey50", breaks= c(60,65,70,75,80,85,90,95,100),labels= c("60%","65%","70%","75%","80%", "85%","90%","95%","100%"),
                        name="Percent fully vaccinated")+#, breaks=c(60,70,80,90,100)), 
  #)+
  labs(
    #subtitle = "hello",
    caption="*These results are weighted", 
    fill = NULL) +
  theme_blank() + 
  guides(fill = guide_legend(reverse=T))

#bcg = annotate_figure(bcg, top =text_grob("BCG", color = "black", face = "bold", size=16))
bcg = annotate_figure(bcg, fig.lab ="BCG", fig.lab.pos = "top", fig.lab.size= 16, fig.lab.face= "bold")


fic = district_propvax12mo%>%
  ggplot () +
  geom_sf(aes(fill=FIC), # This is the average MCV vaccinated (no weights as of now & including only over 12 month old 
          color="dim grey", size=.1) + 
  scale_fill_continuous(high ="#ffd966", low = "#c01f1f", na.value = "grey50", breaks= c(55,65,75,85,95,100),labels= c("55","65%", "75%","85%","95%","100%"),
                        name="Percent fully vaccinated")+#, breaks=c(60,70,80,90,100)), 
  #)+
  labs(
    #subtitle = "hello",
    caption="*These results are weighted", 
    fill = NULL) +
  theme_blank() + 
  guides(fill = guide_legend(reverse=T))

#fic = annotate_figure(fic, top =text_grob("FIC", color = "black", face = "bold", size=16))
fic = annotate_figure(fic, fig.lab ="FIC", fig.lab.pos = "top", fig.lab.size= 16, fig.lab.face= "bold")




pal2 <- colorNumeric(c("#e9d4b7", "#5885AF", "#0C2C84"), values(flood_area_percent_mask_2),
                     na.color = "transparent")

pal <- colorRampPalette(c("#e9d4b7", "#5885AF", "#0C2C84"))
colorlength <- 100
## Map data
flood_area_percentPlot <- rasterVis::levelplot(flood_area_percent_mask_2, 
                                               margin = FALSE,                       
                                               colorkey = TRUE,
                                               main = NULL,
                                               par.settings = list(
                                                 axis.line = list(col = 'transparent') 
                                               ),
                                               xlab = NULL,
                                               ylab = NULL,
                                               scales = list(draw = FALSE),            
                                               col.regions = pal,
                                               #col.regions = plasma(n = colorlength, direction = -1),
                                               at = seq(from = 0, to = 80, length.out = colorlength))
#   +latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2))  
flood_area_percentPlot = annotate_figure(flood_area_percentPlot, top =text_grob("Percent days flooded", color = "black", face = "bold", size=16))

finalfig = ggarrange(flood_area_percentPlot,fic, nrow =2)
annotate_figure(finalfig, top =text_grob("Fig 1. Proportion Fully Vaccinated by Subregion and Severity of Flooding Across Bangladesh", color = "black", face = "bold", size=20))

suppfig = ggarrange(bcg,dpt,opv,mcv)
annotate_figure(suppfig, top =text_grob("Fig S1. Proportion Fully Vaccinated by Subregion", color = "black", face = "bold", size=20))



