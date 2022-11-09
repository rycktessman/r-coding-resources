library(ggplot2)
library(dplyr)

#made up data - you'll use your own output from sensitivity analyses
icer_main <- 552 #icer when all params fixed at means (or medians)
#output from sensitivity analyses: each row represents a different parameter
output <- data.frame("param_min"=c(0, 0.2, 1, 10, 0.5), #min values of each of 5 parameters
                      "param_max"=c(1, 0.5, 10, 100, 0.75), #max values of each of 5 parameters
                      "icer_min"=c(100, 400, 800, 350, 700), #icer when each of 5 params are set at their mins and all others are set at their means (or medians)
                      "icer_max"=c(1500, 600, 500, 1000, 300), #icer when each of 5 params are set at their maxes and all others are set at their means (or medians)
                      "param_mean"=c(0.5, 0.3, 5, 40, 0.6), #param means (or medians)
                      "param_lab"=c("Parameter 1", "Parameter 2", "Parameter 3", "Parameter 4", "Parameter 5") #param name labels for graphing
)

#add in icer_main
output <- output %>% mutate(icer_main=icer_main)

#create param_lab2, a factor variable where the factor level is based on the size of the bar in the tornado diagram (icer_diff)
output <- output %>% mutate(icer_diff=abs(icer_max-icer_min))
output <- output %>% mutate(param_lab2=factor(param_lab, levels=output$param_lab[order(output$icer_diff)]))
#create variable for whether icer_max or icer_min is larger
output <- output %>% mutate(max=if_else(icer_max>icer_min, 1, -1))
#assign variable to position means on left or right side of vertical line
output <- output %>% mutate(right=if_else((pmax(icer_min, icer_max)-icer_main) >
                                            (icer_main-pmin(icer_min, icer_max)), 
                                          1, -1))                            

#tornado graph 
width <- 0.65 #use this to adjust the height of the bars
ggplot(output) + 
  geom_rect(aes(xmin=as.numeric(param_lab2)-width/2, 
                xmax=as.numeric(param_lab2)+width/2, 
                ymin=icer_min, ymax=icer_max), fill="light blue") +
  geom_hline(yintercept=icer_main) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_min-75*max,
                label=param_min), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_max+75*max,
                label=param_max), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2)+width/4, y=icer_main+60*right,
                label=param_mean), size=3.2,
            fontface="bold") +
  scale_x_continuous(breaks=as.numeric(unique(output$param_lab2)), 
                     labels=unique(output$param_lab2)) +
  scale_y_continuous(expand=expansion(mult=c(0.1, 0.1))) +
  labs(x="", y="Incremental Cost-Effectiveness Ratio") + 
  coord_flip() + theme_bw() + 
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank())
ggsave(filename="plot1.jpg", dpi=500, height=4, width=5)
#bold numbers on the graph indicate means (or medians), numbers at the ends of the bars indicate mins/maxes 
#vertical line indicates icer when all params fixed at means (or medians)
#explain all of this in the caption

#version of tornado graph with shading by whether param is min or max
ggplot(data=output) + 
  geom_rect(data=output,
            aes(xmin=as.numeric(param_lab2)-width/2, 
                xmax=as.numeric(param_lab2)+width/2, 
                ymin=icer_min, ymax=icer_main, fill="Parameter Lower bound")) +
  geom_rect(data = output,
            aes(xmin=as.numeric(param_lab2)-width/2, 
                xmax=as.numeric(param_lab2)+width/2, 
                ymin=icer_main, ymax=icer_max, fill="Parameter Upper bound")) +
  geom_hline(yintercept=icer_main) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_min-75*max,
                label=param_min), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_max+75*max,
                label=param_max), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2)+width/4, y=icer_main+60*right,
                label=param_mean), size=3.2,
            fontface="bold") +
  scale_x_continuous(breaks=as.numeric(unique(output$param_lab2)), 
                     labels=unique(output$param_lab2)) +
  scale_y_continuous(expand=expansion(mult=c(0.1, 0.1))) +
  labs(x="", y="Incremental Cost-Effectiveness Ratio", fill="") + 
  coord_flip() + theme_bw() + 
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank())
ggsave(filename="plot2.jpg", dpi=500, height=4, width=5)

#alternative version (points and errorbars)
ggplot(output) + 
  geom_point(aes(x=as.numeric(param_lab2)), y=icer_main) +
  geom_errorbar(aes(x=as.numeric(param_lab2), ymin=icer_min, ymax=icer_max),
                width=0.5) +
  geom_hline(yintercept=icer_main) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_min-75*max,
                label=param_min), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2), y=icer_max+75*max,
                label=param_max), size=3.2) +
  geom_text(aes(x=as.numeric(param_lab2)+width/4, y=icer_main+60*right,
                label=param_mean), size=3.2,
            fontface="bold") +
  scale_x_continuous(breaks=as.numeric(unique(output$param_lab2)), 
                     labels=unique(output$param_lab2)) +
  scale_y_continuous(expand=expansion(mult=c(0.1, 0.1))) +
  labs(x="", y="Incremental Cost-Effectiveness Ratio") + 
  coord_flip() + theme_bw() + 
  theme(panel.grid=element_blank())
ggsave(filename="plot3.jpg", dpi=500, height=4, width=5)
#bold numbers on the graph indicate means (or medians), numbers at the ends of the bars indicate mins/maxes 
#vertical line indicates icer when all params fixed at means (or medians)
#explain all of this in the caption