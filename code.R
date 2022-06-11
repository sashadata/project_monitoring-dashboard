
---
  title: "Community Action Agency: Monitoring Dashboard"
output: html_document
runtime: shiny 

-------------------------------------------------------------------------
  Inputs{.sidebar}
-------------------------------------------------------------------------
  
```{r}

selectInput("Program", "Program:", choices = c("Education ", "Employment"))

selectInput("Caseworker", "Caseworker", choices = c("Robert", "Patricia"))

library(lubridate)
library(tidyverse)
library(plogr)
library(ggthemes)
library(ggfittext)
library(mgcv)
library(treemapify)
library(selectr)
library(rematch)


csbg_template<-read.csv("agencydata.csv")
csbg_template$date<-mdy(csbg_template$Date)
csbg_template <-csbg_template %>%
  mutate(AgeGroup= case_when(Age >= 19 & Age <=24 ~ "19-24",
                             Age >= 25 & Age <=34 ~ "25-34",
                             Age >= 35 & Age <=44 ~ "35-44",
                             Age >= 45 & Age <=54 ~ "45-54",
                             Age >= 55 & Age <=64 ~ "55-64",
                             Age >= 65 & Age <=70 ~ "65-70",
                             Age >=71 ~ "19-24"))

csbg_template_1<-csbg_template %>%
  group_by(Program, Caseworker, date) %>%
  count()

csbg_template_3<- csbg_template_1 %>%
  mutate(datea=as.Date(date),
         m=month(date, label = TRUE),
         d=day(date))
```

Row {data-height=500}
-----------------------------------------------------------------------
  
  
  ```{r}
library(ggplot2)

plotOutput("plot")
output$plot <- renderPlot({
  csbg_template %>% group_by(Program, Outcome, Caseworker)%>%
    summarise(freq1=n()) %>% mutate(total=sum(freq1)) %>%
    filter(Program==input$Program)%>%
    filter(Caseworker==input$Caseworker)%>%
    ggplot(aes(x=reorder(Outcome, freq1), y=freq1, fill=Outcome)) +
    geom_bar(stat="identity", position="dodge", width=0.6) +
    geom_text(aes(label=freq1), position=position_dodge(width=1), 
              vjust=0, hjust=-.6,  size=6, color="black")+ylim(0,800)+scale_fill_tableau()+
    theme_minimal()+theme(legend.position = "none", axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          axis.title.y = element_blank(),
                          text=element_text(size=16),
                          plot.title = element_text(hjust = 0))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle("Achieved Outcomes")+
    coord_flip()
  
})
```


```{r}

library(treemap)
library(gridExtra)
library(treemapify)
library(ggplot2)
library(ggthemes)
plotOutput("plot3")


output$plot3<-renderPlot({
  csbg_template_3_a %>% filter(Program==input$Program & Caseworker==input$Caseworker)%>%
    filter(n >=20) %>%
    ggplot(aes(fill = Service, 
               area = n, 
               label = paste(Service))) +
    geom_treemap(alpha=1) + 
    geom_treemap_text(colour = "black", 
                      place = "centre")+
    theme(plot.margin = unit(c(1,2,2,1), "cm"),
          plot.title = element_text(size=19))+
    ggtitle("Popular Services")+scale_fill_tableau(
      palette = "Tableau 20",
      type = "regular")+
    theme(legend.position = "none")
  
})
```


Row {data-height=500}
--------------------------------------------
  
  ```{r}

csbg_template_3_a<-csbg_template %>%
  group_by(Program, Caseworker, Service) %>%
  count()
library(ggalluvial)
plotOutput("plot2")
output$plot2 <- renderPlot({
  
  csbg_template %>%
    group_by(Gender, Race, Outcome, Program, Caseworker, Housing, AgeGroup, Language) %>%
    count() %>% filter(Program==input$Program)%>%
    filter(Caseworker==input$Caseworker)%>%
    ggplot(
      aes(axis1 = Gender, axis2 = Language, axis3 = AgeGroup,
          y = n)) +
    scale_x_discrete(limits = c("Gender", "Language", "Age"), expand = c(0, 0)) +
    xlab("Demographic") +
    geom_alluvium(aes(fill = Outcome),alpha = 1, decreasing = FALSE, width = 1/4,
                  curve_type = "sigmoid") +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    ggtitle("Graph")+scale_fill_tableau()+theme_minimal()+
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right",text=element_text(size=16),
          legend.text = element_text(size=12), legend.title = element_blank())+
    ggtitle("Connecting Demographics to Outcomes")
  
})
```

```{r}
library(ggplot2)

plotOutput("plot4")
output$plot4 <- renderPlot({
  csbg_template_3 %>% na.omit() %>% filter(Program==input$Program)%>%
    filter(Caseworker==input$Caseworker)%>%
    ggplot(aes(d, n, fill="m"))+geom_area()+scale_fill_tableau()+
    facet_wrap(~m)+theme_minimal()+ggtitle("Frequnecy of Activities")+geom_line(size=1, col="red")+theme(legend.position = "none", text = element_text(size=16),
                                                                                                         axis.title.x = element_blank(), axis.title.y = element_blank() )+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
})

```

