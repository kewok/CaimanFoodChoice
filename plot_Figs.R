# Visualizations:
library(tidyverse)
library(patchwork)

expResult_Lineage <- read.csv('fitted_interactive_mvProbit_Lineage.csv')
expResult_PurportedSubspecies <- read.csv('fitted_interactive_mvProbit_PurportedSubspecies.csv')
expResult_Region <- read.csv('fitted_interactive_mvProbit_Region.csv')

expResult_noInt_Lineage <- read.csv('fitted_additive_mvProbit_Lineage.csv')
expResult_noInt_PurportedSubspecies <- read.csv('fitted_additive_mvProbit_PurportedSubspecies.csv')
expResult_noInt_Region <- read.csv('fitted_additive_mvProbit_Region.csv')
medem <- read.csv('medem_revision.csv')  

prey <- c('Invertebrates','Fish','Tetrapods')

# Remove the spaces
medem[,'PurportedSubspecies'] <- ifelse(medem[,'PurportedSubspecies']=='C. c.  crocodilus','C. c. crocodilus',medem[,'PurportedSubspecies'])
medem[,'Lineage'] <- ifelse(medem[,'Lineage']=='C. c.  crocodilus','C. c. crocodilus',medem[,'Lineage'])
medem[,'Region'] <- ifelse(medem[,'Region']==' cis-Andes','cis-Andes',medem[,'Region'])

f <- function(criteria, model, y, show_legend = TRUE) {
  if (model=='Interaction')
  {
    if (criteria=='Region')
      expResult <- expResult_Region
    if (criteria=='PurportedSubspecies')
      expResult <- expResult_PurportedSubspecies
    if (criteria=='Lineage')
      expResult <- expResult_Lineage
  } else {
    if (criteria=='Region')
      expResult <- expResult_noInt_Region
    if (criteria=='PurportedSubspecies')
      expResult <- expResult_noInt_PurportedSubspecies
    if (criteria=='Lineage')
      expResult <- expResult_noInt_Lineage
  }
    
  plot_data <- data.frame()
  
  # Combine data for each group in the criteria
  for (x in 1:length(unique(medem[,criteria]))) {
    vals <- which(medem[,criteria]==unique(medem[,criteria])[x])
    temp_data <- data.frame(
      Length = medem[vals,'Length'],
      Probability = expResult[vals,y],
      Group = factor(unique(medem[,criteria])[x])
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  p <- ggplot(plot_data, aes(x = Length, y = Probability, color = Group, shape = Group)) +
    geom_point(size = 2.5) + 
    scale_x_continuous(limits = c(10, 130)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = 'TL (cm)',
      y = paste0('Pr(', prey[y], ' in stomach)'),
      title = paste(prey[y],'by',ifelse(criteria=='PurportedSubspecies','Purported Subspecies', criteria)) 
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (criteria == "Lineage") {
    lineage_labels <- levels(plot_data$Group)
    italic_taxa <- lapply(lineage_labels, function(x) bquote(italic(.(x))))
    
    p <- p + 
      scale_shape_manual(values = c(16, 15), labels = italic_taxa) +
      scale_color_manual(values = c("#ff7c02", "#3fd2bc"), labels = italic_taxa)
    } 
    else if (criteria == "PurportedSubspecies") {
    subspecies_labels <- levels(plot_data$Group)
    italic_subspecies <- lapply(subspecies_labels, function(x) bquote(italic(.(x))))
    
    p <- p + 
      scale_shape_manual(values = c(16, 17, 15), labels = italic_subspecies) +
      scale_color_manual(values = c("#ff7c02", "#66fd02", "#3fd2bc"), , labels = italic_subspecies)
  } else if (criteria == "Region") {
    p <- p + 
      scale_shape_manual(values = c(16, 17, 15, 3)) +
      scale_color_manual(values = c("#ff7c02", "#66fd02", "#3fd2bc", "#e41ab5"))
  }
  
  if (show_legend) {
    # Format the legend title appropriately
    legend_title <- ifelse(criteria == 'PurportedSubspecies', 'Purported Subspecies', criteria)
    
    p <- p + 
      labs(
        color = paste0(legend_title, ":"),
        shape = paste0(legend_title, ":")
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "lines"),
        # Split legends with >2 elements over multiple rows
        legend.box = "vertical",
        legend.spacing.y = unit(0.1, "cm"),
        legend.margin = margin(0, 0, 0, 0)
      ) +
      # Determine number of columns based on number of elements
      guides(
        color = guide_legend(nrow = ifelse(length(unique(plot_data$Group)) > 2, 2, 1), 
                              byrow = TRUE),
        shape = guide_legend(nrow = ifelse(length(unique(plot_data$Group)) > 2, 2, 1),
                              byrow = TRUE)
      )
  } else {
    p <- p + guides(color = "none", shape = "none")
  }
  return(p)
}

pij <- list()
i <- 0
for (model in c('Additive','Interaction'))
    {
    for (prey_type in 1:3)
        {
        for(criterion in c('Region','PurportedSubspecies','Lineage'))
            {
            i <- i + 1
            if (prey_type != 3)
                {
                pij[[i]] <- f(criterion,model, prey_type, FALSE)
                }
            else 
                {
                pij[[i]] <- f(criterion,model, prey_type, TRUE)
                }
            }
        }
    }

pdf('Figure1_revision.pdf',width=12,height=8)
(pij[[1]] + pij[[2]] + pij[[3]]) / 
(pij[[4]] + pij[[5]] + pij[[6]]) / 
(pij[[7]] + pij[[8]] + pij[[9]]) +
plot_annotation(
    title = "Figure 1",
    tag_levels = 'A', 
    theme = theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.tag = element_text(size = 16, face = "bold")  
    )
)
dev.off()

pdf('Figure2_revision.pdf',width=12,height=8)
(pij[[10]] + pij[[11]] + pij[[12]]) / 
(pij[[13]] + pij[[14]] + pij[[15]]) / 
(pij[[16]] + pij[[17]] + pij[[18]]) +
plot_annotation(
    title = "Figure 2",
    tag_levels = 'A', 
    theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.tag = element_text(size = 16, face = "bold")
    )
)
dev.off()
