# AutoGraph v1
# Thomas Ashhurst
# 2018-01-16


###################################################### 1. INSTALL AND-OR LOAD PACKAGES ###################################################### 

    ### 1.1. Install packages
        install.packages("ggpubr")
        install.packages("cowplot")
        
        library(ggplot2)
        library(ggpubr)
        library(scales)
        library(devtools)
        #library(cowplot)
        
        detach("package:cowplot", unload=TRUE)
        
        #devtools::install_github("baptiste/egg")
        #library(egg)

    ### 1.2 Working directory
        dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
        setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
        getwd()
        PrimaryDirectory <- getwd()
        PrimaryDirectory
    
        #setwd("/Users/Tom/Desktop/auto_test_2/")
        #PrimaryDirectory <- getwd()
        #PrimaryDirectory
        
        list.files(path = PrimaryDirectory, ".csv")

###################################################### 2. SETUP DATA ######################################################     
    
    ### 2.1. Setup data
        
        # data <- iris # use for TESTING
        data <- read.csv("test_data.csv")
        data <- as.data.frame(data)
        head(data)
        
        # What column defines your groups (grouping on the X axis, dependent variable e.g. infection groups) -- convert this column to a 'factor' and assign to Xaxis
        Xaxis <- data$Species <- as.factor(data$Species) # insert the name of the column after both $'s
        Xaxis
    
        # Assign order and colour of groups
        as.matrix(unique(Xaxis))
        
        Group_Order <- c("setosa", "versicolor", "virginica") # left to right
        Colour_Order_Fill <- c("Black", "red", "#377eb8") # number of distinct columns must be the same as the number of groups
        Colour_Order_Lines <- c("Black", "red", "#377eb8")
        
        # Create string of columns to analyse -- REMOVE the column that represents the groups (the column that defines the X axis)
        ColNames <- names(data)
        ColNames
        
        ColNames <- ColNames[c(1:4)]
        ColNames
        
        # Labels for X and Y axis
        X_axis_label <- " " # Leave empty for no axis label
        Y_axis_label <- "Cells per femur" # Y axis labl
    
        Dot.Size <- 7   # Default 7, large dot
        
    ### 2.2. Setup statistics
        
        # COMING SOON: run gaussian assessment -- help decide test
        
        # Comparing overall variance (ANOVA, Kruskal-Wallis etc)
        Run_variance_assessment <- 1      
        Variance_test <- "kruskal.test"   # can be "kruskal.test" (non-parametric) or "anova" (parametric)
        
        # Pair-wise comparisons (T-test, Willcox test etc)
        Run_pairwise_assessment <- 1
        Pairwise_test <- "wilcox.test"  # default is "wilcox.test" (non-parametric), can be "t.test" (parametric)
        
        # IF performing pair-wise comparisons, specificy which groups you want to compare statistically -- not all columns have to be compared
        as.matrix(unique(Xaxis))    
        my_comparisons <- list(c("setosa", "versicolor"), # comparison 1
                               c("versicolor", "virginica"), # comparison 2
                               c("setosa", "virginica")) # comparison 3 etc
    
        
        
###################################################### 3. Plotting Loop (END USER INPUT) ######################################################       

        
        
    ### 3.1. Set output directory
        setwd(PrimaryDirectory)
        dir.create("Output", showWarnings = FALSE)
        setwd("Output")
        OutputDirectory <- getwd()
        OutputDirectory
    
    ### 3.2. LOOP
  
        for (a in ColNames) {
            # a <- "Sepal.Length" # ONLY REQUIRED when testing iris dataset without the loop
            
            # Naming
            a <- noquote(a)
            Yaxis <- a
        
            # Create plot name
            plotname <- a
            plotname <- gsub("\\.", " ", plotname)
            plotname <- gsub("_", " ", plotname)
        
            # assign max and min values
                max_y_value <- max(data[a], na.rm = TRUE)
                max_y_value_p10 <- max_y_value*1.1
                max_y_value_p40 <- max_y_value*1.4
        
                min_y_value<- min(data[a], na.rm = TRUE)
                bottom_y <- min_y_value
                bottom_y
                
            # CREATE PLOTS
            p <- ggplot(data, aes(x=Xaxis, y=data[a]))+ #, fill=Species)) + # can use fill = Species -- makes coloured by Species, with block outline of point -- NEED FILL for violin plot to be filled
              
              ## BOX PLOTS
                  #geom_boxplot(fill = "White") + 
                  #geom_boxplot(aes(colour = Species)) +
                  #geom_violin(trim = FALSE) +
                  #stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="crossbar", color = "black") + # "crossbar" or "pointrange"
                  
                  # PRISM version (from ...) -- doesn't quite work, very squished error bards
                  #stat_summary(
                  #  fun.ymax=function(i) mean(i) + qt(0.975, length(i))*sd(i)/length(i), # 0.975
                  #  fun.ymin=function(i) mean(i) - qt(0.975, length(i))*sd(i)/length(i), # 0.975
                  #  geom="errorbar", width=0.5) +
                  
                  # PRISM -- SE (from https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean)
                  stat_summary(
                    fun.ymax=function(i) mean(i) + sd(i)/sqrt(length(i)), # 0.975
                    fun.ymin=function(i) mean(i) - sd(i)/sqrt(length(i)), # 0.975
                    geom="errorbar", width=0.5, size = 1) +  
        
                  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", width = 0.7, size = 0.5) + # add a large point for the mean
              
              ## POINTS
                  #geom_dotplot(aes(fill=Xaxis, colour = Xaxis), binaxis='y', stackdir='center', stackratio = 0.8, dotsize = 1.5) + 
                  #geom_jitter(aes(fill=Xaxis, colour = Xaxis), shape = 16, position = position_jitter(0.2))+
                  geom_point(aes(fill=Xaxis, colour = Xaxis), shape=21, stroke = 0, size = Dot.Size, position=position_jitter(width = 0.1, height = 0)) +
                  #geom_sina(aes(color = Xaxis), size = 2) +
              
              # VARIANCE
                  #stat_summary(fun.y=mean, geom="point", shape=18, size=8, color="Black") + # add a large point for the mean
                  #geom_pointrange() + # need ymin and ymax
                  #geom_errorbar(aes(ymax = Sepal.Length+se, ymax = Sepal.Length-se)) + # SEM -- requires prior calculation of SE
                  #geom_errorbar(aes(ymax = Sepal.Length+sd, ymax = Sepal.Length-sd)) +
            
              # MORE THAN TWO GROUPS: pairwise comparison with overall anova/Kruskal-Wallis result
                    stat_compare_means(comparisons = my_comparisons, method = Pairwise_test)+ #, label.y = max_y_value_p10) + # Add pairwise comparisons p-value # default is "wilcox.test" (non-parametric), can be "t.test" (parametric)
                    stat_compare_means(method = Variance_test, label.y = max_y_value_p40)+ # Add global Anova ("anova") or Kruskal-Wallis ("kruskal.test", default) p-value # an add label.y = 50 to specifiy position
              
                        # MORE THAN TWO GROUPS: compare against reference sample
                        #stat_compare_means(method = "kruskal.test", label.y = 45) +      # Add global p-value
                        #stat_compare_means(label = "p.signif", method = "t.test",ref.group = "0.5", label.y = 40)    
            
              ## SCALES
                  scale_x_discrete(limits=Group_Order) + # use to re-arrange X axis values
                  scale_y_continuous(labels = scales::scientific, limits = c(0, max_y_value_p40)) +
                  
              ## COLOUR CONTROL
              scale_fill_manual(values=Colour_Order_Fill) + # for circle fills
              scale_color_manual(values=Colour_Order_Lines) + # for circle outlines # (values=c("Black", "Red", "Blue"))
              
                        # OTHER OPTIONS
                        #scale_fill_brewer(palette="Dark2") +
                        #scale_color_brewer(palette="Dark2") +
              
              ## VISUALS
                  labs(title=paste0(plotname), x= paste0(X_axis_label), y = paste0(Y_axis_label)) + # colnames(data)[3] would return the name -- use similar for loop -- maybe data$dose
                  #coord_fixed(ratio = 1) + # determines size ratio of the plot -- smaller increases width
                  theme_classic(base_size = 30) + # can be theme_classic(), theme_bw()
              
              ## THEMES
              theme(legend.position = "none", # can be "left" "right" "top" "bottom" "none
                    axis.text.x = element_text(colour="black",size=18,angle=45,hjust=1,vjust=1,face="bold"),
                    axis.text.y = element_text(colour="black",size=18,angle=0,hjust=1,vjust=0,face="bold"),  
                    axis.title.x = element_text(colour="black",size=18,angle=0,hjust=.5,vjust=0,face="bold"),
                    axis.title.y = element_text(colour="black",size=18,angle=90,hjust=.5,vjust=1,face="bold"),
                    plot.title = element_text(lineheight=.8, face="bold", hjust = 0, size = 24), # hjust = 0.5 to centre
                    axis.line = element_line(colour = 'black', size = 1),
                    axis.ticks = element_line(colour = "black", size = 1)
                    )
            
            ### End construction of 'p'
            
            ## View plot
            p
    
            ## Save        
            ggsave(p, filename = paste0(plotname, ".pdf"), width = 5.5, height = 7.5)
            
            }
            
