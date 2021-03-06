# AutoGraph v1.0
# Thomas Ashhurst
# 2018-01-16
# www.github.com/sydneycytometry/autograph


###################################################### 1. INSTALL AND-OR LOAD PACKAGES ###################################################### 

    ### 1.1. Install packages
        if(!require('ggplot2')) {install.packages('ggplot2')}
        if(!require('ggpubr')) {install.packages('ggpubr')}
        if(!require('scales')) {install.packages('scales')}
        if(!require('devtools')) {install.packages('devtools')}
        if(!require('rstudioapi')) {install.packages('rstudioapi')}

        library(ggplot2)
        library(ggpubr)
        library(scales)
        library(devtools)
        library(rstudioapi)

        #if(!require('cowplot')) {install.packages('cowplot')} # not required, development only
        #devtools::install_github("baptiste/egg") # not required, development only
        #library(egg) # not required, development only

    ### 1.2 Working directory
        
        ## Option A: set working directory to the folder containing this script
        dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
        setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
        getwd()
        PrimaryDirectory <- getwd()
        PrimaryDirectory
    
        ## Option B: set working directory manually
        #setwd("/Users/Tom/Desktop/auto_test_2/")
        #PrimaryDirectory <- getwd()
        #PrimaryDirectory
        
        ## Create a list of .csv files
        list.files(path = PrimaryDirectory, ".csv")

###################################################### 2. SETUP DATA ######################################################     
    
    ### 2.1. Setup data
        
        
        ## OPTION A: read .csv file
        data <- read.csv("SumTable_CellsPerTissue.csv")
        
        ## OPTION B: use iris dataset for testing
        #data <- iris # use for TESTING
        
        # Set up as data frame
        data <- as.data.frame(data)
        head(data)
    
        unique(data$Group)
        target <- c("Mock", "WNV_D7_PBS", "WNV_D7_isotype", "WNV_D7_anti-TNF")
        target
            
            #data <- data[match(target, data$Group),]
            #head(data)
            
        if(!require('dplyr')) {install.packages('dplyr')}
        require(dplyr)
        
        data <- data %>% mutate(Group =  factor(Group, levels = target)) %>% arrange(Group)    
        
        # What column defines your groups (grouping on the X axis, dependent variable e.g. infection groups) -- convert this column to a 'factor' and assign to Xaxis
        Xaxis <- data$Group <- as.factor(data$Group) # insert the name of the column after both $'s
        Xaxis
        
        # Assign order and colour of groups
        Group_Order <- target 
        Group_Order[order(Group_Order)] # alphabetical order
        
        Colour_Order_Fill <- c("Black", "red", "red", "#377eb8") # number of distinct columns must be the same as the number of groups -- MUST MATCH ALPHABETICAL ORDER
        Colour_Order_Lines <- c("Black", "Black", "Black", "Black") # "Black" will create an outline of each point. Can change to match the colours on the line above.
        
        # Create string of columns to analyse -- REMOVE the column that represents the groups (the column that defines the X axis)
        ColNames <- names(data)
        ColNames
        
        ColNames <- ColNames[-c(1:3)]
        ColNames
        
        # Labels for X and Y axis
        X_axis_label <- " " # Leave empty for no axis label
        Y_axis_label <- "Cells per femur" # Y axis labl
    
        Dot.Size <- 5   # 5 = mid size, 7 = large dot
        
    ### 2.2. Setup statistics
        
        # COMING SOON: run gaussian assessment -- help decide test
        
        # Comparing overall variance (ANOVA, Kruskal-Wallis etc)
            # Run_variance_assessment <- 1 # Coming soon
        Variance_test <- "kruskal.test"   # can be "kruskal.test" (non-parametric) or "anova" (parametric)
        
        # Pair-wise comparisons (T-test, Willcox test etc)
            # Run_pairwise_assessment <- 1  # Coming soon
        Pairwise_test <- "wilcox.test"  # default is "wilcox.test" (non-parametric), can be "t.test" (parametric)
        
        # IF performing pair-wise comparisons, specificy which groups you want to compare statistically -- not all columns have to be compared
        as.matrix(unique(Xaxis))    
        my_comparisons <- list(c("Mock", "WNV_D7_isotype"), # comparison 1
                               c("WNV_D7_isotype", "WNV_D7_PBS"), # comparison 2
                               c("WNV_D7_isotype", "WNV_D7_anti-TNF")) # comparison 3 etc
        
        
        
###################################################### 3. Plotting Loop (END USER INPUT) ######################################################       

        
        
    ### 3.1. Set output directory
        setwd(PrimaryDirectory)
        dir.create("Output_AutoGraph", showWarnings = FALSE)
        setwd("Output_AutoGraph")
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
            p <- ggplot(data, aes(x=Xaxis, y=data[[a]]))+ #, fill=Species)) + # can use fill = Species -- makes coloured by Species, with block outline of point -- NEED FILL for violin plot to be filled

              ## SCALES
                  #scale_x_discrete(limits=Group_Order) + # use to re-arrange X axis values
              scale_y_continuous(labels = scales::scientific, limits = c(0, max_y_value_p40)) +
              
              
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
            
