##############################################
#### Individual tree detection using lidR ####
##############################################

# import lidar package
library(lidR)
library(terra)

# set work directory
dir <- setwd("C:\\Users\\wendiz3.stu\\OneDrive - UBC\\Documents\\GEM 521\\Lab 2\\L2_data\\Data")

#### plot extraction ####

  # read plot csv
  plots <- read.csv("C:\\Users\\wendiz3.stu\\OneDrive - UBC\\Documents\\GEM 521\\Lab 5\\Data\\Lab5_Plots.csv")
  
  # read multiple .las files into LAScatalog object
  norm_cat_mkrf <- readLAScatalog("Normalized")
  crs(norm_cat_mkrf)<- "EPSG:3005"
  
  #Extract single circular plot (diameter 154 m)
  plot1 <- clip_circle(norm_cat_mkrf,
                       531704.8, # X
                       5462033, # Y
                       77) # radius
  
  plot2 <- clip_circle(norm_cat_mkrf, 530050.0, 5461597, 77)
  plot3 <- clip_circle(norm_cat_mkrf, 532811.0, 5463481, 77)
  plot4 <- clip_circle(norm_cat_mkrf, 533849.0, 5464680, 77)
  
  #las_check(plot1), the return says normalization: maybe
  norm_cat_mkrf$Max.Z #is another way of checking if the Z values (reference) is indeed added to the data
  
#### li2012 ####
  
  plot1_li2012 <- segment_trees(plot1,
                                    li2012(dt1 = 1.5, dt2 = 2, R = 2, Zu = 15, hmin = 2, speed_up = 10))
  plot2_li2012 <- segment_trees(plot2, li2012())
  plot3_li2012 <- segment_trees(plot3, li2012())
  plot4_li2012 <- segment_trees(plot4, li2012())
  
  plot(plot1_li2012, color = "treeID")
  plot(plot2_li2012, color = "treeID")
  plot(plot3_li2012, color = "treeID")
  plot(plot4_li2012, color = "treeID")
  
#### dalponte2016 ####
  
  # create CHM for the four plots
  chm_plot1 <- rasterize_canopy(plot1, 
                                res = 0.5, 
                                pitfree(thresholds = c(0, 10, 20, 30),
                                        max_edge = c(0,1),
                                        subcircle = 0.2),
                                highest = TRUE)
  chm_plot2 <- rasterize_canopy(plot2, 0.5, pitfree(thresholds = c(0, 10, 20, 30),
                                                    max_edge = c(0,1),
                                                    subcircle = 0.2),
                                highest = TRUE)
  chm_plot3 <- rasterize_canopy(plot3, 0.5, pitfree(thresholds = c(0, 10, 20, 30),
                                                    max_edge = c(0,1),
                                                    subcircle = 0.2),
                                highest = TRUE)
  chm_plot4 <- rasterize_canopy(plot4, 0.5, pitfree(thresholds = c(0, 10, 20, 30),
                                                    max_edge = c(0,1),
                                                    subcircle = 0.2),
                                highest = TRUE)
  
  plot(chm_plot1)
  plot(ttops_plot1, add = TRUE)
  plot(chm_plot2)
  plot(chm_plot3)
  plot(chm_plot4)
  
  # segmenting trees for the four plots
    
    # find tree tops
    ttops_plot1 <- find_trees(plot1, lmf(ws = 5, hmin = 2, shape = "circular"))
    ttops_plot2 <- find_trees(plot2, lmf(ws = 5, hmin = 2, shape = "circular"))
    ttops_plot3 <- find_trees(plot3, lmf(ws = 5, hmin = 2, shape = "circular"))
    ttops_plot4 <- find_trees(plot4, lmf(ws = 5, hmin = 2, shape = "circular"))
    
  # run dalponte2016   
  
  plot1_dalponte2016 <- segment_trees(plot1, 
                                      algorithm = dalponte2016(chm_plot1,
                                                               ttops_plot1,
                                                               th_tree = 2,
                                                               th_seed = 0.45,
                                                               th_cr = 0.55,
                                                               max_cr = 10))
  plot2_dalponte2016 <- segment_trees(plot2, dalponte2016(chm_plot2, ttops_plot2))
  plot3_dalponte2016 <- segment_trees(plot3, dalponte2016(chm_plot3, ttops_plot3))
  plot4_dalponte2016 <- segment_trees(plot4, dalponte2016(chm_plot4, ttops_plot4))
  
  plot(plot1_dalponte2016, color = "treeID")
  plot(plot2_dalponte2016, color = "treeID")
  plot(plot3_dalponte2016, color = "treeID")
  plot(plot4_dalponte2016, color = "treeID")

# plot plot 2 in 3D
  plot_dtm3d(plot2_dalponte2016)
  
  