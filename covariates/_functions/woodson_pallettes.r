#############################################################################################################
# Purpose: Create beautiful color ramps for cartography
##############################################################################################################

### Libraries
library(rje) #miscellaneous random functions-- this has the CubeHelix function in it
library(ggplot2)
library(ggthemes)
library(grid) # You need this for the vplayout function

##############################################
## Defining how the image screen is split up
##############################################
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# This function allows you to split the graphics space into rows x and columns y

##############################################
## Plotting a color pallette
##############################################

  # Adapted from:
  #https://learnr.wordpress.com/2009/04/15/ggplot2-qualitative-colour-palettes/
  plot_colors<-function(color_list,color_list_name=NULL){
      # color_list: list of colors
      # color_list_name: The title of the plot
      # requires: data.table,ggplot2,ggthemes
    
    data_table<-data.table(data.frame(x=letters[1:length(unique(color_list))]))
    data_table[, colors:=unique(color_list)]
    data_table[, bar_height:=1]
    p<-ggplot(data_table,aes(x=colors))+geom_bar(aes(fill=colors, stat="identity"))+theme_tufte()+labs(x=paste(unique(color_list), collapse=', '))+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
    p<-p+ scale_fill_manual(values=color_list) + ggtitle(color_list_name)
      return(p)
  }

################################################
## Generating a cubehelix intensity pallette
#################################################
  intensity_pallette<-function(start,r,gamma,hue_list=c(.5,1,3)){
        # This function creates a color pallette from a cubehelix that increases in Intensity/hue as it gets darker.
        # Start: what color you want to start with
        # hue: how bright you want the colors to be (1: normal, higher: brighter, lower: more demure)
        # R: How many "rotations" through color space you want it to do (how complicated do you want your color ramp to be?)
        # gamma: How light or dark you want it to be (1: normal, higher:darker, lower: lighter)
        # requires: library(rje) for the cubehelix function. 
    #low intensity pallette
      mellow<-cubeHelix(11, start = start, r = r, hue = hue_list[1], gamma = gamma)
    #middling intensity
      mid<-cubeHelix(11, start = start, r = r, hue = hue_list[2], gamma = gamma) 
    #strong intensity
      strong<-cubeHelix(11, start = start, r = r, hue = hue_list[3], gamma = gamma) #hues higher than 2 get weird
    # binding together the colors such that they are flipped (light to dark)
      colors<-(rbind(rev(mellow[2:8]),rev(mid[2:8]),rev(strong[2:8])))
      mellow_to_strong<-append(colors[1,1:2],colors[2,3:5]) #appending together the mellow and middling colors
      mellow_to_strong<-append(mellow_to_strong,colors[3,6:7]) # appending together the strong colors onto the mellow and middling
      return(mellow_to_strong)}

################################################
## Creating a function (woodson_pallettes)  
#################################################
#http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/cubewedges.html
woodson_pallettes<-function(color=NULL){
  woodson_pallettes<-list()
  woodson_pallettes[["easter_to_earth"]]<-c("#000000", "#5b2c44", "#826737", "#67af6d", "#90c6de", "#ffff4c")
  woodson_pallettes[["cool_toned"]]<-c("#000000", "#3E350C", "#9D4B60" , "#AB86D0" ,"#97E4DF","#ffff4c")

  ## A bunch of different, single-rotation color helix patterns:
  woodson_pallettes[["black_to_light_1"]]<-cubeHelix(11, start = 0, r = -1, hue = 1, gamma = 1)[1:10]
  woodson_pallettes[["black_to_light_2"]]<-cubeHelix(11, start = .5, r = -1, hue = 1, gamma = 1)[1:10]
  woodson_pallettes[["black_to_light_3"]]<-cubeHelix(11, start = 1, r = -1, hue = 1, gamma = 1)[1:10]
  woodson_pallettes[["black_to_light_4"]]<-cubeHelix(11, start = 1.5, r = -1, hue = 1, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_5"]]<-cubeHelix(11, start = 2, r = -1, hue = 1, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_6"]]<-cubeHelix(11, start = 0, r = 1, hue = 1.5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_7"]]<-cubeHelix(11, start = .5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_8"]]<-cubeHelix(11, start = 1, r = 1, hue = 1.5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_9"]]<-cubeHelix(11, start = 1.5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_10"]]<-cubeHelix(11, start = 2, r = 1, hue = 1.5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_11"]]<-cubeHelix(11, start = 0, r = 1, hue = .5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_12"]]<-cubeHelix(11, start = .5, r = 1, hue = .5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_13"]]<-cubeHelix(11, start = 1, r = 1, hue = .5, gamma = 1)[1:10]  
  woodson_pallettes[["black_to_light_14"]]<-cubeHelix(11, start = 1.5, r = 1, hue = .5, gamma = 1)[1:10]
  woodson_pallettes[["black_to_light_15"]]<-cubeHelix(11, start = 2, r = 1, hue = .5, gamma = 1)[1:10]


##############################
# Half-Rotation Colors
##############################
  woodson_pallettes[["purple_to_sea_green"]]<-cubeHelix(11, start = 0.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["thanksgiving"]]<-cubeHelix(11, start = 0.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["purple_to_lavender"]]<-cubeHelix(11, start = 1, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["brown_to_sea_green"]]<-cubeHelix(11, start = 1, r = .5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["brown_to_pink"]]<-cubeHelix(11, start = 1.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["green_to_lavender"]]<-cubeHelix(11, start = 1.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["green_to_salmon"]]<-cubeHelix(11, start = 2, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["sea_green_to_pink"]]<-cubeHelix(11, start = 2, r = .5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  
  ##############  .8 rotation colors
  woodson_pallettes[["brown_green_blue_pink"]]<-cubeHelix(11, start = 1, r = .8, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["red_green_blue"]]<-cubeHelix(11, start = .5, r = .8, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["purple_red_green"]]<-cubeHelix(11, start = 0, r = .8, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["sea_green_purple_tan"]]<-cubeHelix(11, start = 2, r = .8, hue = 1.5, gamma = 1)[1:10] 
  woodson_pallettes[["classy_earth"]]<-cubeHelix(11, start = .5, r = .8, hue = .5, gamma = 1)[1:10]
  woodson_pallettes[["stormy_seas"]]<-cubeHelix(11, start = 1, r = .8, hue = .5, gamma = 1)[1:10]
    
########################
# Diverging Schemes
########################
  
    ### Diverging from black
  woodson_pallettes[["pink_blue_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  woodson_pallettes[["orange_blue_diverging_from_black"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[1:10])
  woodson_pallettes[["green_purple_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[1:10])
  woodson_pallettes[["tan_green_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
    ### Diverging from white
  woodson_pallettes[["pink_blue_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  woodson_pallettes[["purple_blue_diverging_from_white"]]<-append(cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  woodson_pallettes[["tan_green_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
    ## Diverging from Colors 
  woodson_pallettes[["green_pink_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = 3, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 3, r = .4, hue = 1.75, gamma = 1)[3:10]))
  woodson_pallettes[["orange_blue_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[3:10]))
  woodson_pallettes[["tan_blue_multi_diverging_from_green"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 2, r = .4, hue = 1.75, gamma = 1)[3:10]))

##############################
# Making Intensity Pallettes
##############################
  ## Testing out some different color pallettes  
  woodson_pallettes[["tan_to_red_intensity"]]<-intensity_pallette(start=0.5,r=.5,gamma=1)
  woodson_pallettes[["lavender_to_deep_green_intensity"]]<-intensity_pallette(start=1.75,r=.5,gamma=1)
  woodson_pallettes[["pink_to_purple_intensity"]]<-intensity_pallette(start=3,r=.5,gamma=1)
  woodson_pallettes[["mild_green_to_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.5,gamma=1)
  woodson_pallettes[["lavender_green_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.75,gamma=1)
  woodson_pallettes[["sea_green_to_purple_intensity"]]<-intensity_pallette(start=.5,r=-.5,gamma=1)
  woodson_pallettes[["skin_to_purple_intensity"]]<-intensity_pallette(start=.2,r=.4,gamma=1)
  woodson_pallettes[["sea_green_to_blue_intensity"]]<-intensity_pallette(start=3,r=-.4,gamma=1)
  
  ### Diverging Pallettes Based on Intensity Pallettes
  woodson_pallettes[["purple_to_green_diverging_intensity"]]<-append(rev(intensity_pallette(start=.2,r=.4,gamma=1)), intensity_pallette(start=1.75,r=.5,gamma=1))
  woodson_pallettes[["blue_to_red_intensity"]]<-append(rev(intensity_pallette(start=.5,r=-.5,gamma=1)), intensity_pallette(start=0.5,r=.5,gamma=1))

if (length(color)>0){return(woodson_pallettes[[color]])}else{return(woodson_pallettes)}
}

################ 
# Writing function to view plots
view_woodson_pallettes<-function(color=NULL){
  
  if (length(color)<1){
  index<-1
  for (color in names(woodson_pallettes())){
    if (index==1){grid.newpage(); pushViewport(viewport(layout = grid.layout(5, 15)))}
    if (index<=5){print((plot_colors(woodson_pallettes(color),color_list_name=color)), vp = vplayout(index, 1:15))}
    index <- index+1
    if (index>5){index<-1}
  }}else{print((plot_colors(woodson_pallettes(color),color_list_name=color)))}
  } # Closing function

