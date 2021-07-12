#rm(list=ls())
library(ggplot2)
library(grid)

#temp = getwd()

#setwd("S:/Business Analytics/2014/projects/arena.visualization/from syd/")
#setwd('C:/Users/brian.macdonald/Documents/hockey-studies/projects/rink')
#setwd("projects/rink/")
#folder = "S:/Business Analytics/2014/projects/arena.visualization/from syd/"
#data=read.csv("logos/logos.csv",header=TRUE,stringsAsFactors = FALSE)

#p=ggplot(data.frame(x=1:2, y=1:2), aes(x=x, y=y))
p=ggplot()
#print(p)
clearandprint = function(){ ### not used anymore: coord.flip=F. Probably should be used though.
  assign("p", p + 
    #annotation_custom(g, xmin=-42.5, xmax=42.5, ymin=-13, ymax=67) + ## background image
    #xlab(NULL) + ylab(NULL) +
    #ylim(c(106,-106)) + 
    #xlim(c(-47.6, 47.6)) + 
    #coord_flip()+ doesn't seem to work
    theme_bw()
    # theme(panel.border = element_blank())+
    # theme(panel.grid.major.x = element_blank())+
    # theme(panel.grid.major.y = element_blank())+
    # theme(panel.grid.minor.x = element_blank())+
    # theme(panel.grid.minor.y = element_blank())+
    # theme(axis.ticks  = element_blank()) +
    # theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
    # theme(plot.title = element_text(size=36)) +
    # theme(legend.position="none")
    ,envir=.GlobalEnv)
    #print(p)
  }

polygon2 = function(x,y,border="black", lwd=1, col=NA){
  a = eval(substitute(
          expr = {
            aes(x=x,y=y)
          },
          env = list(x=x, y=y)))
  lp = geom_polygon(a,fill=col, linetype="solid", colour=border, size=lwd/2)
  assign("p", p + lp,envir=.GlobalEnv)
  #p
}

segments2 = function(x,y,xend,yend, col=1, lwd=3,lty=1){
  a = eval(substitute(
    expr = {
      aes(x=x,y=y,xend=xend,yend=yend)
    },
    env = list(x=x, y=y,xend=xend, yend=yend)))
  lp = geom_segment(a, size=lwd/2,colour=col, lineend="square", linetype=lty)
  assign("p", p + lp,envir=.GlobalEnv)
}

lines2 = function(x, y, col="black", lwd=stdlwd, lty=1){
  a = eval(substitute(
    expr = {
      aes(x=x,y=y)
    },
    env = list(x=x, y=y)))
  lp = geom_line(a, size=lwd/2, colour=col, lineend="square", linetype=lty)
  assign("p", p + lp,envir=.GlobalEnv)
}

rect2 = function(xleft,ybottom,xright,ytop, col=NA,border="black",lwd=stdlwd){
  polygon2(c(xleft,xleft,xright,xright),c(ytop,ybottom,ybottom,ytop), border=border, col=col,lwd=lwd)
}
################### ACTUAL CODE FOR RINK ######################

library(png)
stdlwd=1.5

full.rink = function (COLOR=TRUE, board.logos=F, ice.logos=F, center.ice.logo=T) {
  red="gray";blue="gray";lightblue="gray70"
  if (COLOR){red="red";blue="blue";lightblue="lightblue"}
  faceoff.circle = function (x,y) {
    theta = seq(0,2*pi,length=300)
    dd <- (5+7/12)/2 #http://www.nhl.com/ice/news.htm?id=733257
    polygon2 (
      x= x + 15*cos(theta),#faceoff outer circle
      y= y + 15*sin(theta),
      lwd=stdlwd,
      border=red)
    polygon2 (
      x=x + 1*cos(theta),#faceoff dot
      y=y + 1*sin(theta),
      col=red,#color of inside faceoff dot
      border=red)#red color outline
    segments2 (
      x=c(x-0.75,x-0.75, x+0.75,x+0.75, x-0.75,x-0.75, x+0.75,x+0.75), #faceoff guides #0.75 = 9 inches
      y=c(y-2,y-2, y-2,y-2, y+2,y+2,y+2,y+2),
      xend=c(x-0.75,x-3.75, x+0.75,x+3.75, x-0.75,x-3.75, x+0.75,x+3.75),
      yend=c(y-6,y-2, y-6,y-2, y+6,y+2,y+6,y+2),
      col=red, lwd=stdlwd)
    
    segments2 (
      x=c(x-15, x-15, x+15, x+15), #hashmarks #vertical points
      y=c(y-dd, y+dd, y-dd, y+dd), #lwd ? may not be exactly 2 inches
      xend=c(x-17, x-17, x+17, x+17),
      yend=c(y-dd, y+dd, y-dd, y+dd),
      col=red, lwd=stdlwd)
  }
  goal.crease.2 = function(flip=FALSE) {
    xseq=c()
    yseq=c()
    theta=c()
    if(flip){ #round part up
      x=0 #center of oval cordinates (x,y)
      y=-84.5
      theta = seq(0,pi,length=300) #top part of oval
      xseq=x+4*cos(theta) #long radius
      yseq=y+1.5*sin(theta) #short radius
      xseq=c(xseq,x-4,x+4) #bottom rectangle points
      yseq=c(yseq,y-4.5,y-4.5) 
    }
    if(!flip){#round part down
      x=0
      y=84.5
      theta = seq(pi,2*pi,length=300)#bottom of oval
      xseq=x+4*cos(theta)
      yseq=y+1.5*sin(theta)
      xseq=c(xseq,x+4,x-4) #top rectangle points 
      yseq=c(yseq,y+4.5,y+4.5)
    }
    polygon2 (xseq,
              yseq,
              lwd=stdlwd,#width of red line
              border=red,
              col=lightblue)
    
    
  }
  
  icelogo = function(img_name, xdim, ydim, xcenter, ycenter){
    img=readPNG(img_name)
    gray=.21*img[,,1]+.71*img[,,2]+.07*img[,,3];gray=gray/max(gray);
    if (!COLOR) {img=gray}
    g=rasterGrob(img,interpolate=TRUE) #image plotting(library(png))
    assign("p",p+ annotation_custom(g,xcenter-xdim,xcenter+xdim,ycenter-ydim,ycenter+ydim),envir=.GlobalEnv)
  }
  
  ## center ice logo
  if(center.ice.logo==T){
  dim = 30/sqrt(2)/2 *1.25 
  icelogo("logos/panther.png", dim, dim, 0, 0)
  }
  
  ## ice logos
  if(ice.logos==T){
  dim = 4.75
  icelogo("logos/lexus.png", dim,2*dim, -28.75, 12.5)
  icelogo("logos/BH.png", dim, 2.2*dim, 28.75, 12.5)
  }
  
  #Board panels next
  boardlogo = function(img_name, panel_num, COLOR=TRUE, BOX=FALSE){
    img=readPNG(img_name)
    
#     if (data$Status = "C"||data$Status="O")
#       COLOR=TRUE
#     if (data$Status ="P")
#       COLOR=FALSE
    
    ratio=dim(img)[2]/dim(img)[1]
    if(panel_num>=1&&panel_num<=16){
      xcenter=45
      ycenter=-6.25-(8-panel_num)*12.5
      pratio=12.5/4
      if(ratio<pratio){
        xdim=4/2
        ydim=xdim*ratio
      }
      if(ratio>=pratio){
        ydim=12.5/2
        xdim=ydim/ratio
      }
    }
    
    if(panel_num>=23&&panel_num<=38){
      xcenter=-45
      ycenter=6.25+(30-panel_num)*12.5
      pratio=12.5/4
      if(ratio<pratio){
        xdim=4/2
        ydim=xdim*ratio
      }
      if(ratio>=pratio){
        ydim=12.5/2
        xdim=ydim/ratio
      }
    }
    
    
    if(panel_num>=17&&panel_num<=22){
      img2=aperm(img,c(2,1,3))
      dim1=dim(img2)[1]
      dim2=dim(img2)[2]
      img = img2[,dim2:1,] 
      ratio=dim(img)[2]/dim(img)[1]
    
      
      xcenter=13.5/2+(19-panel_num)*13.5
      ycenter=102.5
      pratio=4/13.5
      if(ratio<pratio){
        xdim=13.5/2
        ydim=xdim*ratio
      }
      if(ratio>=pratio){
        ydim=4/2
        xdim=ydim/ratio
      }
    }
    
    if(panel_num>=39&&panel_num<=44){
      img2=aperm(img,c(2,1,3))
      dim1=dim(img2)[1]
      dim2=dim(img2)[2]
      img = img2[dim1:1,,]  
      ratio=dim(img)[2]/dim(img)[1]
      
      
      xcenter=-13.5/2-(41-panel_num)*13.5
      ycenter=-102.5
      pratio=4/13.5
      if(ratio<pratio){
        xdim=13.5/2
        ydim=xdim*ratio
      }
      if(ratio>=pratio){
        ydim=4/2
        xdim=ydim/ratio
      }
    } 
    
    
    

    if (!COLOR) {
      gray=.21*img[,,1]+.71*img[,,2]+.07*img[,,3];gray=gray/max(gray)
      img=gray
    }
    g=rasterGrob(img,interpolate=TRUE) #image plotting(library(png))
    assign("p",p+ annotation_custom(g,xcenter-xdim,xcenter+xdim,ycenter-ydim,ycenter+ydim),envir=.GlobalEnv)
    
    #box=polygon(xcenter-xdim,xcenter+xdim,ycenter-ydim,ycenter+ydim)
   # if(BOX == TRUE){polygon2(x=c(xcenter-xdim-0,xcenter-xdim-0,xcenter+xdim+0,xcenter+xdim+0),
    #                         y=c(ycenter-ydim-0,ycenter+ydim+0,ycenter+ydim+0,ycenter-ydim-0)) }
  }
  
  #THiS READS THE BOARD PANELS IN

  if(board.logos==T){  
  for (i in 1:44){ boardlogo(data$Logo[i], i)
    if (data$Status[i]=="C"|data$Status[i]=="O"){COLOR=TRUE;BOX=FALSE}
    if (data$Status[i]=="P"){ COLOR=FALSE;BOX=TRUE}
    boardlogo(data$Logo[i], i,COLOR,BOX)
    }
  }
   #if status is C color is true if status is P color false
  
  
  
  theta = seq(0,2*pi,length=300)
  polygon2 (15*cos(theta), 15*sin(theta), lwd=stdlwd, border=blue)#center circle
  theta2 = seq (-pi/2, pi/2, length=300)
  polygon2 (-42.5 + 10*cos(theta2), 10*sin(theta2), lwd=stdlwd, border=red)#ref circle
  
  rect2(-42.5, 25, 42.5, 26, col=blue, border="white", lwd=.001)#top base/blue line
  rect2(-42.5, -25, 42.5, -26, col=blue, border="white", lwd=.001)#bottom blue line
  
  rect2(-42.5, -0.5, 42.5, 0.5, col=red, border="white", lwd=.001)#center red line
  lines2(c(-42.5,42.5),c(0,0),lty=2,lwd=1, col="white") #dotted white line
  
  goal.line.extreme = 42.5 - 28 + sqrt(28^2 - (28-11)^2)
  
  lines2(goal.line.extreme*c(-1, 1), rep(89,2), col=red,lwd=stdlwd)        #the goal line.
  lines2(goal.line.extreme*c(-1, 1), rep(-89,2), col=red,lwd=stdlwd)        #the goal line.
  
  lines2(c(-3,-3,3,3), c(90,92,92,90)-1, col=1, lwd=stdlwd)    #the goal net.
  lines2(c(-3,-3,3,3), -(c(90,92,92,90)-1), col=1, lwd=stdlwd)    #the goal net.
  goal.crease.2(); goal.crease.2(flip=TRUE)
  ## traps.
  segments2(c(-11, 11, -11, 11), c(89,89,-89,-89),
           c(-14,14,-14,14), c(100,100, -100,-100), col=red, lwd=stdlwd)
  
  faceoff.circle (-22, 69)
  faceoff.circle (22, 69)
  
  faceoff.circle (-22, -69)
  faceoff.circle (22, -69)
  
  faceoff.dot = function (x,y,r=1,col=red) {
    polygon2 (x + r*cos(theta),
             y + r*sin(theta),
             col=col,
             border="white", lwd=.001)
  }
  faceoff.dot (22,20); faceoff.dot (22,-20); faceoff.dot (-22,20); faceoff.dot (-22,-20);
  
  lines2(c(-42.5, #outer edge, top
           -42.5 + 28 - 28*cos(seq(0,pi/2,length=20)),
           42.5 - 28 + 28*cos(seq(pi/2,0,length=20)),
           42.5),
         c(-24,
           72 + 28*sin(seq(0,pi/2,length=20)),
           72 + 28*sin(seq(pi/2,0,length=20)),
           -24),
         col="black", lwd=stdlwd)
  lines2 (c(-42.5, #outer edge, bottom
            -42.5 + 28 - 28*cos(seq(0,pi/2,length=20)),
            42.5 - 28 + 28*cos(seq(pi/2,0,length=20)),
            42.5),
          c(-24,
            -72 - 28*sin(seq(0,pi/2,length=20)),
            -72 - 28*sin(seq(pi/2,0,length=20)),
            -24),
          col="black", lwd=stdlwd)
  faceoff.dot(0,0,.5,blue)
}


full.rink(COLOR=FALSE, board.logos=F, ice.logos=F, center.ice.logo=F)

rink=p
# #######################   EXAMPLES  ###########################
#   polygon2(x=c(-15,-15,15,15),y=c(15,-15,-15,15),border="blue")
# 
#   polygon2(aes(x=c(-3,-3,3,3),y=c(3,-3,-3,3)),border="red")
# 
#   polygon2(aes(x=c(-3,3),y=c(3,3)),border="green")
# 
#    x=0
#    y=0
#    stdlwd=3
#   segments2 (aes(
#     x=c(x-0.75,x-0.75, x+0.75,x+0.75, x-0.75,x-0.75, x+0.75,x+0.75), #faceoff guides #0.75 = 9 inches
#     y=c(y-2,y-2, y-2,y-2, y+2,y+2,y+2,y+2),
#     xend=c(x-0.75,x-3.75, x+0.75,x+3.75, x-0.75,x-3.75, x+0.75,x+3.75),
#     yend=c(y-6,y-2, y-6,y-2, y+6,y+2,y+6,y+2)),
#     col=2, lwd=stdlwd)
##############################################################

## cleanup
## if want to print it it here.  
## Otherwise, if want to draw points on it, leave commented
clearandprint() 

#png("full-rink3.png", height=854, width=1920); print(p); dev.off()
