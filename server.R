library(shiny)

#########################################
#########################################
#########################################
#########################################
# Define server logic



shinyServer(function(input, output) {
  
  durdist <- function(){
    ### Tow distance / duration
    vol <- input$vol
    spd <- input$spd
    ife <- input$ife / 100
    diam <- input$diam
    A <- (pi*((diam/2)^2))
    
    # Equation: vol = A * ife * dur * spd
    dur = vol / (A*ife*spd)
    dist = spd * dur
    dur = dur / 60  
    
    par(mar=c(4,4,2,1),mfrow=c(1,1))
    plot(dur~dist,ann=FALSE,axes=FALSE,xlim=c(0,1000),ylim=c(0,20),cex=2,col="white")
    abline(h=dur,v=dist,col="light grey")
    par(new=TRUE)
    plot(dur~dist,ann=FALSE,axes=FALSE,xlim=c(0,1000),ylim=c(0,20),cex=2)
    axis(1)
    axis(2,las=2)
    title(xlab="Tow distance (m)",ylab="Tow Duration (m)")
    text(y=19,x=400,paste(format(dist,digits=2)," m. for ",format(dur,digits=2)," min.",sep=""))
  }
  
  minoar <- function(){
    area <- input$area
    V <- input$vol
    spd <- input$spd
    ife <- input$ife / 100
    diam <- input$diam
    A <- (pi*((diam/2)^2)) 
    
    moar <- NA
    moarwarning <- "None."
    
    if(area == "Green waters" | area == "Both"){moar <- 10^(0.38 *(log10(V)-log10(A))-0.17)}
    if(area == "Blue waters"){moar <- 10^(0.37 *(log10(V)-log10(A))-0.49)}
    
    if(is.na(moar)==TRUE){
      moarwarning <- "Error: something is wrong and an NA was returned"
    }else{
      if(moar < 3){
        moar <- 3
        moarwarning <- "The minimum OAR for any net design is 3 (Tranter & Smith 1968)"
      }
      if(input$mesh > 300 & moar < 5){
        moar <- 5
        moarwarning <- "The minimum OAR for nets with mesh greater than 300u is 5:1 (Tranter & Smith 1968)"
      }
      if(input$mesh < 300 & moar < 9){
        moar <- 9
        moarwarning <- "The minimum OAR for nets with mesh smaller than 300u design is 9:1 (Tranter & Smith 1968)"
      }
    }
    
    sr1 <- paste("<b>Minimum OAR is ",format(moar,digits=3)," : 1</b>",sep="")
    sr2 <- paste("<b>Note:</b> ",moarwarning,sep="")
    HTML(paste(sr1,sr2,sep="<br/><br/>"))
  }
  
  por <- function(){
    # Determine B based on mesh size
    mesh <- input$mesh
    #We can get porosity values for different Nitex mesh sizes:
    #From http://www.dynamicaqua.com/nitex.html
    nitexmesh<-c(60,64,80,100,105,120,125,140,150,160,180,200,224,240,250,280,300,310,335,400,500,600,700,750,1000)
    nitexB<-   c(35,45,37,32, 40, 34, 41, 43, 50, 44, 43.5,45, 46, 44, 50, 45,50.5,45, 46, 38, 47, 57, 53, 52,56.5)
    nitexseq<-seq(50,1000,by=50)
    # From wildlife supply co, www.wildco.com
    wcmesh<-c(53,63,80,100,118,153,163,210,243,300,333,363,425,500,560,600,750,1000)
    wcB<-c(30,30,29,32,42,38,38,40,44,51,46,48,46,47,49,51,52,57)
    #wcprice<-c(62,59,49,45,39,39,35,32,32,32,32,42,32,32,32,31,31,31)
    
    mesh1 <- cbind(nitexmesh,nitexB)
    mesh2 <- cbind(wcmesh,wcB)
    meshdf <- data.frame(rbind(mesh1,mesh2))
    names(meshdf)[1] <- "mesh"
    names(meshdf)[2] <- "B"
    
    um <- unique(meshdf$mesh)
    wmesh <- vector()
    minB <- vector()
    for(i in 1:length(um)){
      thismesh <- subset(meshdf,meshdf$mesh==um[i])
      wmesh[i] <- um[i]
      minB[i] <- min(thismesh[,2])
    }
    meshfinal <- data.frame(wmesh,minB)
    
    imesh <- which.min(abs(meshfinal$wmesh-mesh))
    B <- min(meshfinal$minB[imesh])
    
    
    par(mar=c(4,4,2,1),mfrow=c(1,1))
    plot(x=nitexmesh,y=nitexB, cex=.25, lwd=2, xlim=c(50,1000), ylim=c(28,60), ann=FALSE, axes=FALSE, col="darkblue")
    #abline(v=nitexseq, h=c(28,seq(30,60,by=3)), col="lightgray")
    par(new=T)
    plot(x=nitexmesh,y=nitexB, cex=.25, type="o", lwd=1, xlim=c(50,1000), ylim=c(28,60), ann=FALSE, axes=FALSE, col="cadet blue")
    par(new=T)
    plot(x=wcmesh, y=wcB, cex=.5, type="o", lwd=1, xlim=c(50,1000), ylim=c(28,60), ann=FALSE, axes=FALSE, col="forestgreen")
    points(mesh,B,cex=2,col="black",pch=1)
    axis(1,cex.axis=1.1, at=nitexseq, labels=nitexseq)
    axis(2,cex.axis=1.1, at=c("",seq(30,60,by=5)), labels=c(28,seq(30,60,by=5)), las=2)
    title(xlab="Mesh Size (microns)", ylab="Porosity (%)", cex.lab=1.1)
    legend(500,40,c("Dynamic Aqua", "Wildco"),lwd=1.2,col=c("cadet blue","forestgreen"), cex=.75,bg="white")
    text(300,59,paste("Min. Porosity = ",B,"%",sep=""))
  }
  
  
  mainplot <- function(){
    # Givens
    diam <- input$diam
    R <- diam/2
    theta <- 81.33
    rad<- theta/180*pi
    A <- pi*(R^2)
    r <- .075/2 # the radius of the WP-2 codend, T&S 1968
    Lcone <- tan(rad)*(R-r)  # solve for the length of the conical section
    s = sqrt((Lcone)^2+(R-r)^2)  # Compute side length    
    SAcone <- pi*(R+r)*s
    
    # Porosity
    # Determine B based on mesh size
    # Determine B based on mesh size
    mesh <- input$mesh
    #We can get porosity values for different Nitex mesh sizes:
    #From http://www.dynamicaqua.com/nitex.html
    nitexmesh<-c(60,64,80,100,105,120,125,140,150,160,180,200,224,240,250,280,300,310,335,400,500,600,700,750,1000)
    nitexB<-   c(35,45,37,32, 40, 34, 41, 43, 50, 44, 43.5,45, 46, 44, 50, 45,50.5,45, 46, 38, 47, 57, 53, 52,56.5)
    nitexseq<-seq(50,1000,by=50)
    # From wildlife supply co, www.wildco.com
    wcmesh<-c(53,63,80,100,118,153,163,210,243,300,333,363,425,500,560,600,750,1000)
    wcB<-c(30,30,29,32,42,38,38,40,44,51,46,48,46,47,49,51,52,57)
    #wcprice<-c(62,59,49,45,39,39,35,32,32,32,32,42,32,32,32,31,31,31)
    
    mesh1 <- cbind(nitexmesh,nitexB)
    mesh2 <- cbind(wcmesh,wcB)
    meshdf <- data.frame(rbind(mesh1,mesh2))
    names(meshdf)[1] <- "mesh"
    names(meshdf)[2] <- "B"
    
    um <- unique(meshdf$mesh)
    wmesh <- vector()
    minB <- vector()
    for(i in 1:length(um)){
      thismesh <- subset(meshdf,meshdf$mesh==um[i])
      wmesh[i] <- um[i]
      minB[i] <- min(thismesh[,2])
    }
    meshfinal <- data.frame(wmesh,minB)
    
    imesh <- which.min(abs(meshfinal$wmesh-mesh))
    B <- min(meshfinal$minB[imesh])
    
    
    #Determine MOAR
    area <- input$area
    V <- input$vol
    diam <- input$diam
    A <- (pi*((diam/2)^2)) 
    
    moar <- NA
    
    if(area == "Green waters" | area == "Both"){moar <- 10^(0.38 *(log10(V)-log10(A))-0.17)}
    if(area == "Blue waters"){moar <- 10^(0.37 *(log10(V)-log10(A))-0.49)}
    
    if(is.na(moar)!=TRUE){
      if(moar < 3){
        moar <- 3
      }
      if(input$mesh > 300 & moar < 5){
        moar <- 5
      }
      if(input$mesh < 300 & moar < 9){
        moar <- 9
      }
    }  
    
    # Variables 
    # X
    OAR <- seq(3,13,length=100) # X = OAR
    
    # Y
    Lcyl <- (OAR*(A/(B/100))-SAcone)/(2*pi*R)  
    
    L <- Lcone + Lcyl  # Add to Lcyl for total length of net
    
    L[L>10] <- NA
    L[L<Lcone] <- NA
    
    if(is.na(moar)==FALSE){
      moarmatch <- which.min(abs(OAR-moar))
      moarL <- L[moarmatch]
    }else{
      moarL <- 100
    }
    
    mLcyl <- (1/(2*pi*R))*(moar*(A/(B/100))-SAcone)  
    #mLcyl[mLcyl<0] <- 0
    
    #mLcyl <- moarL-Lcone
    #tL <- mLcyl + Lcyl
    
    par(mar=c(4,4,0,.25))
    plot(y=L,x=OAR,xlim=c(3,13),ylim=c(0,10),type="l",lwd=2.5,col="white",axes=FALSE,ann=FALSE)
    abline(v=seq(3,13,by=1),h=seq(0,10,by=1),lwd=.5,col="light grey")
    points(moar,moarL,col="black",pch="|",cex=4)
    text(moar+1,moarL-1,paste("Min. length is ",format(moarL,digits=3),"m.",sep=""),cex=1.25,col="black")
    par(new=TRUE)
    plot(y=L,x=OAR,xlim=c(3,13),ylim=c(0,10),type="l",lwd=3,col="firebrick",axes=FALSE,ann=FALSE)
    axis(1,cex.axis=1.5,cex.lab=1.5,at=seq(3,13,by=1),labels=seq(3,13,by=1))
    axis(2,las=2,cex.axis=1.5,at=seq(0,10,by=2),labels=seq(0,10,by=2))
    title(ylab="Net length (m)",xlab="OAR",cex.lab=1.5,)
    #text(11,1.5,paste("Total length: ",format(moarL,digits=3)," m",sep=""))
    text(11,1,paste("Length of conical: ",format(Lcone,digits=3)," m",sep=""))
    text(11,.5,paste("Length of cylinder: ",format(mLcyl,digits=3)," m",sep=""))
  }
  
  
  htmlout <- function(){
    ab01 <- paste("For a complete overview of the models and theory behind this website, see...",sep="")
    ab02 <- paste("<strong>Keen, EM. 2015. Net savvy: a practical design to plankton sampler design. NOAA-TM-SWFSC-####. 31 pp. </strong>",sep="")
    ab01a <- paste("<br/>...which draws primarily upon four classic papers in net design:",sep="")
    ab02a <- paste("1) Tranter, D.J., P.E. Smith. 1968. Filtration Performance. In Monographs on oceanographic methodology 2: Zooplankton Sampling, UNESCO, Switzerland.",sep="")
    ab02b <- paste("2) Harris, R.P., P.H. Wiebe, J. Lenz, H.R. Skjoldal, M. Huntley, eds. 2000. ICES Zooplankton Methodology Manual. Academic Press: London. (esp. Chapters 3 and 4).",sep="")
    ab02c <- paste("3) de Bernardi, Riccardo de. 1984. Methods for the estimation of zooplankton abundance. In JA Drowning & FH Rigler (Eds), A Manual on Methods for the Assessment of Secondary Productivity in Fresh Waters. IBP Handbook 17. 2nd ed. Blackwell, Oxford. Pp. 59-86.",sep="")
    ab02d <- paste("4) Ohman, M. 2013. Zooplankton. Crustacean field collection and preservation techniques. DeGrave, Sammy, and Joel Martin, eds. Oxford University Press. ",sep="")
    
    ab03 <- paste("<br/><strong>Starting Points </strong>",sep="")
    ab04 <- paste("To perform well and consistently, a good net must achieve the following:",sep="")
    ab05 <- paste("    1. Sample for a target duration before clogging deteriorates filtration efficiency and the size selectivity of the mesh.",sep="")
    ab06 <- paste("    2. Retain the organisms targeted by the study's objectives, minimizing their ability to avoid the net and extrude through the mesh.",sep="")
    ab07 <- paste("    3. Filter a sufficient volume of water over sufficient depths to provide statistically robust results.",sep="")
    ab08 <- paste("    4. Be sustainably operable given the finite energies of the crew and resources at disposal.",sep="")
    ab09 <- paste("<br/>    The imperative upon which all other criteria hinge, and the concern that will organize and motivate the decision tree outlined below, is the need to maximize and maintain a net's filtration efficiency (1). In the end, however, if a net suffers from a poor filtration efficiency to begin with and clogs easily, it will fail at both capturing target organisms (2) and filtering sufficient volumes of water at a sustained efficacy (3). The resources devoted to the study (4) would therefore be wasted.",sep="")
    ab10 <- paste("<br/>    Filtration efficiency is the percent of water a net passes through that is actually sampled. Net performance can be discussed in terms of both initial and sustained filtration efficiency (IFE and SFE, respectively) . In addition to diminishing the volume sampled, clogging over the course of a tow can cause a net to become increasingly size selective, which introduces a host of biases to results. Filtration efficiency can also affect rates of net avoidance and escapement (Tranter and Smith 1968).",sep="")
    ab11 <- paste("<br/><strong>The Design Process</strong>",sep="")
    ab12 <- paste("    The design process begins by (1) rigorously answering basic questions about study objectives and target organisms, the study area, and the resources at hand. These answers will determine (2) the mesh size required (and its associated porosity) and (3) a range of viable mouth diameters. Finally, to determine overall net length, (4) a target tow volume must be set, which (5) informs the minimum OAR permissible given mesh size, mouth diameter, and the qualities of the study area. 6) The one variable that remains is the overall net length, which can now be manipulated to achieve the minimum OAR. A case study is used to demonstrate the process in practice.",sep="")
    ab13 <- paste("<br/><strong>Details</strong>",sep="")
    ab14 <- paste("<br/><strong>Initial Filtration Efficiency (IFE)</strong>",sep="")
    ab15 <- paste("    The general shape of a net, the speed at which it is towed, and the care it receives will contribute to its initial (inherent) filtration efficiency (IFE).",sep="")
    ab16 <- paste("<br/>    Typical IFE values for nets include:",sep="")
    ab17 <- paste("Simple conical net: 85%")
    ab17a <- "WP2 cyl-cone net: 100%"
    ab17b <- "Reducing cone net: 110%"
    ab17c <-"Non-porous encasements: 60-70%"
    ab18 <- paste("<br/><strong>Open-Area Ratio (OAR) </strong>",sep="")
    ab19 <- paste("    A net can be designed to sustain its initial filtration efficiency for a target tow volume by strategically selecting the netÕs mesh size, mouth diameter and overall length. Sustained filtration efficiency is best indicated by the Open-Area Ratio (OAR), the ratio of effective filtering area to mouth area.",sep="")
    ab20 <- paste("<br/>    The OAR of a net is a compromise between porosity, filtering area, and mouth area Ð which are products of a net's mesh size, diameter, and overall length Ð all of which are constrained by the organisms of interest, target volume, and resources. As such, the OAR represents a game of trade-offs.",sep="")
    ab21 <- paste("<br/><strong>Mesh Porosity</strong>",sep="")
    ab22 <- paste("    The width of holes in mesh, or porosity, influence SFE in two ways. First, fine gauze will clog more readily than coarse gauze merely because it catches more particles. In order to compensate for the inherently high clogging rate of fine mesh nets, OAR must dramatically increase.",sep="")
    ab23 <- paste("<br/>    For mesh sizes greater than 300u, Tranter and Smith (1968) recommend an overall OAR of 5 or higher, with an OAR of 3 in the conical portion and an OAR of 2 in the cylinder. They warn that any mesh smaller than that would require an OAR of 9 or higher, with 3 in the cone and 6 in the cylinder. However, in the same monograph, Working Party 2 proposed a standardized net schematic with 200? mesh that had an OAR of only 6:1, 3 in the cylinder and 3 in the cone (Tranter and Fraser 1968). To err on the conservative side (sensu Ohman 2013), we will hold minimum OAR at 9:1 for mesh apertures less than 300u, although such arbitrary thresholds should be taken only as guidelines.",sep="")
    ab24 <- paste("<br/>    Second, mesh size determines a netÕs porosity, which is a term in the equation for OAR. While one would expect porosity to decrease linearly with shrinking mesh size, different widths of Nylon monofilament are used in weaves for various mesh sizes, such that porosity levels rise and fall abruptly as mesh size increases. Since mesh of the same aperture can be available at different porosities and varies among distributors, it behooves investigators to seek out the most porous mesh available. Doing so may allow a net to be shorter. While high-porosity material may not be as supple as thick-filament (low porosity) mesh, weave strength is not of great concern for small-scale studies with low tow speeds.",sep="")
    ab25 <- paste("<br/>    The porosities used here are based on those from Dynamic Aqua Supply and Wildlife Supply Company.",sep="")
    ab26 <- paste("<br/><strong>Net Model</strong>",sep="")
    ab27 <- paste("    The net design model used here is based on the standard cylindrical-conical (cyl-cone) design advocated by UNESCO Working Party no. 2 (WP-2). The WP-2 standard is intended for small-scale studies of plankton 200u to 10 mm in length within the upper 200m of ocean (Tranter &amp; Fraser 1968, schematic on p. 155). However, the principles and process invoked here should be of use in a variety of applications.",sep="")
    ab28 <- paste("",sep="")
    ab29 <- paste("",sep="")
    ab30 <- paste("",sep="")
    ab31 <- paste("",sep="")
    ab32 <- paste("",sep="")
    
    
    HTML(paste(ab01,ab02,ab01a,ab02a,ab02b,ab02c,ab02d,ab03,ab04,ab05,ab06,ab07,ab08,ab09,ab10,ab11,ab12,ab13,
               ab14,ab15,ab16,ab17,ab17a,ab17b,ab17c,ab18,ab19,ab20,ab21,ab22,ab23,ab24,ab25,ab26,ab27,sep="<br/>"))
  }
  
  
  
  output$durdist <- renderPlot({
    durdist()},height=200)
  
  output$por <- renderPlot({
    por()},height=200)
  
  output$moar <- renderUI({
    minoar()})
  
  output$main_plot <- renderPlot({
    layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
    por()
    durdist()
    mainplot()
  })
  
  
  output$about <- renderUI({htmlout()})
  
  output$netpic <- renderImage({return(list(src="net.jpg",filetype="image/jpeg",alt="This is the net model"))},deleteFile=FALSE)

  output$collab <- renderImage({return(list(src="collaborators.jpg",filetype="image/jpeg"))},deleteFile=FALSE)


})