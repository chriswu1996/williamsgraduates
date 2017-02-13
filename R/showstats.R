#' @title Show Statistics
#' @description Uses the data from the read function and performs appropriate analysis and generates graphics associated with the gender distribution.
#' @param type The kind of analysis to be done. The possible options are \code{graduates}, \code{latin honors}, \code{department honors}, \code{latin honors detail} and \code{department honors detail}. \code{graduates} displays the timeplot of the gender distribution of the graduating population. \code{latin honors} displays the timeplot of the gender distribution of the population that has latin honors. \code{latin honors detail} splits the population that has latin honors into three: Summa Cum Laude, Magna Cum Laude and Cum Laude, and displays the timeplot of the gender distribution. \code{department honors} displays the timeplot of the gender distribution of the population that has department honors. \code{department honors detail} splits the population that has department honors to two: honors and highest honors, and displays the timeplot of the gender distribution.
#' @return Results from the kind of analysis specified by \code{type}.
#' @usage showstats(type)
#' @import ggplot2
#' @import stringr
#' @export
showstats <- function(type){

  # Load all data
  data( graduates2000 )
  data( graduates2001 )
  data( graduates2002 )
  data( graduates2003 )
  data( graduates2004 )
  data( graduates2005 )
  data( graduates2006 )
  data( graduates2007 )
  data( graduates2008 )
  data( graduates2009 )
  data( graduates2010 )
  data( graduates2011 )
  data( graduates2012 )
  data( graduates2013 )
  data( graduates2014 )
  data( graduates2015 )
  list<-vector(mode="list", length=16)
  list[[1]] <- graduates2000
  list[[2]] <- graduates2001
  list[[3]] <- graduates2002
  list[[4]] <- graduates2003
  list[[5]] <- graduates2004
  list[[6]] <- graduates2005
  list[[7]] <- graduates2006
  list[[8]] <- graduates2007
  list[[9]] <- graduates2008
  list[[10]] <- graduates2009
  list[[11]] <- graduates2010
  list[[12]] <- graduates2011
  list[[13]] <- graduates2012
  list[[14]] <- graduates2013
  list[[15]] <- graduates2014
  list[[16]] <- graduates2015

  #------------------------------------------------------------------------
  if(type=="graduates"){

    data = data.frame( Year = 2000:2015,
                       Gender = 0)
    for ( i in 1:16 ){
      gender <- table(unlist(list[[i]]$gender))
      data[i,2] <- gender[[1]]/(gender[[1]]+gender[[2]])
    }

    ggplot(data = data)+
      geom_point(aes(x = Year, y = Gender ), size = 5, color = "steelblue")+
      geom_line(aes(x = Year, y = Gender ), size = 2, color = "steelblue")+
      ylab("Ratio of Female Students")+
      ggtitle("Timeplot of Gender Distribution of Student Population")+
      geom_hline( aes( yintercept=mean(data[,2])), size=1, color = "black")+
      annotate("text", x=2016, y=0.50, label= "Mean")+
      theme_bw()

  } else if(type=="latin honors detail"){

    data = data.frame( Year = 2000:2015,
                       Summa = 0,
                       Magna = 0,
                       CumLaude = 0 )
    for ( i in 1:16 ){
      gender <- table(unlist(subset( list[[i]], latinHonors == "Summa Cum Laude")$gender))
      data[i,2] <- gender[[1]]/(gender[[1]]+gender[[2]])

      gender <- table(unlist(subset( list[[i]], latinHonors == "Magna Cum Laude")$gender))
      data[i,3] <- gender[[1]]/(gender[[1]]+gender[[2]])

      gender <- table(unlist(subset( list[[i]], latinHonors == "Cum Laude")$gender))
      data[i,4] <- gender[[1]]/(gender[[1]]+gender[[2]])
    }

    ggplot(data = data)+
      geom_point(aes(x = Year, y = Summa, col = "Summa" ), size = 5, alpha=0.7)+
      geom_line(aes(x = Year, y = Summa, col = "Summa" ), size = 2, alpha=0.7)+
      geom_point(aes(x = Year, y = Magna, col = "Magna" ), size = 5, alpha=0.7)+
      geom_line(aes(x = Year, y = Magna, col = "Magna" ), size = 2, alpha=0.7)+
      geom_point(aes(x = Year, y = CumLaude, col = "CumLaude" ), size = 5, alpha=0.7)+
      geom_line(aes(x = Year, y = CumLaude, col = "CumLaude" ), size = 2, alpha=0.7)+
      ylab("Ratio of Female Students")+
      ggtitle("Timeplot of Gender Distribution of Latin Honors")+
      scale_color_manual( name = "Legend", values = c("green", "steelblue", "coral"), labels = c("Cum Laude", "Magna Cum Laude", "Summa Cum Laude"))+
      theme_bw()

  } else if(type=="department honors detail"){

    data = data.frame( Year = 2000:2015,
                       Honors = 0,
                       HighestHonors = 0 )
    for ( i in 1:16 ){
      gender <- table(unlist(subset( list[[i]], departmentHonors == "Honors")$gender))
      data[i,2] <- gender[[1]]/(gender[[1]]+gender[[2]])

      gender <- table(unlist(subset( list[[i]], departmentHonors == "Highest Honors")$gender))
      data[i,3] <- gender[[1]]/(gender[[1]]+gender[[2]])
    }

    ggplot(data = data)+
      geom_point(aes(x = Year, y = Honors, col = "Honors" ), size = 5, alpha=0.7)+
      geom_line(aes(x = Year, y = Honors, col = "Honors" ), size = 2, alpha=0.7)+
      geom_point(aes(x = Year, y = HighestHonors, col = "HighestHonors" ), size = 5, alpha=0.7)+
      geom_line(aes(x = Year, y = HighestHonors, col = "HighestHonors" ), size = 2, alpha=0.7)+
      ylab("Ratio of Female Students")+
      ggtitle("Timeplot of Gender Distribution of Department Honors")+
      scale_color_manual( name = "Legend", values = c("steelblue", "coral"), labels = c("Hightest Honors", "Honors"))+
      theme_bw()

  } else if(type=="latin honors"){

    data = data.frame( Year = 2000:2015,
                       Gender = 0 )
    for ( i in 1:16 ){
      gender <- table(unlist(subset( list[[i]], latinHonors == "Summa Cum Laude")$gender))+
        table(unlist(subset( list[[i]], latinHonors == "Magna Cum Laude")$gender))+
        table(unlist(subset( list[[i]], latinHonors == "Cum Laude")$gender))
      data[i,2] <- gender[[1]]/(gender[[1]]+gender[[2]])
    }

    ggplot(data = data)+
      geom_point(aes(x = Year, y = Gender ), size = 5, color = "steelblue")+
      geom_line(aes(x = Year, y = Gender ), size = 2, color = "steelblue")+
      ylab("Ratio of Female Students")+
      ggtitle("Timeplot of Gender Distribution of Latin Honors")+
      geom_hline( aes( yintercept=mean(data[,2])), size=1, color = "black")+
      annotate("text", x=2016, y=0.54, label= "Mean")+
      theme_bw()

  } else if(type=="department honors"){

    data = data.frame( Year = 2000:2015,
                       Gender = 0 )
    for ( i in 1:16 ){
      gender <- table(unlist(subset( list[[i]], departmentHonors == "Honors")$gender))+
        table(unlist(subset( list[[i]], departmentHonors == "Highest Honors")$gender))
      data[i,2] <- gender[[1]]/(gender[[1]]+gender[[2]])
    }

    ggplot(data = data)+
      geom_point(aes(x = Year, y = Gender ), size = 5, color = "steelblue")+
      geom_line(aes(x = Year, y = Gender ), size = 2, color = "steelblue")+
      ylab("Ratio of Female Students")+
      ggtitle("Timeplot of Gender Distribution of Department Honors")+
      geom_hline( aes( yintercept=mean(data[,2])), size=1, color = "black")+
      annotate("text", x=2016, y=0.54, label= "Mean")+
      theme_bw()

  } else{
    warning("The specified type does not exist. Enter ?showstats or help(showstats) to view all the summary options available.")
  }
}
