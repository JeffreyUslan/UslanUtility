#' Changes directory based on an external reference document
#'
#' @param targ A projectory reference id.
#' @examples
#' cdx("NILM")
#' cdx("rbsaSM2")
#'
#'

cdx<-function(targ="list",newPath=FALSE) {
  require("dplyr")
  cpath="C:\Program Files\R"
  
	if (!newPath){
	  load(paste0(cpath,"cdxdirs.rda"))
	  d$targ=as.character(d$targ)
	  d$path=as.character(d$path)
    
	  loc=which(d$targ==targ)
	  if(length(loc))
	  {
	    setwd(d$path[loc])  
	  } else {
	    cat("Could not find",key,"path\n")
	  }
	} else {
	  if (file.exists(paste0(cpath,"cdxdirs.rda"))){
	    load(paste0(cpath,"cdxdirs.rda"))
	  } else{
	    d=data.frame(targ=NULL,path=NULL) 
	  }
    bind_vec=data.frame(targ,as.character(choose.dir()))
	  names(bind_vec)=c("targ","path")
    d=rbind(d,bind_vec)
	  d=distinct(d)
    d=na.omit(d)
	  names(d)=c("targ","path")
	  save(d,file=paste0(cpath,"cdxdirs.rda"))
	}
}
