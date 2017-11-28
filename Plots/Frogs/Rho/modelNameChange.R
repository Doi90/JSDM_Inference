modelNameChange <- function(dataframe){
  
  tmp.df <- dataframe
  
  if("BayesComm" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("BayesComm","MPR",x,fixed=TRUE)
    })))
  }
  
  if("boral" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("boral","LPR",x,fixed=TRUE)
    })))
  }
  
  if("Clark" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("Clark","DPR",x,fixed=TRUE)
    })))
  }
  
  if("Ovaskainen 2016 NS" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("Ovaskainen 2016 NS","HLR-NS",x,fixed=TRUE)
    })))
  }
  
  if("Ovaskainen 2016" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("Ovaskainen 2016","HLR-S",x,fixed=TRUE)
    })))
  }
  
  if("Pollock" %in% unique(tmp.df[,"model"])){
    tmp.df$model <- as.factor(unlist(lapply(tmp.df$model,function(x) {
      gsub("Pollock","HPR",x,fixed=TRUE)
    })))
  }
  return(tmp.df)
}