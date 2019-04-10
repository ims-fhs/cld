ref_mode_to_ggplot <- function(str) {
  str_sub <- gsub("[%/]","_",str) #sub all % and / with ""
  str_split <- trimws(strsplit(str_sub,"_")[[1]]) #split by whitespace
  str_corr <- c(str_split,rep(NA,3-length(str_split) %% 3)) #correct length, fill the end with NAs

  df <- as.data.frame(matrix(str_corr,ncol=3,byrow=T), stringsAsFactors = FALSE) #convert to data.frame via matrix
  colnames(df) <- c("x","y","curve") #set colnames
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  e <- "ggplot()"
  for(i in 1:(nrow(df) - 1)) {
    if (df$curve[i] == "-") {
      e <- paste0(e, " + geom_segment(aes(x = ", df$x[i], ", y =", df$y[i], ", xend = ", df$x[i + 1], ", yend = ", df$y[i + 1], "))")
    } else if (df$curve[i] == "(") {
      e <- paste0(e, " + geom_curve(aes(x = ", df$x[i], ", y =", df$y[i], ", xend = ", df$x[i + 1], ", yend = ", df$y[i + 1], "), curvature = -.2)")
    } else if (df$curve[i] == ")") {
      e <- paste0(e, " + geom_curve(aes(x = ", df$x[i], ", y =", df$y[i], ", xend = ", df$x[i + 1], ", yend = ", df$y[i + 1], "), curvature = .2)")
    } else if (df$curve[i] == "s") {
      x_mid <- df$x[i] + (df$x[i + 1] - df$x[i])/2
      y_mid <- df$y[i] + (df$y[i + 1] - df$y[i])/2
      e <- paste0(e, " + geom_curve(aes(x = ", df$x[i], ", y =", df$y[i], ", xend = ", x_mid, ", yend = ", y_mid, "), curvature = .2) + geom_curve(aes(x = ", x_mid, ", y =", y_mid, ", xend = ", df$x[i + 1], ", yend = ", df$y[i + 1], "), curvature = -.2)")
    }
  }
  return(e)
}
