#' Using ggplot2 to plot matrix of correlations
#' 
#' Lightly modified from a post by Mike Lawrence at:
#' https://groups.google.com/forum/#!searchin/ggplot2/ggcorplot/ggplot2/odaZVAyKvE4/lsFIE86pUVoJ
#' 
#' \code{ggcorplot} takes a data frame or matrix, strips numeric variables, and 
#' makes a "splom" style correlation plot, with significance values. Useful for 
#' exploratory data analysis. 
#' 
#' @param data Data frame or matrix to be plotted.
#' @param var_text_size Variable text size.
#' 
#' @keywords plotting
#' @export
#' @examples
#' ## using iris data
#' ggcorplot(iris)
#' 
#' @seealso \code{\link[ggplot2]{qplot}}

ggcorplot <- function(data, 
                      var_text_size = 5) {
  
  # munge data ----------------------------
  
  # drop non-numeric columns
  nums <- sapply(data, is.numeric)
  data <- data[,nums]
  
  # reshape
  data <- as.data.frame(scale(data)) # scale
  
  # obtain new data frame
  # this should be easy to make functional (but I haven't figured it out)
  z=data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      temp=as.data.frame(cbind(x,y))
      temp=cbind(temp, names(data)[i], names(data)[j])
      z=rbind(z, temp)
      j=j+1
    }
  }
  
  names(z)=c('x','y','x_lab','y_lab')
  z$x_lab = ezLev(factor(z$x_lab), names(data))
  z$y_lab = ezLev(factor(z$y_lab), names(data))
  z=z[z$x_lab!=z$y_lab,]
  
  #obtain correlation values
  z_cor = data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      x_mid = min(x)+diff(range(x))/2
      y_mid = min(y)+diff(range(y))/2
      this_cor = cor(x,y)
      this_cor.test = 0
      this_cor.test = cor.test(x,y)
      this_col = ifelse(this_cor.test$p.value<.05,'<.05','>.05')
      this_size = (this_cor)^2
      cor_text = ifelse(
        this_cor>0
        ,substr(format(c(this_cor,.123456789),digits=2)[1],2,4)
        ,paste('-',substr(format(c(this_cor,.123456789),digits=2)[1],3,5),sep='')
      )
      b=as.data.frame(cor_text)
      b=cbind(b,x_mid,y_mid,this_col,this_size,names(data)[j],names(data)[i])
      z_cor=rbind(z_cor,b)
      j=j+1
    }
  }
  names(z_cor)=c('cor','x_mid','y_mid','p','rsq','x_lab','y_lab')
  z_cor$x_lab = ezLev(factor(z_cor$x_lab),names(data))
  z_cor$y_lab = ezLev(factor(z_cor$y_lab),names(data))
  diag = z_cor[z_cor$x_lab==z_cor$y_lab,]
  z_cor=z_cor[z_cor$x_lab!=z_cor$y_lab,]
  
  # ggplot 2 layers ----------------------------
  # used to be jitter, but this obscures data structure if used naively
  points_layer <- ggplot2::layer(geom = 'point', 
                                 data = z, 
                                 mapping = ggplot2::aes(x = x, y = y))
  
  lm_line_layer <- ggplot2::layer(geom = 'line', 
                                  geom_params = list(colour = 'red'), 
                                  stat = 'smooth', 
                                  stat_params = list(method = 'lm'), 
                                  data = z, 
                                  mapping = ggplot2::aes(x = x, y = y))
  
  lm_ribbon_layer <- ggplot2::layer(geom = 'ribbon',
                                    geom_params = list(fill = 'green', alpha = .5),
                                    stat = 'smooth',
                                    stat_params = list(method = 'lm'),
                                    data = z,
                                    mapping = ggplot2::aes(x = x, y = y))
  
  cor_text <- ggplot2::layer(geom = 'text', data = z_cor, 
                             mapping = ggplot2::aes(x = y_mid, 
                                           y = x_mid, 
                                           label = cor,
                                           size = rsq, 
                                           colour = p))
  
  var_text <- ggplot2::layer(geom = 'text',
                             geom_params = list(size=var_text_size), 
                             data = diag, 
                             mapping = ggplot2::aes(x=y_mid, y=x_mid, label=x_lab))
  
  ggplot2::ggplot() + 
    points_layer +
    lm_ribbon_layer + lm_line_layer +
    var_text + cor_text +
    ggplot2::facet_grid(y_lab~x_lab, scales='free') +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position='none') + 
    ggplot2::scale_size(limits = c(0,1))
}


#define a helper function (borrowed from the "ez" package)
ezLev=function(x, new_order){
  for(i in rev(new_order)){
    x=relevel(x,ref=i)
  }
  return(x)
}

