`bootdif` <-
  function(y, g, conf.level=0.95, B.reps = 2000) {
    require(Hmisc)
    lowq = (1 - conf.level)/2
    g <- as.factor(g)
    a <- attr(smean.cl.boot(y[g==levels(g)[1]], B=B.reps, reps=TRUE),'reps')
    b <- attr(smean.cl.boot(y[g==levels(g)[2]], B=B.reps, reps=TRUE),'reps')
    meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
    a.b <- quantile(b-a, c(lowq,1-lowq))
    res <- c(meandif, a.b)
    names(res) <- c('Mean Difference',lowq, 1-lowq)
    res
  }

`Emp_Rule` <- function(x, na.rm=FALSE) {
  tmp.overall_n <- length(x)
  tmp.mean <- mean(x)
  tmp.sd <- sd(x)
  tmp.in1sd <- length(x[x > tmp.mean - tmp.sd & 
                          x < tmp.mean + tmp.sd])
  tmp.in2sd <- length(x[x > tmp.mean - 2*tmp.sd & 
                          x < tmp.mean + 2*tmp.sd])
  tmp.in3sd <- length(x[x > tmp.mean - 3*tmp.sd & 
                          x < tmp.mean + 3*tmp.sd])
  tmp.prop1sd <- tmp.in1sd / tmp.overall_n
  tmp.prop2sd <- tmp.in2sd / tmp.overall_n
  tmp.prop3sd <- tmp.in3sd / tmp.overall_n
  res.count <- c(tmp.in1sd, tmp.in2sd, tmp.in3sd, tmp.overall_n)
  res.prop <- c(tmp.prop1sd, tmp.prop2sd, tmp.prop3sd, 1)
  res.exp <- c(0.68, 0.95, 0.997, 1)
  res <- data.frame(count = res.count, proportion = res.prop)
  row.names(res) <- c("Mean +/- 1 SD", "Mean +/- 2 SD", "Mean +/- 3 SD", "Entire Data Set")
  return(pander::pander(res))
  rm(tmp.overall_n, tmp.mean, tmp.sd, tmp.in1sd, tmp.in2sd, tmp.in3sd,
     tmp.prop1sd, tmp.prop2sd, tmp.prop3sd, res.count, res.prop, res)
}

`fd_bins` <- function(x, na.rm=FALSE) {
  bw <- 2 * IQR(x) / (length(x)^(1/3))
  bins <- round((max(x) - min(x)) / bw, digits = 0)
  return(bins)
}

qq_int <- function(tempdat, na.rm = TRUE) {
  y <- quantile(tempdat, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  intercept <- y[1L] - slope * x[1L]
  return(intercept)
}

qq_slope <- function(tempdat, na.rm = TRUE) {
  y <- quantile(tempdat, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  intercept <- y[1L] - slope * x[1L]
  return(slope)
}

`saifs.ci` <- 
  function(x, n, conf.level=0.95, dig=3)
  {
    p.sample <- round(x/n, digits=dig)
    
    p1 <- x / (n+1)
    p2 <- (x+1) / (n+1)
    
    var1 <- (p1*(1-p1))/n
    se1 <- sqrt(var1)
    var2 <- (p2*(1-p2))/n
    se2 <- sqrt(var2)
    
    lowq = (1 - conf.level)/2
    tcut <- qt(lowq, df=n-1, lower.tail=FALSE)
    
    lower.bound <- round(p1 - tcut*se1, digits=dig)
    upper.bound <- round(p2 + tcut*se2, digits=dig)
    res <- c(p.sample, lower.bound, upper.bound)
    names(res) <- c('Sample Proportion',lowq, 1-lowq)
    res
  }

`skew1` <- function(x, na.rm=FALSE) {
  a <- (mean(x) - median(x))/sd(x)
  return(a)
}

`twobytwo` <-
  function(a,b,c,d, namer1 = "Row1", namer2 = "Row2", namec1 = "Col1", namec2 = "Col2", 
           conf.level = 0.95)
    # build 2 by 2 table and run Epi library's twoby2 command to summarize
    # from the row-by-row counts in a cross-tab
    # upper left cell is a, upper right is b, lower left is c, lower right is d
    # names are then given in order down the rows then across the columns
    # use standard epidemiological format - outcomes in columns, treatments in rows
  {
    require(Epi)
    .Table <- matrix(c(a, b, c, d), 2, 2, byrow=T, dimnames=list(c(namer1, namer2), c(namec1, namec2)))
    twoby2(.Table, alpha = 1 - conf.level)
  }

## Code from https://gist.github.com/rentrop/d39a8406ad8af2a1066c
## Slightly modified by T. Love

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2], color="red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    labs(y = "Ordered Sample Data", x = "Standard Normal Quantiles")
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  # coef ## can print 25th and 75h percentiles if desired by removing the single # in this line
}



# EDA alpha as built by Ascha and Love
# released as a test to students in 431: 2016-11-01

`eda.1sam` <- 
    function(dataframe, variable, 
             x.title = "", ov.title = "", 
             col_A = '#440154', col_B = '#FDE725', col_C = '#1FA187', ...) {
        
        #requirements
        require(ggplot2)
        require(gridExtra)
        
        var_values <- as.numeric(na.omit(with(dataframe, variable)))
        
        # binwidth
        fd_bins <- function(x, na.rm = TRUE) {
            bw <- 2 * IQR(x) / (length(x) ^ (1/3))
            bins <- round((max(x) - min(x)) / bw, digits = 0)
            return(bins)
        }
        
        # qq line
        stat_qq_abline <- function(some_variable, ...) {
            y <- quantile(some_variable[!is.na(some_variable)], c(0.25, 0.75))
            x <- qnorm(c(0.25, 0.75))
            slope <- diff(y) / diff(x)
            intercept <- y[1L] - slope * x[1L]
            geom_abline(slope = slope, intercept = intercept, ...)
        }
        
        hist_plot <- 
            ggplot(dataframe, aes(x = variable)) + 
            geom_histogram(
                aes(y = ..density..), 
                bins = fd_bins(var_values),
                fill = col_A, col = col_B) + 
            coord_flip() + 
            stat_function(fun = dnorm, 
                          args = list(mean = mean(var_values), 
                                      sd = sd(var_values)), 
                          lwd = 1.5, col = col_C) + 
            labs(title = "Histogram", 
                 x = x.title, y = "")
        
        boxplot_plot <- 
            ggplot(dataframe, aes(x = 1, y = variable)) + 
            geom_boxplot(fill = col_B, notch = TRUE, col = col_A, outlier.color = col_A) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) + 
            labs(title = "Boxplot", x = "", y = "")
        
        qqplot_plot <- 
            ggplot(dataframe, aes(sample = variable)) + 
            geom_qq(geom = "point", col = col_A, size = 2) + 
            stat_qq_abline(with(dataframe, variable), color = col_C, size = 1.25) + 
            labs(title = "Normal QQ", 
                 x = "", y = "")
        
        grid.arrange(hist_plot, boxplot_plot, qqplot_plot, nrow = 1, top = ov.title)
        
        #   pander(mosaic::favstats(with(dataframe, variable)), 
        #          top = paste("Numerical summary of", substitute(variable), "from", substitute(dataframe)))
        
    }

`eda.2sam` <- 
    function(outcome, group,
             y.title = "", ov.title = "", 
             col_A = '#440154', col_B = '#FDE725', col_C = '#1FA187', ...) {
        
        #requirements
        require(ggplot2)
        require(gridExtra)
        require(tibble)
        require(stringr)
        require(tidyr)
        require(dplyr)
        
        name_outcome <- 
            ifelse(length(grep(pattern = "\\$", x = substitute(outcome))) > 0, 
                   as.character(substitute(outcome))[3], 
                   as.character(substitute(outcome))[1])
        
        name_group <- 
            ifelse(length(grep(pattern = "\\$", x = substitute(group))) > 0, 
                   as.character(substitute(group))[3], 
                   as.character(substitute(group))[1])
        
        df <- 
            tbl_df(data = 
                       data.frame(name_outcome = outcome, 
                                  name_group = group))
        
        if(y.title == ""){
            y.title <- name_outcome
        }
        
        # binwidth
        fd_bins <- function(x, na.rm = TRUE) {
            bw <- 2 * IQR(x) / (length(x) ^ (1/3))
            bins <- round((max(x) - min(x)) / bw, digits = 0)
            return(bins)
        }
        
        
        hist_plot <- 
            ggplot(df, aes(x = outcome, fill = name_group)) +
            geom_histogram(aes(y = ..density..), bins = fd_bins(outcome), col = "white") + 
            facet_grid(~ name_group) + 
            labs(title = "Comparison Histogram",
                 x = y.title, y = "") + 
            theme(legend.position = "none")
        
        boxplot_plot <-
            ggplot(df, aes(x = group, y = outcome, fill = name_group)) +
            geom_boxplot(notch = TRUE) +
            coord_flip() + 
            theme(legend.position = "none") +
            labs(title = "Comparison Boxplot", x = "", y = y.title)
        
        if(ov.title == "") {
            grid.arrange(boxplot_plot, hist_plot, nrow = 2, top = paste("Comparison of",name_outcome, "on", name_group))
        } else {
            grid.arrange(boxplot_plot, hist_plot, nrow = 2, top = ov.title)
        }
        
        
    }

# Code from Gelman and Carlin

`retrodesign` <- function(A, s, alpha=.05, df=Inf, 
                        n.sims=10000){
    z <- qt(1-alpha/2, df)
    p.hi <- 1 - pt(z-A/s, df)
    p.lo <- pt(-z-A/s, df)
    power <- p.hi + p.lo
    typeS <- p.lo/power
    estimate <- A + s*rt(n.sims,df)
    significant <- abs(estimate) > s*z
    exaggeration <- mean(abs(estimate)[significant])/A
    return(list(power=power, typeS=typeS, 
                exaggeration=exaggeration))
}

# panel.hist and panel.cor modified from Chang

`panel.hist` <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

`panel.cor` <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * (1 + abs(r)) / 2)
}

