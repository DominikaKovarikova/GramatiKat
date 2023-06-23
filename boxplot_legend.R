##########################################
#                                        # 
#             BOXPLOT LEGEND             #
#                                        #
##########################################

ggplot_box_legend_cz <- function(family = "serif"){
  set.seed(100)
  sample_df <- data.frame(parameter = "test", values = sample(500))
  sample_df$values[1:100] <- 701:800
  sample_df$values[1] <- -350 # poloha dolní tečky
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25. percentil",
                          "50. percentil\n(medián)",
                          "75. percentil")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  ggplot_output <- ggplot2_boxplot(sample_df$values)
  update_geom_defaults("text", list(size = 4, hjust = 0, family = family))
  update_geom_defaults("label", list(size = 4, hjust = 0, family = family)) # kvartily

  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df, aes(x = parameter, y=values), geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df, aes(x = parameter, y=values), width = 0.3, fill = "lightgrey") +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, y = ggplot_output[["75th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]), label = "Mezikvartilové\nrozpětí", fontface = "bold", vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17), y = c(ggplot_output[["upper_whisker"]], ggplot_output[["lower_whisker"]]), label = c("Maximum: nejvyšší hodnota v rámci 1,5-násobku\nmezikvartilového rozpětí nad 75. percentilem", "Minimum: nejnižší hodnota v rámci 1,5-násobku\nmezikvartilového rozpětí pod 25. percentilem")), fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17), y = ggplot_output[["lower_dots"]], label = "Odlehlá hodnota:"), vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(2.1), y = ggplot_output[["lower_dots"]], label = "vše pod Minimem"), vjust = 0.5) +
    geom_text(aes(x = 1.15, y = ggplot_output[["lower_dots"]], label = " a nad Maximem"), vjust = 2.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]], label = names(ggplot_output[["quartiles"]])), vjust = c(0.4,0.85,0.4), fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 20)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "Vysvětlivky") +
    theme(plot.title = element_text(size=14))
#    geom_point(data = head(sample_df, 1), aes(x = parameter, y  = values), size = 4, alpha = 1.0, color='cornflowerblue') +
#    geom_point(data = head(sample_df, 1), aes(x = parameter, y  = values), shape = 1, size = 4, alpha = 1.0, color='black')

  return(explain_plot)

}

ggplot_box_legend_en <- function(family = "serif"){
  set.seed(100)
  sample_df <- data.frame(parameter = "test", values = sample(500))
  sample_df$values[1:100] <- 701:800
  sample_df$values[1] <- -350
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  ggplot_output <- ggplot2_boxplot(sample_df$values)
  update_geom_defaults("text", list(size = 4, hjust = 0, family = family))
  update_geom_defaults("label", list(size = 4, hjust = 0, family = family))

  explain_plot <- ggplot() +     stat_boxplot(data = sample_df,                  aes(x = parameter, y=values),                  geom ='errorbar', width = 0.3) +     geom_boxplot(data = sample_df,                  aes(x = parameter, y=values),                   width = 0.3, fill = "lightgrey") +        theme_minimal(base_size = 5, base_family = family) +     geom_segment(aes(x = 2.3, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["25th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["75th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),                label = "Interquartile\nrange", fontface = "bold",               vjust = 0.4) +     geom_text(aes(x = c(1.17,1.17),                    y = c(ggplot_output[["upper_whisker"]],                         ggplot_output[["lower_whisker"]]),                    label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",                             "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),                   fontface = "bold", vjust = 0.9) +     geom_text(aes(x = c(1.17),                    y =  ggplot_output[["lower_dots"]],                    label = "Outside value"),                vjust = 0.5, fontface = "bold") +     geom_text(aes(x = c(1.9),                    y =  ggplot_output[["lower_dots"]],                    label = "-Value is >1.5 times and"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")  +
    theme(plot.title = element_text(size=14))

  #print("explain_plot")
  #print(explain_plot)
  return(explain_plot)

}



ggplot_box_legend2_cz <- function(family = "serif"){
  set.seed(100)
  sample_df <- data.frame(parameter = "test", values = sample(500))
  sample_df$values[1:100] <- 701:800
  sample_df$values[1] <- -350 # poloha dolní tečky
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25. percentil",
                          "50. percentil\n(medián)",
                          "75. percentil")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  ggplot_output <- ggplot2_boxplot(sample_df$values)
  update_geom_defaults("text", list(size = 4, hjust = 0, family = family))
  update_geom_defaults("label", list(size = 4, hjust = 0, family = family)) # kvartily

  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df, aes(x = parameter, y=values), geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df, aes(x = parameter, y=values), width = 0.3, fill = "lightgrey") +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, y = ggplot_output[["75th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]), label = "Mezikvartilové\nrozpětí", fontface = "bold", vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17), y = c(ggplot_output[["upper_whisker"]], ggplot_output[["lower_whisker"]]), label = c("Maximum: nejvyšší hodnota v rámci 1,5-násobku\nmezikvartilového rozpětí nad 75. percentilem", "Minimum: nejnižší hodnota v rámci 1,5-násobku\nmezikvartilového rozpětí pod 25. percentilem")), fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17), y = ggplot_output[["lower_dots"]], label = "Odlehlá hodnota:"), vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(2.1), y = ggplot_output[["lower_dots"]], label = "vše pod Minimem"), vjust = 0.5) +
    geom_text(aes(x = 1.15, y = ggplot_output[["lower_dots"]], label = " a nad Maximem"), vjust = 2.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]], label = names(ggplot_output[["quartiles"]])), vjust = c(0.4,0.85,0.4), fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "Vysvětlivky") +
    theme(plot.title = element_text(size=14))
  #    geom_point(data = head(sample_df, 1), aes(x = parameter, y  = values), size = 4, alpha = 1.0, color='cornflowerblue') +
  #    geom_point(data = head(sample_df, 1), aes(x = parameter, y  = values), shape = 1, size = 4, alpha = 1.0, color='black')


  #print("explain_plot")
  #print(explain_plot)
  return(explain_plot)

}

ggplot_box_legend2_en <- function(family = "serif"){
  set.seed(100)
  sample_df <- data.frame(parameter = "test", values = sample(500))
  sample_df$values[1:100] <- 701:800
  sample_df$values[1] <- -350
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  ggplot_output <- ggplot2_boxplot(sample_df$values)
  update_geom_defaults("text", list(size = 4, hjust = 0, family = family))
  update_geom_defaults("label", list(size = 4, hjust = 0, family = family))

  explain_plot <- ggplot() +     stat_boxplot(data = sample_df,                  aes(x = parameter, y=values),                  geom ='errorbar', width = 0.3) +     geom_boxplot(data = sample_df,                  aes(x = parameter, y=values),                   width = 0.3, fill = "lightgrey") +        theme_minimal(base_size = 5, base_family = family) +     geom_segment(aes(x = 2.3, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["25th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["75th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),                label = "Interquartile\nrange", fontface = "bold",               vjust = 0.4) +     geom_text(aes(x = c(1.17,1.17),                    y = c(ggplot_output[["upper_whisker"]],                         ggplot_output[["lower_whisker"]]),                    label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",                             "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),                   fontface = "bold", vjust = 0.9) +     geom_text(aes(x = c(1.17),                    y =  ggplot_output[["lower_dots"]],                    label = "Outside value"),                vjust = 0.5, fontface = "bold") +     geom_text(aes(x = c(1.9),                    y =  ggplot_output[["lower_dots"]],                    label = "-Value is >1.5 times and"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")  +
    theme(plot.title = element_text(size=14))

  #print("explain_plot")
  #print(explain_plot)
  return(explain_plot)

}

ggplot_box_legend2smazat <- function(family = "serif"){
  set.seed(100)
  sample_df <- data.frame(parameter = "test", values = sample(500))
  sample_df$values[1:100] <- 701:800
  sample_df$values[1] <- -350
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  ggplot_output <- ggplot2_boxplot(sample_df$values)
  update_geom_defaults("text", list(size = 4, hjust = 0, family = family))
  update_geom_defaults("label", list(size = 4, hjust = 0, family = family))

  explain_plot <- ggplot() +     stat_boxplot(data = sample_df,                  aes(x = parameter, y=values),                  geom ='errorbar', width = 0.3) +     geom_boxplot(data = sample_df,                  aes(x = parameter, y=values),                   width = 0.3, fill = "lightgrey") +        theme_minimal(base_size = 5, base_family = family) +     geom_segment(aes(x = 2.3, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["25th percentile"]],                       yend = ggplot_output[["25th percentile"]])) +     geom_segment(aes(x = 1.2, xend = 2.3,                       y = ggplot_output[["75th percentile"]],                       yend = ggplot_output[["75th percentile"]])) +     geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),                label = "Interquartile\nrange", fontface = "bold",               vjust = 0.4) +     geom_text(aes(x = c(1.17,1.17),                    y = c(ggplot_output[["upper_whisker"]],                         ggplot_output[["lower_whisker"]]),                    label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",                             "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),                   fontface = "bold", vjust = 0.9) +     geom_text(aes(x = c(1.17),                    y =  ggplot_output[["lower_dots"]],                    label = "Outside value"),                vjust = 0.5, fontface = "bold") +     geom_text(aes(x = c(1.9),                    y =  ggplot_output[["lower_dots"]],                    label = "-Value is >1.5 times and"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")

  #print("explain_plot")
  #print(explain_plot)
  return(explain_plot)

}
