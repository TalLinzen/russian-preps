# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Plots for Gouskova and Linzen, Morphological conditioning of phonological 
# regularization.

library(ggplot2)
library(plyr)
library(grid)
library(scales)

if ((root <- Sys.getenv('RUSS_PREPS_ROOT')) == "") {
    stop('RUSS_PREPS_ROOT must be set')
}

directory <- file.path(root, 'morph_squib'

load <- function() {
    counts <- read.csv(file.path(directory, 'morph_counts.csv'))
    counts$tokens <- counts$cv + counts$c
    counts$cv_rate <- counts$cv / counts$tokens
    counts <- add_confint(counts)
    return(counts)
}

add_confint <- function(df) {
    return(ddply(df, .(form, corpus), function(row) {
        b <- binom.test(row[1, 'cv'], row[1, 'tokens'])
        row$low <- b$conf.int[1]
        row$high <- b$conf.int[2]
        return(row)
    }))
}

plot_single <- function(df) {
    p <- ggplot(df, aes(x=cv_rate, y=form, xmax=high, xmin=low)) +
        geom_vline(aes(xintercept=c(0, 1)), linetype='dashed') +
        geom_errorbarh() + 
        geom_point() +
        scale_x_continuous(limits=c(0, 1), labels=percent) +
        theme_bw() +
        theme(panel.border=element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.margin=unit(c(0.1, 0, 0, 0), 'in'),
            panel.margin=unit(rep(0, 4), 'in'))
    return(p)
}

plot_all_single <- function(df) {
    d_ply(df, .(form, corpus), function(row) {
        filename <- sprintf('%s_%s.pdf', row$form[1], row$corpus[1])
        filename <- file.path(directory, 'plots', filename)
        p <- plot_single(row)
        ggsave(filename=filename, plot=p, height=32/50, width=124/50)
    })
}

plot_all <- function(df) {
    p <- ggplot(df, aes(x=cv_rate, y=form, xmax=high, xmin=low)) +
        geom_errorbarh(height=0.2) + 
        geom_point() +
        scale_x_continuous(limits=c(0, 1), labels=percent) +
        theme_bw() +
        facet_grid(group ~ corpus, scales='free', space='free')
    return(p)
    
}
