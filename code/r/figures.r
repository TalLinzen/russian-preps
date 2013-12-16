# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

if ((root <- Sys.getenv('RUSS_PREPS_ROOT')) == "") {
    stop('RUSS_PREPS_ROOT must be set')
}

source(file.path(root, 'code', 'r', 'stats.r'))
source(file.path(root, 'code', 'r', 'corpus.r'))

library(boot)
library(plyr)
library(tikzDevice)
library(ggplot2)
library(grid)
library(scales)

############
### Settings

options(tikzLatexPackages = c(getOption('tikzLatexPackages'), 
    '\\usepackage{tipa}'))

black_and_white <- T
theme_set(theme_bw())
theme_update(
        plot.margin=unit(rep(1, 4), 'lines'),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.x=element_text(vjust=0),
        axis.title.y=element_text(angle=90, vjust=0),
        legend.position='right',
        legend.key=element_rect(size=3, color='white'))

linetypes <- c('solid', 'dotted', 'dashed', 'longdash')

axis_arguments <- list(name='CV rate',
            limits=c(0, 1),
            labels=c('0\\%', '50\\%', '100\\%'),
            breaks=c(0, 0.5, 1))

epenth_x_scale <- do.call(scale_x_continuous, axis_arguments)
epenth_y_scale <- do.call(scale_y_continuous, axis_arguments)


#######################
### Auxiliary functions

ipa <- function(labels) {
    return(aaply(labels, 1, function(x) sprintf('\\textipa{%s}', x)))
}

remove_outliers <- function(df) {
    return(ddply(df, .(full_cluster), function (clst) {
        q <- quantile(clst$noep_prop, c(0.1, 0.9))
        return(subset(clst, noep_prop >= q[1] & noep_prop <= q[2]))
    }))
}

pdflatex <- function(d, f) {
    wd <- getwd()
    setwd(d)
    system(paste('pdflatex', f))
    setwd(wd)
}

output_tikz <- function(d, funcname, width=4, height=2.7, standAlone=FALSE) {
    tikz(file.path(d, sprintf('%s.tex', funcname)), width=width,
        height=height, standAlone=standAlone)
    p <- get(funcname)()
    if (!is.logical(p)) { 
        print(p)
    }
    dev.off()
    if (standAlone) {
        pdflatex(d, funcname)
    }
}

add_exception_confint <- function(df) {
    df$cv <- floor(df$tokens * df$prop)
    return(ddply(df, .(tipatrans), function(row) {
        b <- binom.test(row[1, 'cv'], row[1, 'tokens'])
        row$low <- b$conf.int[1]
        row$high <- b$conf.int[2]
        return(row)
    }))
}

ratio_with_bootstrapped_ci <- function(df) {

    ratiostat <- function(data, indices) {
        data <- data[indices,]
        return(with(data, sum(!s_better) / 
                    (sum(s_better) + sum(!s_better))))
    }

    bootresults <- boot(data=df, statistic=ratiostat, R=999)
    ci <- boot.ci(bootresults, type='basic')
    out <- data.frame(y=ci$t0, ymin=ci$basic[4], ymax=ci$basic[5])
}

estimate_ratios_with_stress <- function() {
    responses <- load_processed_responses()
    return(ddply(responses, .(latex_cluster, first, stress), 
        ratio_with_bootstrapped_ci))
}

process_ratios <- function(ratios, collapse_v_f=FALSE) {
    ratios$latex_cluster <- factor(ratios$latex_cluster,
        levels=c('\\textrtails t', '\\textrtails r',
            'rd', 'rn', 'fs', 'vz', 'vd',
            'vn', 'xs', 'xr', 'ms', 'k'))
    ratios <- transform(ratios, first=as.character(first))
    ratios <- transform(ratios,
        first=ifelse(first == 'sh', '\\textrtails', first))

    if (collapse_v_f) {
        ratios$first <- ifelse(ratios$first %in% c('v', 'f'), 'v / f',
                               ratios$first)
        ratios$first <- factor(ratios$first, 
                               levels=c('k', 'x', '\\textrtails', 
                                        'v / f', 'm', 'r'))
    }
    else {
        ratios$first <- factor(ratios$first, 
                               levels=c('k', 'x', '\\textrtails', 
                                        'v', 'f', 'm', 'r'))
    }
    
    return(ratios)
}

density_plot <- function(df) {
    if (black_and_white) {
        a <- aes(x=1 - noep_prop, linetype=klass)
    }
    else {
        a <- aes(x=1 - noep_prop, color=klass)
    }
    q <- ggplot(df, a) + epenth_x_scale + geom_density() +
        theme(axis.title.y=element_blank())
    return(q)
}

plot_exceptions <- function(filename, facet_by_lemma=T, rev_lemmas=F) {
    df <- read.csv(filename)
    df$tipatrans <- ipa(df$tipatrans)
    df <- add_exception_confint(df)
    if (facet_by_lemma) {
        df$transcr_lemma <- ipa(df$transcr_lemma)
        if (rev_lemmas) {   # hack for dno
            lv <- levels(factor(df$transcr_lemma))
            df$transcr_lemma <- factor(df$transcr_lemma, levels=rev(lv))
        }
    }

    p <- ggplot(df, aes(y=tipatrans, x=prop, xmin=low, xmax=high)) + 
        geom_errorbarh(height=0.2) + geom_point() +
        scale_y_discrete('') + 
        epenth_x_scale +
        theme(axis.title.y=element_text(size=rel(0.8), vjust=0.2))

    if (facet_by_lemma) {
        p <- p + facet_grid(transcr_lemma ~ ., scales='free_y', space='free')
    }

    return(p)
}

load_syandex_with_classes <- function() {
    syandex <- load_csv('s_yandex.csv')
    syandex$klass <- 'Other'
    syandex$klass[
        syandex$first %in% c('lj', 'l', 'v', 'r', 'm', 'n')] <- 'Sonorant'
    syandex$klass[syandex$first %in% c('s', 'sh', 'z', 'zh')] <- 'Sibilant'
    syandex$klass <- factor(syandex$klass, 
        levels=c('Sibilant', 'Sonorant', 'Other'))
    return(syandex)
}

yandex_s_vC_dataset <- function() {
    syandex <- load_csv('s_yandex.csv')
    syandex <- subset(syandex, first == 'v' & second != 'v' & second != "'")
    return(syandex)
}

by_lemma <- function(df, cv) {
    df$tipatrans <- df$form
    df <- add_exception_confint(df)
    medians <- ddply(df, .(sim), summarise, med=median(prop))
    return(ggplot(df, 
        aes(y=tipatranslemma, x=prop, xmin=low, xmax=high)) +
        geom_point() + geom_errorbarh(height=0.2) +
        geom_vline(aes(xintercept=med), data=medians, linetype='dashed',
                   color='red') +
        scale_y_discrete('') + 
        epenth_x_scale + 
        facet_grid(sim ~ ., scales='free_y', space='free'))
}


#############
### Load data

sonorants <- c('l', 'r', 'm', 'n')
stridents <- c('s', 'sh', 'z', 'zh')

exc_path <- file.path(root, 'results', 'corpus', 'rnc_exceptions_fixed.csv')
exceptions <- suppressWarnings(transform(
    read.csv(exc_path, stringsAsFactors=FALSE),
    tipatranslemma=ipa(tipatranslemma),
    ep=as.numeric(ep),
    noep=as.numeric(noep)))

monosyll_path <- file.path(root, 'results', 'monosyll.csv')
monosyll <- transform(read.csv(monosyll_path, stringsAsFactors=FALSE),
                      tipaform=ipa(tipaform),
                      logtokens=log(c+cv),
                      prop=(cv+1)/(c+cv+1),
                      logodds=log((cv+1)/(c+1)))

similarity_df <- function(preposition, both, others) {
    x <- subset(exceptions, prep==preposition)
    x$sim <- 'none'
    x[x$first %in% others | x$second %in% others, 'sim'] <- 'place'
    x[x$first %in% both | x$second %in% both, 'sim'] <- 'both'
    x$sim <- factor(x$sim, levels=c('none', 'place', 'both'))
    return(x)
}

labial_others <- c('b', 'm', 'p')
labial_fricatives <- c('v', 'f')
v <- similarity_df('v', labial_fricatives, labial_others)
coronal_others <- c('t', 'tj', 'd', 'dj', 'ts', 'ch', 'n', 'nj',
    'r', 'rj', 'l', 'lj', 'j')
coronal_fricatives <- c('s', 'z', 'sh', 'zh')
s <- similarity_df('s', coronal_fricatives, coronal_others)
dorsal_others <- c('x')
dorsal_stops <- c('k', 'g')
k <- similarity_df('k', dorsal_stops, dorsal_others)

dvor_path <- file.path(root, 'results', 'corpus', 'dvor.csv')
den_path <- file.path(root, 'results', 'corpus', 'den.csv')
dno_path <- file.path(root, 'results', 'corpus', 'dno.csv')
dvor_derived_p <- file.path(root, 'results', 'corpus', 'dvor_derived.csv')


###########
### Figures

# Figure 1a
s_lemma <- function () by_lemma(s, 'so')

# Figure 1b
v_lemma <- function () by_lemma(v, 'vo')

# Figure 2a
dvor <- function() {
    return(plot_exceptions(dvor_path))
}

# Figure 2b
dvor_derived <- function() {
    filename <- dvor_derived_p

    df <- read.csv(filename)
    df$tipatrans <- ipa(df$tipatrans)
    df <- add_exception_confint(df)
    
    levels <- c("dvar\\'e\\t{ts}", "dv\\'orik", "dv\\'ornik", 
                "dv@rin\\'\\i n")

    df$transcr_lemma <- factor(ipa(df$transcr_lemma))
    unq <- ddply(df, .(transcr_lemma), head, 1)
    lev <- levels(unq$transcr_lemma)[unq$ord]
    df$transcr_lemma <- factor(df$transcr_lemma, levels=lev)

    p <- ggplot(df, aes(y=tipatrans, x=prop, xmin=low, xmax=high)) + 
        geom_errorbarh(height=0.2) + geom_point() +
        scale_y_discrete('') + 
        epenth_x_scale +
        theme(axis.title.y=element_text(size=rel(0.8), vjust=0.2))

    p <- p + facet_grid(transcr_lemma ~ ., scales='free_y', space='free')
    return(p)
}

# Figure 3a
den <- function() {
    return(plot_exceptions(den_path))
}

# Figure 3b
dno <- function() {
    return(plot_exceptions(dno_path, rev_lemmas=T))
}

# Figure 4
k_monosyll_sonority_prop <- function() {
    yl <- with(monosyll, c(min(prop), max(prop)) * 1.2 - 0.1)
    xl <- with(monosyll, c(min(sonslope), max(sonslope)) * 1.2)
    return(ggplot(aes(x=sonslope, y=prop, label=tipaform),
                  data=monosyll) +
           scale_x_continuous('Sonority slope', limits=xl, breaks=c(-5:5)) +
           scale_y_continuous('[ko] proportion', limits=yl, 
                              breaks=seq(0, 1, 0.2)) +
           theme(legend.position='none',
                panel.grid.major=element_blank(),
                axis.title.y=element_text(vjust=0.4),
                panel.grid.minor=element_blank()) +
           geom_text(aes(size=logtokens)) + scale_size(range=c(4,6)))
}

# Figure 5
yandex_boxplot_by_first <- function() {
    syandex <- load_syandex_with_classes()
    p <- ggplot(syandex, aes(x=first_transcribed, y=1-noep_prop))
    p <- p + facet_grid(. ~ klass, scales='free_x', space='free')
    p <- p + geom_boxplot(outlier.size=0.5) + epenth_y_scale + 
        scale_x_discrete('Cluster-initial consonant')
    return(p)
}

# Figure 6
yandex_s_strid <- function() {
    syandex <- load_csv('s_yandex.csv')
    syandex <- subset(syandex, first %in% c('s', 'sh', 'z', 'zh'))
    syandex$klass <- syandex$first
    d <- density_plot(syandex)
    d <- d + scale_linetype_manual('First consonant\nof cluster',
        labels=c('\\textipa{s (s svin\\t{ts}\\\'a)}',
            '\\textipa{z (z zvin\\\'a)}',
            '\\textipa{\\:s (s \\:sv\\\'ed@f)}',
            '\\textipa{\\:z (z \\:zri\\t{ts}\\\'a)}'),
        breaks=c('s', 'z', 'sh', 'zh'), values=linetypes)
    return(d)
}

# Figure 7
yandex_s_vC_stress <- function() {
    syandex <- yandex_s_vC_dataset()
    syandex$klass <- syandex$first_stressed

    d <- density_plot(syandex)
    d <- d + scale_linetype_manual('Stressed syllable',
        labels=c('First \\textipa{(s vd\\\'ox@)}', 
            'Other \\textipa{(s vdaf\\t{ts}\\\'a)}'),
        breaks=c(TRUE, FALSE), values=linetypes)

    return(d)
}

# Figure 8
yandex_s_vC_stop_strident <- function() {
    syandex <- yandex_s_vC_dataset()
    syandex <- subset(syandex, !(second %in% sonorants))

    levels <- c('vS', 'vK')
    syandex$klass <- factor(ifelse(syandex$second %in% stridents,
                                   'vS', 'vK'), levels=levels)

    d <- density_plot(syandex)
    d <- d + coord_cartesian(ylim=c(0, 11)) + scale_linetype_manual('Cluster',
        labels=c(ipa('s+v\\*S (s vz\\super j\\\'atk@j)'), 
                 ipa('s+v\\*K (s vdaf\\t{ts}\\\'a)')),
        breaks=levels, values=linetypes)

    return(d)
}

# Figure 9
yandex_s_vC_stop_sonorant <- function() {
    syandex <- yandex_s_vC_dataset()
    syandex <- subset(syandex, !(second %in% stridents))

    levels <- c('vR', 'vK')
    syandex$klass <- factor('vK', levels=levels)
    syandex$klass[syandex$second %in% sonorants] <- 'vR'

    d <- density_plot(syandex)
    d <- d + coord_cartesian(ylim=c(0, 11)) + scale_linetype_manual('Cluster',
        labels=c(ipa('s+v\\*R (s vrid\\\'ami)'), 
                 ipa('s+v\\*K (s vdaf\\t{ts}\\\'a)')),
        breaks=levels, values=linetypes)

    return(d)
}

# Figure 10a
yandex_s_vC_case <- function() {
    syandex <- yandex_s_vC_dataset()
    syandex$klass <- factor(syandex$case, levels=c('gen', 'inst'))

    d <- density_plot(syandex)
    d <- d + scale_linetype_manual('Case',
        labels=c(' Genitive \\textipa{(s vz\\super j\\\'atki)}',
            ' Instrumental \\textipa{(s vz\\super j\\\'atk@j)}'),
        breaks=c('gen', 'inst'), values=linetypes)
    d <- d + theme(legend.position='bottom', legend.direction='vertical')
    return(d)
}

# Figure 10b
yandex_s_vC_number <- function() {

    syandex <- yandex_s_vC_dataset()
    syandex$klass <- factor(syandex$number, levels=c('sg', 'pl'))

    d <- density_plot(syandex)
    d <- d + scale_linetype_manual('Number',
        labels=c(' Singular \\textipa{(s vzl\\super j\\\'ot@)}',
            ' Plural \\textipa{(s vzl\\super j\\\'ot@f)}'),
        breaks=c('sg', 'pl'), values=linetypes)
    d <- d + theme(legend.position='bottom', legend.direction='vertical')
    return(d)
}

# Figure 11
experiment_overview_stress <- function() {
    ratios_stress <- estimate_ratios_with_stress()
    ratios <- process_ratios(ratios_stress, collapse_v_f=TRUE)    
    a <- aes(color=stress, x=latex_cluster, y=y, ymax=ymax, ymin=ymin)

    axis_arguments <- list(name='CV preference',
            limits=c(0, 1),
            breaks=c(0, 0.25, 0.5, 0.75, 1), 
            labels=c('0\\%', '25\\%', '50\\%', '75\\%', '100\\%'))
    epenth_y_scale <- do.call(scale_y_continuous, axis_arguments) 

    p <- ggplot(ratios, a) +
        geom_pointrange(position=position_dodge(width=0.4))
    p <- p + facet_grid(. ~ first, scales='free_x', space='free') +
        epenth_y_scale + xlab('Cluster')
    color <- ifelse(black_and_white, 'black', 'red')
    p <- p + geom_hline(aes(yintercept=0.5), color=color, linetype='dotted')
    p <- p + scale_color_grey('Stress', breaks=c('initial', 'final'),
        labels=c('Initial', 'Final'), start=0, end=0.5)
    return(p)
}

# Figure that didn't make it into the paper: a graphical representation of
# Table IV
experiment_lmer <- function(model, flip=TRUE) {
    labels <- names(fixef(model))

    model <- data.frame(predictor=factor(labels, levels=rev(labels)),
        y=fixef(model),
        se=sqrt(diag(vcov(model))))

    map <- c(clusterxr='xr', clusterxs='xs', clustervz='vz', clustervn='vn',
        clustervd='vd', clustersht='\\textrtails t',
        clustershr='\\textrtails r',
        clusterrn='rn', clusterrd='rd', clusterms='ms', 
        stressfinal='Final')

    if (flip) {
        model$y <- -model$y
    }

    firstmap <- c(x='x', m='m', F='stress', v='v', r='r')
    firstmap['\\'] = '\\textrtails'

    model <- mutate(model, latex_cluster=map[as.character(predictor)],
        ymin=y-2*se, ymax=y+2*se,
        first=firstmap[substr(latex_cluster, 1, 1)])

    levels <- c('Final', '\\textrtails t', '\\textrtails r', 'rd', 'rn', 
                'fs', 'vz', 'vd', 'vn', 'xs', 'xr', 'ms', 'k')
    model$latex_cluster <- factor(model$latex_cluster, levels=levels)

    levels <- c('x', '\\textrtails', 'v', 'm', 'r')
    model$first <- factor(model$first, levels=levels)

    # na.omit removes intercept
    model <- na.omit(model)
    p <- ggplot(model, aes(x=latex_cluster, y=y, ymax=ymax, ymin=ymin)) +
        geom_errorbar() + geom_point()
    p <- p + facet_grid(. ~ first, scales='free_x', space='free') +
        scale_x_discrete('Cluster') + 
        scale_y_continuous('Regression coefficient')
    p <- p + geom_hline(aes(yintercept=0), color='red', linetype='dotted')
    return(p)
}

######################
### Generate all plots

generate_plots <- function(d, s=TRUE) {
    b <- 2.3
    output_tikz(d, 'yandex_s_vC_stop_strident', width=5, height=3,
                standAlone=s)
    output_tikz(d, 'yandex_s_vC_stop_sonorant', width=5, height=3,
                standAlone=s)
    output_tikz(d, 'yandex_s_vC_stress', width=5, height=3, standAlone=s)
    output_tikz(d, 'yandex_s_strid', width=5, height=3, standAlone=s)
    output_tikz(d, 'yandex_boxplot_by_first', width=5, height=3, standAlone=s)
    output_tikz(d, 'experiment_overview_stress', width=5, height=3,
                standAlone=s)
    output_tikz(d, 'yandex_s_vC_number', width=2.7, height=4, standAlone=s)
    output_tikz(d, 'yandex_s_vC_case', width=2.7, height=4, standAlone=s)
    output_tikz(d, 's_lemma', width=b + 0.78, height=5, standAlone=s)
    output_tikz(d, 'v_lemma', width=b + 0.85, height=5, standAlone=s)
    output_tikz(d, 'k_monosyll_sonority_prop', width=3.5, height=3,
                standAlone=s)
    output_tikz(d, 'dvor', width=b + 0.7, height=3, standAlone=s)
    output_tikz(d, 'den', width=b + 0.91, height=3, standAlone=s)
    output_tikz(d, 'dno', width=b + 0.73, height=3, standAlone=s)
    output_tikz(d, 'dvor_derived', width=b + 0.97, height=3, standAlone=s)
}
