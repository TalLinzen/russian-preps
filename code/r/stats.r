# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Analysis of the experiment reported in Section 4 in Linzen, Kasyanenko, & 
# Gouskova (2013). (Lexical and phonological variation in Russian prepositions,
# Phonology 30(3).)

# Some of the preprocessing functions here may not be up to date: preprocessed
# data was saved to CSV files and subsequent processing was always performed on
# these CSV files.

library(rjson)
library(Hmisc)
library(plyr)
library(lme4)

options(stringsAsFactors=FALSE)

if ((root <- Sys.getenv('RUSS_PREPS_ROOT')) == "") {
    stop('RUSS_PREPS_ROOT must be set')
}

natural_classes <- c(t='stop', d='stop', x='fricative', f='fricative',
    s='strident', z='strident', sh='strident', v='sonorant', n='sonorant',
    m='sonorant', r='sonorant')
natural_classes_levels <- c('strident', 'stop', 'fricative', 'sonorant')

# Words that were miscoded:
flipped <- c('xsin', 'xsUm', 'kup')
outlying_words <- c('xrix')

# Based on Clemens 1990:
sonority <- c(v=4, f=4, r=3, l=3, m=2, n=2, s=1, t=1, d=1, z=1, sh=1, x=1)

results_dir <- file.path(root, 'results')
completed_dir <- file.path(results_dir, 'completed')
responses_csv <- file.path(results_dir, 'responses.csv')
personal_csv <- file.path(results_dir, 'personal.csv')

load_responses <- function(filename) {
    return(ldply(fromJSON(file=filename)$responses, data.frame))
}

load_personal <- function(filename) {
    decoded <- fromJSON(file=filename)
    df <- data.frame(decoded$personal, stringsAsFactors=FALSE)
    df <- subset(df, select=-feedback)
    df$year <- as.numeric(df$year)
    df <- transform(df, 
        year=ifelse(!is.na(year) & year < 100, year + 1900, year))
    df$age <- 2011 - df$year
    return(df)
}

load_all_responses <- function(directory) {
    return(ldply(list.files(directory, full.names=TRUE), function(filename) {
        transform(load_responses(filename),
            s_better=s_first & choice == 1 | !s_first & choice == 2,
            subject=unlist(strsplit(basename(filename), '\\.'))[1])
    }))
}

load_all_personal <- function(directory) {
    return(ldply(list.files(directory, full.names=TRUE), function(filename) {
        transform(load_personal(filename),
            subject=unlist(strsplit(basename(filename), '\\.'))[1])
    }))
}

dump_all_responses_df <- function() {
    df <- load_all_responses(completed_dir) 
    df <- subset(df, select=-c(choice, s_first)) 
    df$word <- paste(df$cluster, df$rime, sep="") #Added a word column
    write.csv(df, responses_csv, row.names=FALSE)
}

dump_all_personal_df <- function() {
    df <- load_all_personal(completed_dir)
    write.csv(df, personal_csv, row.names=FALSE)
}

pool_responses <- function(responses) {
    return(ddply(responses, c('cluster', 'stress'), function (x) {
        s_better <- length(which(x$s_better))
        so_better <- length(which(!x$s_better))
        return(data.frame(cluster=x[1,'cluster'], 
            stress=x[1,'stress'],
            s_better=s_better,
            so_better=so_better,
            s_ratio=s_better / (s_better + so_better)))
    }))
}

pool_by_item <- function(responses) {
    return(ddply(responses, .(word), function (x) {
        s_better <- length(which(x$s_better))
        so_better <- length(which(!x$s_better))
        return(data.frame(
            word=x[1,'word'], s_ratio=s_better / (s_better + so_better)))
    }))
}

load_processed_responses <- function() {
    df <- read.csv(responses_csv)
    df[df$word %in% flipped, 's_better'] <- 
        !df[df$word %in% flipped, 's_better']
    df$stress <- factor(df$stress, levels=c('initial', 'final'))
    df$subject <- factor(df$subject)
    df <- transform(df, 
        first=ifelse(substr(cluster, 1, 2) == 'sh', 
            'sh', substr(cluster, 1, 1)), 
        second=substr(cluster, nchar(cluster), nchar(cluster)))
    df <- transform(df, first_class=natural_classes[first],
        second_class=natural_classes[second],
        sondiff=sonority[second] - sonority[first])

    u <- unique(df$first)
    df$first  <- factor(df$first, levels=u[order(u)])
    u <- unique(df$second)
    df$second  <- factor(df$second, levels=u[order(u)])

    df <- transform(df, latex_cluster=ifelse(first == 'sh',
        paste('\\textrtails', second), cluster))

    u <- unique(df$cluster)
    df$cluster <- factor(df$cluster, levels=u[order(u)])
    df$latex_cluster <- factor(df$latex_cluster,
        levels=unique(df$latex_cluster)[order(u)])
    df$first_class <- factor(df$first_class, levels=natural_classes_levels)
    df$second_class <- factor(df$second_class, levels=natural_classes_levels)
    return(df)
}

code_region <- function(city_new, is_russia) {
    if (city_new == 'St. Petersburg') {
        return('SPB')
    } else if (city_new == 'Moscow') {
        return('Moscow')
    } else if (is_russia) {
        return('OtherRussia')
    } else {
        return('NotRussia')
    }
}

load_processed_personal <- function() {
    df <- read.csv(file.path(results_dir, 'personal_annotated.csv'))
    df$sex <- factor(df$sex, levels=c('female', 'male'))
    df$region <- mapply(code_region, df$city_new, df$is_russia)
    return(df)
}

responses <- load_processed_responses()
personal <- na.omit(load_processed_personal())
responses <- subset(merge(responses, personal, 'subject'),
    select=-c(is_russia, city, city_new, year))
responses_with_outliers <- responses
responses <- subset(responses, !(word %in% outlying_words))
pooled <- pool_responses(responses)
    
lmes <- function() {
    basic_model <- lmer(s_better ~ stress + cluster + (1|subject),
        family=binomial, data=target_clusters)
    with_item_intercept <- lmer(s_better ~ stress + cluster + (1|subject) +
        (1|word), family=binomial, data=target_clusters)
    print(anova(basic_model, with_item_intercept))

    with_subject_stress_slope <- lmer(s_better ~ stress + cluster + 
        (1+stress|subject),
        family=binomial, data=target_clusters)
    print(anova(basic_model, with_subject_stress_slope))

    with_subject_stress_slope <- lmer(s_better ~ stress + cluster + 
        (1+stress|subject),
        family=binomial, data=target_clusters)

    print(anova(basic_model, with_subject_stress_slope))
    with_interactions <- lmer(s_better ~ stress * cluster + 
        (1+stress|subject),
        family=binomial, data=target_clusters)
    print(anova(with_subject_stress_slope, with_interactions))

    with_region <- lmer(s_better ~ stress + cluster +
        region + (1+stress|subject), family=binomial,
        data=target_clusters)
    print(anova(with_subject_stress_slope, with_region))
    # Not significant (Chisq = 1.87, df = 3)

    with_age <- lmer(s_better ~ stress + cluster +
        age + (1+stress|subject), family=binomial,
        data=target_clusters)
    print(anova(with_subject_stress_slope, with_age))
    # N.s

    with_sex <- lmer(s_better ~ stress + cluster +
        sex + (1+stress|subject), family=binomial,
        data=target_clusters)
    print(anova(with_subject_stress_slope, with_sex))
    # N.s

    # do we really have enough data points to have a by subject 
    # random slope for cluster? doesn't seem to terminate on my laptop
    # with_subject_cluster_slope <- lmer(s_better ~ stress + cluster + 
    #    (1+stress+cluster|subject) + (1|word),
    #    family=binomial, data=target_clusters)
    # print(anova(with_subject_stress_slope, with_subject_stress_slope))
}

lme3 <- function() {
    # This is the best converging LME model
    target_clusters <- subset(responses, cluster != 'k')
    with_subject_stress_slope <- lmer(s_better ~ stress + cluster +
        (1+stress|subject), family=binomial, data=target_clusters)
    return(with_subject_stress_slope)
}

# Adapted from the LMERConvenienceFunctions package to work with GLMs
posthoc.hacked <- function (model, data, factor, prior.releveling = NULL,
    two.tailed = TRUE, num.comp = NULL, ndigits = 4, MCMC = FALSE,
    nsim = 10000, addPlot = FALSE) 
{
    if (!is.null(prior.releveling)) {
        cat("releveling specified factors prior to performing posthoc ...\n")
        for (pr in 1:length(prior.releveling)) {
            data[, prior.releveling[[pr]][1]] <- relevel(data[, 
                prior.releveling[[pr]][1]], prior.releveling[[pr]][2])
            cat("\treference level of factor", prior.releveling[[pr]][1], 
                "set to", prior.releveling[[pr]][2], "...\n")
        }
    }
    levs = sort(levels(data[, factor]))
    posthoc = list()
    cat("performing posthoc ...\n")
    for (lev in levs) {
        cat(paste("\tprocessing level", lev, "of factor", factor, 
            "(", grep(lev, levs), "of", length(levs), ") ...\n"), 
            sep = " ")
        data[, factor] = relevel(data[, factor], lev)
        model = update(model, . ~ ., data = data)
        if (class(model) == "lm") {
            temp <- round(summary(model)$coefficients, ndigits)
            posthoc[[lev]] <- temp
        }
        else {
            temp <- as.data.frame(round(summary(model)@coefs, 
                ndigits))
            dims <- NULL
            rank.X = qr(model@X)$rank
            udf <- nrow(model@frame) - rank.X
            model.ranef <- ranef(model)
            lower.bound <- 0
            for (i in 1:length(names(model.ranef))) {
                dims <- dim(model.ranef[[i]])
                lower.bound <- lower.bound + dims[1] * dims[2]
            }
            ldf <- nrow(model@frame) - rank.X - lower.bound
            multip <- ifelse(two.tailed, 2, 1)
            temp[, "udf"] <- udf
            temp[, "ldf"] <- ldf
            temp[, "up.p.unadj."] <- round(multip * (1 - pt(abs(temp[, 
                "z value"]), udf)), ndigits)
            temp[, "low.p.unadj."] <- round(multip * (1 - pt(abs(temp[, 
                "z value"]), ldf)), ndigits)
            if (!is.null(num.comp)) {
                temp[, "up.p.adj."] <- round(num.comp * (multip * 
                  (1 - pt(abs(temp[, "t value"]), udf))), ndigits)
                temp[, "low.p.adj."] <- round(num.comp * (multip * 
                  (1 - pt(abs(temp[, "t value"]), ldf))), ndigits)
            }
            posthoc[[lev]] <- temp
            if (MCMC) {
                stop("MCMC not implemented\n")
            }
        }
    }
    cat("\n")
    return(posthoc)
}

