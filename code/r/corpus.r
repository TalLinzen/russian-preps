# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

library(plyr)
library(boot)

if ((root <- Sys.getenv('RUSS_PREPS_ROOT')) == "") {
    stop('RUSS_PREPS_ROOT must be set')
}

sonority <- c(p=0, t=0, k=0, b=1, d=1, g=1, s=2, f=2, sh=2, shch=2, ts=2, ch=2,
    x=2, z=3, v=3, zh=3, m=4, n=4, l=5, r=5)  

natural_classes <- c(p='voiceless_stop', t='voiceless_stop', 
    k='voiceless_stop',
    b='voiced_stop', d='voiced_stop', g='voiced_stop', 
    s='strident', f='voiceless_fricative', sh='strident', shch='strident',
    ts='strident', ch='strident',
    x='voiceless_fricative', z='strident', v='glide',
    zh='strident', m='nasal', n='nasal', l='glide', r='glide')  

streamlined_classes <- c(glide='sonorant', nasal='sonorant',
    voiced_stop='obstruent', voiceless_stop='obstruent', strident='strident',
    voiceless_fricative='obstruent')

sonority_classes <- c(voiceless_stop=0, voiced_stop=1,
    voiceless_fricative=2, strident=3, voiced_fricative=3, nasal=4, glide=5)

# Latin to Russian
l2r <- c(p='п', t='т', k='к', b='б', d='д', g='г', s='с', f='ф', sh='ш', 
    shshj='щ', ts='ц', ch='ч', x='х', z='з', v='в', zh='ж', m='м', n='н',
    l='л', r='р')  
r2l <- names(l2r)
names(r2l) <- l2r
r2l <- c(r2l, ъ='!', ь='\'')
r2l_latex <- r2l
r2l_latex['ь'] <- '\\textsuperscript{j}'
r2l_latex['ц'] <- '\\texttoptiebar{ts}'
r2l_latex['ч'] <- '\\texttoptiebar{t\\textesh}'
r2l_latex['ш'] <- '\\textrtails '
r2l_latex['ж'] <- '\\textrtailz '

r2l_latex_string <- function(inp) {
    return(sapply(lapply(strsplit(inp, NULL), 
        function(y) r2l_latex[y]), paste, collapse=""))
}

first_characters <- function(word) {
    chars <- sapply(1:3, function(x) r2l[substr(word, x, x)])
    if (nchar(word) == 1) {
        return(c(chars[1], NULL))
    }
    if (chars[2] == "'") {
        return(c(sprintf('%sj', chars[1]), chars[3]))
    }
    else {
        return(c(chars[1], chars[2]))
    }
}

load_csv <- function(filename) {
    full_filename <- file.path(root, sprintf('results/corpus/%s', filename))
    vec <- read.csv(full_filename, stringsAsFactors=FALSE)
    for (i in 1:nrow(vec)) {
        first_chars <- first_characters(vec[i, 'full_cluster'])
        vec[i, 'first'] <- first_chars[1]
        vec[i, 'second'] <- first_chars[2]
    }
    vec <- transform(vec,
        first_transcribed=as.character(r2l_latex[substr(full_cluster, 1, 1)]),
        noep_prop=noep/(noep+ep),
        cluster_size=nchar(full_cluster),
        length=nchar(gsub('[\'`]', '', form)),
        nvowels=nchar(gsub('[^яюаеиоыуэё]', '', form)),
        first_stressed=factor(vec$stressed_syll == 1, c(TRUE, FALSE)))

    vec <- transform(vec, 
        second_transcribed=as.character(r2l_latex[substr(full_cluster, 2, 2)]),
        last=as.character(r2l[substr(full_cluster, cluster_size, 
            cluster_size)]))

    vec$transcribed <- r2l_latex_string(vec$full_cluster)

    vec <- transform(vec,
        first_sonority=sonority_classes[natural_classes[first]],
        last_sonority=sonority_classes[natural_classes[last]],
        cluster_size=factor(cluster_size))

    vec$last_class <- factor(natural_classes[as.character(vec$last)],
        levels=names(sonority_classes))
    vec$second_class <- natural_classes[as.character(vec$second)]
    vec$second_class_simple <- factor(streamlined_classes[vec$second_class],
        levels=c('obstruent', 'strident', 'sonorant'))
    vec$first_class <- factor(natural_classes[as.character(vec$first)],
        levels=names(sonority_classes))

    return(vec)
}

load_all <- function(only_outliers=F, pruned=F) {
    suffix <- ifelse(pruned, 'pruned', 'unpruned')
    s <- load_csv(sprintf('s_rnc_%s.csv', suffix))
    s$interesting <- s$first_class != 'strident'
    s$prep <- 's'
    v <- load_csv(sprintf('v_rnc_%s.csv', suffix))
    v$interesting <- !(v$first %in% c('f', 'v'))
    v$prep <- 'v'
    k <- load_csv(sprintf('k_rnc_%s.csv', suffix))
    k$interesting <- TRUE
    k$prep <- 'k'

    all <- rbind(s, v, k) 
    all$tokens <- all$ep + all$noep
    all$prop <- all$ep / all$tokens
    all <- ddply(all, .(lemma, case, number), function(row) {
        b <- binom.test(c(row[1, 'ep'], row[1, 'noep']), p=0)
        row[1, 'cilower'] <- b$conf.int[1]
        row[1, 'ciupper'] <- b$conf.int[2]
        row
    })
    if (only_outliers) {
        all <- all[all$prop > 0.25 & all$cilower > 0.2 & !is.na(all$cilower) &
            all$interesting,]
    }
    all <- all[order(all$lemma, all$case),]
    all <- subset(all, first != second)   # Only actual clusters, no vv
    return(all)
}

####
# Code for the analysis reported in Section 3.3

load_regression_data <- function() {
    data <- load_csv('s_yandex.csv')
    data <- subset(data, first == 'v' & second != 'v' & second != "'")
    data <- subset(data, second_class != 'voiced_stop' &
        second_class != 'voiceless_fricative')
    data <- transform(data, second_class_simple=factor(second_class_simple),
                      cluster_size=as.numeric(cluster_size),
        second_class=factor(second_class),
        first_stressed=factor(first_stressed),
        length=nchar(sub('\'', '', form)),
        case=factor(case),
        number=factor(number),
        logfreq=log(1 + lemma_freq))
    return(data)
}

bootstrap_vinitial_coefs <- function(frml, data, niter=1000) {
    fitone <- function(data, indices) {
        d <- data[indices,]
        model <- glm(frml, family=binomial, data=d)
        return(coef(model))
    }

    bootresults <- boot(data=data, statistic=fitone, R=niter)
    return(bootresults)
}

calculate_cis <- function(bootresults) {
    df <- ldply(c(1:length(bootresults$t0)), function (x) {
        bci <- boot.ci(bootresults, type='bca', index=x)
        data.frame(ymin=bci$bca[4], ymax=bci$bca[5])
    })
    df$sd <- apply(bootresults$t, 2, sd)
    df$y <- bootresults$t0
    df$t <- df$y / df$sd
    df$name <- c(names(bootresults$t0))
    return(df)
}

full_formula <- cbind(noep, ep) ~ second_class_simple + first_stressed + 
    cluster_size + nvowels + logfreq + case + number
nostress <- cbind(noep, ep) ~ second_class_simple +
   cluster_size + nvowels + logfreq + case + number
nolength <- cbind(noep, ep) ~ second_class_simple + stressed_syll +
   cluster_size + first_stressed + logfreq + case + number
