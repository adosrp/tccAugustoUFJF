# ------------------------------------------------------------------------------
# Carregando pacotes e definindo diretório de trabalho -------------------------

library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)

setwd("../dados/")

# ------------------------------------------------------------------------------
# Multiple plot function -------------------------------------------------------
# Fonte: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) {
        print(plots[[1]])
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

# ------------------------------------------------------------------------------
# Grid arrange shared legend function ------------------------------------------
# Fonte: http://stackoverflow.com/a/38420690 -----------------------------------

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)),
                                       position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, nrow = nrow, ncol = ncol)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
    
}

# ------------------------------------------------------------------------------
# Carregando Dados -------------------------------------------------------------

dados <- list.files()
db <- lapply(dados, function (x) read.xlsx(x, sheetIndex = 1, header = TRUE))

p2 <- db[unlist(lapply(db, function (x) any(names(x) %in% "p2")))]
p2Cost <- lapply(p2, function (x) x[names(x) %in% c("CM", "p2")])
p2Cost <- do.call(rbind.data.frame, p2Cost)
p2Cost$id <- as.factor(sort(rep(1:5, 16)))

pe <- db[unlist(lapply(db, function (x) any(names(x) %in% "pe")))]
peCost <- lapply(pe, function (x) x[names(x) %in% c("CM", "pe")])
peCost <- do.call(rbind.data.frame, peCost)
peCost$id <- as.factor(sort(rep(1:5, 26)))

alfa <- db[unlist(lapply(db, function (x) any(names(x) %in% "alfa")))]
alfaCost <- lapply(alfa, function (x) x[names(x) %in% c("CM", "alfa")])
alfaCost <- do.call(rbind.data.frame, alfaCost)
alfaCost$id <- as.factor(sort(rep(1:5, 16)))

p2Errors <- db[unlist(lapply(db, function (x) any(names(x) %in% "p2errors")))]
p2Errors <- do.call(rbind.data.frame, p2Errors)
p2Errors$id <- as.factor(sort(rep(1:2, 16)))

peErrors <- db[unlist(lapply(db, function (x) any(names(x) %in% "peerrors")))]
peErrors <- do.call(rbind.data.frame, peErrors)
peErrors$id <- as.factor(sort(rep(1:2, 26)))

alfaErrors <- db[unlist(lapply(db, function (x) any(names(x) %in% "alfaerrors")))]
alfaErrors <- do.call(rbind.data.frame, alfaErrors)
alfaErrors$id <- as.factor(sort(rep(1:2, 16)))

mLFatorial <- db[unlist(lapply(db, function (x) all(any(names(x) %in% "L") && any(names(x) %in% "c_i"))))]
mLFatorial <- do.call(rbind.data.frame, mLFatorial)
mLFatorial$id <- as.factor(c(rep("5%",9),rep("10%",9),rep("15%",9)))

mnFatorial <- db[unlist(lapply(db, function (x) all(any(names(x) %in% "n") && any(names(x) %in% "c_i"))))]
mnFatorial <- do.call(rbind.data.frame, mnFatorial)
mnFatorial$id <- as.factor(c(rep("5%",9),rep("10%",9),rep("15%",9)))

# ------------------------------------------------------------------------------
# Gráfico: Custos vs p2 --------------------------------------------------------

p2CostFig <- ggplot(data = p2Cost, aes(x = p2, y = CM, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(p[2]), y = "Custo Ótimo") +
    scale_shape_manual(values = c(0,1,16,17,3),
                       name = "Modelo:", 
                       breaks = c("1","2","3","4","5"),
                       labels = c("Taguchi", "Nayebpour", "m", "mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("p2CostFig.png", plot = p2CostFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Custos vs pe --------------------------------------------------------

peCostFig <- ggplot(data = peCost, aes(x = pe, y = CM, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(pi), y = "Custo Ótimo") +
    scale_shape_manual(values = c(0,1,16,17,3),
                       name = "Modelo:", 
                       breaks = c("1","2","3","4","5"),
                       labels = c("Taguchi", "Nayebpour", "m", "mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("peCostFig.png", plot = peCostFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Custos vs alfa ------------------------------------------------------

alfaCostFig <- ggplot(data = alfaCost, aes(x = alfa, y = CM, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "Custo Ótimo") +
    scale_shape_manual(values = c(0,1,16,17,3),
                       name = "Modelo:", 
                       breaks = c("1","2","3","4","5"),
                       labels = c("Taguchi", "Nayebpour", "m", "mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("alfaCostFig.png", plot = alfaCostFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# ------------------------------------------------------------------------------
# Gráfico: p2 modelo mL --------------------------------------------------------

p2mL <- p2[[4]]
mp2mLFig <- ggplot(data = p2mL, aes(x = p2, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(p[2]), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
Lp2mLFig <- ggplot(data = p2mL, aes(x = p2, y = L)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(p[2]), y = "Lº") +
    theme(legend.key = element_rect(fill=NA))
png("p2mLFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(mp2mLFig, Lp2mLFig, cols = 2)
dev.off()

# Gráfico: pe modelo mL --------------------------------------------------------

pemL <- pe[[4]]
mpemLFig <- ggplot(data = pemL, aes(x = pe, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(pi), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
LpemLFig <- ggplot(data = pemL, aes(x = pe, y = L)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(pi), y = "Lº") +
    theme(legend.key = element_rect(fill=NA))
png("pemLFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(mpemLFig, LpemLFig, cols = 2)
dev.off()

# Gráfico: alfa modelo mL ------------------------------------------------------

alfamL <- alfa[[4]]
malfamLFig <- ggplot(data = alfamL, aes(x = alfa, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
LalfamLFig <- ggplot(data = alfamL, aes(x = alfa, y = L)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "Lº") +
    theme(legend.key = element_rect(fill=NA))
png("alfamLFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(malfamLFig, LalfamLFig, cols = 2)
dev.off()

# ------------------------------------------------------------------------------
# Gráfico: p2 modelo mn --------------------------------------------------------

p2mn <- p2[[5]]
mp2mnFig <- ggplot(data = p2mn, aes(x = p2, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(p[2]), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
np2mnFig <- ggplot(data = p2mn, aes(x = p2, y = n)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    scale_y_continuous(limits = c(1,4), breaks = c(1:4)) +
    labs(x = expression(p[2]), y = "nº") +
    theme(legend.key = element_rect(fill=NA))
png("p2mnFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(mp2mnFig, np2mnFig, cols = 2)
dev.off()

# Gráfico: pe modelo mn --------------------------------------------------------

pemn <- pe[[5]]
mpemnFig <- ggplot(data = pemn, aes(x = pe, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(pi), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
npemnFig <- ggplot(data = pemn, aes(x = pe, y = n)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    scale_y_continuous(limits = c(1,5), breaks = 1:5) +
    labs(x = expression(pi), y = "nº") +
    theme(legend.key = element_rect(fill=NA))
png("pemnFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(mpemnFig, npemnFig, cols = 2)
dev.off()

# Gráfico: alfa modelo mn ------------------------------------------------------

alfamn <- alfa[[5]]
malfamnFig <- ggplot(data = alfamn, aes(x = alfa, y = m)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "mº") +
    theme(legend.key = element_rect(fill=NA))
nalfamnFig <- ggplot(data = alfamn, aes(x = alfa, y = n)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    scale_y_continuous(limits = c(1,10), breaks = 1:10) +
    labs(x = substitute(alpha*' = '*beta), y = "nº") +
    theme(legend.key = element_rect(fill=NA))
png("alfamnFig.png", width = 12, height = 6, units = "cm", res = 300)
multiplot(malfamnFig, nalfamnFig, cols = 2)
dev.off()

# ------------------------------------------------------------------------------
# Gráfico: Fatorial mL ---------------------------------------------------------

mL05 <- mLFatorial[mLFatorial$id == "5%",]
mL10 <- mLFatorial[mLFatorial$id == "10%",]
mL15 <- mLFatorial[mLFatorial$id == "15%",]

mn05 <- mnFatorial[mnFatorial$id == "5%",]
mn10 <- mnFatorial[mnFatorial$id == "10%",]
mn15 <- mnFatorial[mnFatorial$id == "15%",]

multi.fun <- function(x) {
    mc_i <- c(mean(x$CM[x$c_i == min(x$c_i)]), 
               mean(x$CM[x$c_i == median(x$c_i)]), 
               mean(x$CM[x$c_i == max(x$c_i)]))
    c_i <- c(min(x$c_i), median(x$c_i), max(x$c_i))
    mc_nc <- c(mean(x$CM[x$c_nc == min(x$c_nc)]), 
                mean(x$CM[x$c_nc == median(x$c_nc)]), 
                mean(x$CM[x$c_nc == max(x$c_nc)]))
    c_nc <- c(min(x$c_nc), median(x$c_nc), max(x$c_nc))
    mc_a <- c(mean(x$CM[x$c_a == min(x$c_a)]), 
               mean(x$CM[x$c_a == median(x$c_a)]), 
               mean(x$CM[x$c_a == max(x$c_a)]))
    c_a <- c(min(x$c_a), median(x$c_a), max(x$c_a))
    mc_d <- c(mean(x$CM[x$c_d == min(x$c_d)]), 
               mean(x$CM[x$c_d == median(x$c_d)]), 
               mean(x$CM[x$c_d == max(x$c_d)]))
    c_d <- c(min(x$c_d), median(x$c_d), max(x$c_d))
    result <- cbind(c_i,mc_i,c_nc,mc_nc,c_a,mc_a,c_d,mc_d)
    
    return(result)
}

dataFat <- lapply(list(mL05,mL10,mL15,mn05,mn10,mn15), function(x) multi.fun(x))

# Gráfico: Fatorial 5% mL ------------------------------------------------------

f051 <- ggplot(data = as.data.frame(dataFat[[1]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.05)") +
    scale_x_continuous(breaks = c(min(mL05$c_i), median(mL05$c_i), max(mL05$c_i))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f052 <- ggplot(data = as.data.frame(dataFat[[1]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mL05$c_nc), median(mL05$c_nc), max(mL05$c_nc))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f053 <- ggplot(data = as.data.frame(dataFat[[1]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mL05$c_a), median(mL05$c_a), max(mL05$c_a))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f054 <- ggplot(data = as.data.frame(dataFat[[1]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mL05$c_d), median(mL05$c_d), max(mL05$c_d))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

# Gráfico: Fatorial 10% mL -----------------------------------------------------

f101 <- ggplot(data = as.data.frame(dataFat[[2]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.10)") +
    scale_x_continuous(breaks = c(min(mL10$c_i), median(mL10$c_i), max(mL10$c_i))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f102 <- ggplot(data = as.data.frame(dataFat[[2]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mL10$c_nc), median(mL10$c_nc), max(mL10$c_nc))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f103 <- ggplot(data = as.data.frame(dataFat[[2]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mL10$c_a), median(mL10$c_a), max(mL10$c_a))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f104 <- ggplot(data = as.data.frame(dataFat[[2]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mL10$c_d), median(mL10$c_d), max(mL10$c_d))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

# Gráfico: Fatorial 15% mL -----------------------------------------------------

f151 <- ggplot(data = as.data.frame(dataFat[[3]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.15)") +
    scale_x_continuous(breaks = c(min(mL15$c_i), median(mL15$c_i), max(mL15$c_i))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f152 <- ggplot(data = as.data.frame(dataFat[[3]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mL15$c_nc), median(mL15$c_nc), max(mL15$c_nc))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f153 <- ggplot(data = as.data.frame(dataFat[[3]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mL15$c_a), median(mL15$c_a), max(mL15$c_a))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

f154 <- ggplot(data = as.data.frame(dataFat[[3]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mL15$c_d), median(mL15$c_d), max(mL15$c_d))) +
    scale_y_continuous(limits = c(min(mLFatorial$CM), max(mLFatorial$CM)))

png("fat_mL.png", width = 18, height = 15, units = "cm", res = 300)
multiplot(f051, f101, f151,
          f052, f102, f152,
          f053, f103, f153,
          f054, f104, f154,
          cols = 4)
dev.off()

# Gráfico: Fatorial 5% mn ------------------------------------------------------

f051 <- ggplot(data = as.data.frame(dataFat[[4]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.05)") +
    scale_x_continuous(breaks = c(min(mn05$c_i), median(mn05$c_i), max(mn05$c_i))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f052 <- ggplot(data = as.data.frame(dataFat[[4]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mn05$c_nc), median(mn05$c_nc), max(mn05$c_nc))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f053 <- ggplot(data = as.data.frame(dataFat[[4]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mn05$c_a), median(mn05$c_a), max(mn05$c_a))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f054 <- ggplot(data = as.data.frame(dataFat[[4]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mn05$c_d), median(mn05$c_d), max(mn05$c_d))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

# Gráfico: Fatorial 10% mn -----------------------------------------------------

f101 <- ggplot(data = as.data.frame(dataFat[[5]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.10)") +
    scale_x_continuous(breaks = c(min(mn10$c_i), median(mn10$c_i), max(mn10$c_i))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f102 <- ggplot(data = as.data.frame(dataFat[[5]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mn10$c_nc), median(mn10$c_nc), max(mn10$c_nc))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f103 <- ggplot(data = as.data.frame(dataFat[[5]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mn10$c_a), median(mn10$c_a), max(mn10$c_a))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f104 <- ggplot(data = as.data.frame(dataFat[[5]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mn10$c_d), median(mn10$c_d), max(mn10$c_d))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

# Gráfico: Fatorial 15% mn -----------------------------------------------------

f151 <- ggplot(data = as.data.frame(dataFat[[6]]), aes(x = c_i, y = mc_i)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[insp]), y = "Média dos custos (k = 0.15)") +
    scale_x_continuous(breaks = c(min(mn15$c_i), median(mn15$c_i), max(mn15$c_i))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f152 <- ggplot(data = as.data.frame(dataFat[[6]]), aes(x = c_nc, y = mc_nc)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[nc]), y = "") +
    scale_x_continuous(breaks = c(min(mn15$c_nc), median(mn15$c_nc), max(mn15$c_nc))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f153 <- ggplot(data = as.data.frame(dataFat[[6]]), aes(x = c_a, y = mc_a)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = expression(c[a]), y = "") +
    scale_x_continuous(breaks = c(min(mn15$c_a), median(mn15$c_a), max(mn15$c_a))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

f154 <- ggplot(data = as.data.frame(dataFat[[6]]), aes(x = c_d, y = mc_d)) +
    theme_grey(base_size = 7) +
    geom_line(size = 0.2) +
    geom_point(size = 1) +
    labs(x = substitute(c[sc]* ' = ' *c[snc]), y = "") +
    scale_x_continuous(breaks = c(min(mn15$c_d), median(mn15$c_d), max(mn15$c_d))) +
    scale_y_continuous(limits = c(min(mnFatorial$CM), max(mnFatorial$CM)))

png("fat_mn.png", width = 18, height = 15, units = "cm", res = 300)
multiplot(f051, f101, f151,
          f052, f102, f152,
          f053, f103, f153,
          f054, f104, f154,
          cols = 4)
dev.off()

# ------------------------------------------------------------------------------
# Gráfico: Erro Tipo I vs p2 ---------------------------------------------------

p2typeIFig <- ggplot(data = p2Errors, 
                      aes(x = p2errors, y = typeI, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(p[2]), y = "Erro Tipo I") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("p2typeIFig.png", plot = p2typeIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Erro Tipo II vs p2 --------------------------------------------------

p2typeIIFig <- ggplot(data = p2Errors, 
                     aes(x = p2errors, y = typeII, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(p[2]), y = "Erro Tipo II") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("p2typeIIFig.png", plot = p2typeIIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Não conformidade vs p2 ----------------------------------------------
    
p2EncFig <- ggplot(data = p2Errors, 
                      aes(x = p2errors, y = enc, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(p[2]), y = "Fração de não-conformidade") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    scale_y_continuous(limits = c(0.0045,0.0055), breaks = seq(0.0045,0.0055, by = 0.0005)) +
    theme(legend.key = element_rect(fill=NA))
ggsave("p2EncFig.png", plot = p2EncFig, width = 12, height = 6,
       units = "cm", dpi = 300)

png("p2EstFig.png", width = 18, height = 6, units = "cm", res = 300)
grid_arrange_shared_legend(p2typeIFig, p2typeIIFig, p2EncFig, ncol = 3, nrow = 1)
dev.off()

# Gráfico: Erro Tipo I vs pe ---------------------------------------------------

petypeIFig <- ggplot(data = peErrors, 
                     aes(x = peerrors, y = typeI, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(pi), y = "Erro Tipo I") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("petypeIFig.png", plot = petypeIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Erro Tipo II vs pe --------------------------------------------------

petypeIIFig <- ggplot(data = peErrors, 
                      aes(x = peerrors, y = typeII, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(pi), y = "Erro Tipo II") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("petypeIIFig.png", plot = petypeIIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Não conformidade vs pe ----------------------------------------------

peEncFig <- ggplot(data = peErrors, 
                   aes(x = peerrors, y = enc, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = expression(pi), y = "Fração de não-conformidade") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("peEncFig.png", plot = peEncFig, width = 12, height = 6,
       units = "cm", dpi = 300)

png("peEstFig.png", width = 18, height = 6, units = "cm", res = 300)
grid_arrange_shared_legend(petypeIFig, petypeIIFig, peEncFig, ncol = 3, nrow = 1)
dev.off()

# Gráfico: Erro Tipo I vs alfa -------------------------------------------------

alfatypeIFig <- ggplot(data = alfaErrors, 
                     aes(x = alfaerrors, y = typeI, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "Erro Tipo I") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("alfatypeIFig.png", plot = alfatypeIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Erro Tipo II vs alfa ------------------------------------------------

alfatypeIIFig <- ggplot(data = alfaErrors, 
                      aes(x = alfaerrors, y = typeII, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "Erro Tipo II") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("alfatypeIIFig.png", plot = alfatypeIIFig, width = 12, height = 6,
       units = "cm", dpi = 300)

# Gráfico: Não conformidade vs alfa --------------------------------------------

alfaEncFig <- ggplot(data = alfaErrors, 
                   aes(x = alfaerrors, y = enc, group = id)) + 
    theme_grey(base_size = 8) +
    geom_line(size = 0.2) +
    geom_point(aes(shape = id), size = 1) +
    labs(x = substitute(alpha*' = '*beta), y = "Fração de não-conformidade") +
    scale_shape_manual(values = c(17,3),
                       name = "Modelo:", 
                       breaks = c("1","2"),
                       labels = c("mL", "mn")) +
    theme(legend.key = element_rect(fill=NA))
ggsave("alfaEncFig.png", plot = alfaEncFig, width = 12, height = 6,
       units = "cm", dpi = 300)

png("alfaEstFig.png", width = 18, height = 6, units = "cm", res = 300)
grid_arrange_shared_legend(alfatypeIFig, alfatypeIIFig, alfaEncFig, 
                           ncol = 3, nrow = 1)
dev.off()
       