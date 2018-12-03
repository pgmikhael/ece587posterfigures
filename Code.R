#-------------------------------------------------#
#                    MIC Project                  #
#-------------------------------------------------#

# User-defined function for rendering plots (PGM)
theme_Publication1 <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)+
     theme_classic() +
     theme(  plot.margin=unit(c(10,10,5,10),"mm"),
             plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
             # panel.border = element_rect(colour = NA)
             text = element_text(),
             axis.title = element_text(face = "bold",size = rel(2)),
             axis.title.y = element_text(angle=0,vjust =0.5, margin=margin(0,0,0,0,"mm")),
             axis.title.x = element_text(vjust = 0, margin=margin(0,0,0,0,"mm")),
             axis.text = element_blank(), # element_text(angle = 0, hjust = 0.5, size = rel(1.75)),
             axis.line = element_line(colour="black", size = rel(2)),
             axis.ticks = element_line(size = 0),
             axis.ticks.length = unit(0.5, "cm"),
             # panel.grid.major = element_line(colour="#f0f0f0"),
             # panel.grid.minor = element_blank(),
             legend.key = element_rect(colour = NA),
             legend.position = "right",
             legend.direction = "vertical",
             legend.text = element_text(size = rel(1.75)),
             legend.key.size= unit(0.2, "cm"),
             legend.margin = margin(0,0,10,0, "mm"),
             legend.title = element_blank(),
             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
             strip.text = element_text(face="bold")
     ))
}
theme_Publication2 <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)+
     theme_classic() +
     theme(  plot.margin=unit(c(10,10,5,10),"mm"),
             plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
             # panel.border = element_rect(colour = NA)
             text = element_text(),
             axis.title = element_text(face = "bold",size = rel(2)),
             axis.title.y = element_text(angle=90,vjust =0.5, margin=margin(0,0,0,0,"mm")),
             axis.title.x = element_text(vjust = 0, margin=margin(0,0,0,0,"mm")),
             axis.text = element_blank(), # element_text(angle = 0, hjust = 0.5, size = rel(1.75)),
             axis.line = element_line(colour="black", size = rel(2)),
             axis.ticks = element_line(size = 0),
             axis.ticks.length = unit(0.5, "cm"),
             # panel.grid.major = element_line(colour="#f0f0f0"),
             # panel.grid.minor = element_blank(),
             legend.key = element_rect(colour = NA),
             legend.position = "right",
             legend.direction = "vertical",
             legend.text = element_text(size = rel(1.75)),
             legend.key.size= unit(0.2, "cm"),
             legend.margin = margin(1,1,1,1, "mm"),
             legend.box.background = element_rect(colour = "#000000"),
             legend.title = element_blank(),
             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
             strip.text = element_text(face="bold")
     ))
}

# Color blind - friendly colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Figure 1: MIC Algorithm Schematic -----------------------------------------------------------------------------------------------------------------------

x <-  seq(0,2*pi,0.01)        # equally spaced points
n <- rnorm(length(x), 0, 0.5) # gaussian noise
y <- sin(x)+n                 # y a function of x with noise

griding <- data.frame(matrix(ncol = 2, nrow = length(x)))
griding[,1] <- x
griding[,2] <- y

c <- c(-0.1,runif(2,1,6),2*pi+0.3)
r <- c(min(y)-0.2, runif(3,min(y)+0.5,max(y)-0.1), max(y)+0.3)

fig <- ggplot(griding, aes(x = x, y = y)) + geom_point() + xlab("X") + ylab("Y") +
  geom_vline(xintercept = c, linetype = 2, color = "blue", size = rel(1.2)) +
  geom_hline(yintercept = r, linetype = 2, color = "blue", size = rel(1.2))

fig1a <-  fig+theme_Publication1()
ggsave(filename = "fig1a.png", plot = fig1a, width = 12, height = 10, dpi = 300, units = "cm")

fig1b <- ggplot() + geom_vline(xintercept = c, linetype = 2, color = "blue", size = rel(1.2)) + xlab("X") + ylab("Y") +
            geom_hline(yintercept = r, linetype = 2, color = "blue", size = rel(1.2))+theme_Publication1()
ggsave(filename = "fig1b.png", plot = fig1b, width = 12, height = 10, dpi = 300, units = "cm")


# Figure 2: MIC Examples ----------------------------------------------------------------------------------------------------------------------------------

# Figures with y=x, y=c

x <- c(0.2, 0.9)
y <- x
c <- c(-0.1,0.4,1)
r <- c(0.05,0.8,1.2)
eg1 <- data.frame(matrix(nrow = 2, ncol = 2))
eg1[,1] <- x
eg1[,2] <- y

fig2a <- ggplot(eg1, aes(x = x, y = y)) + geom_point(size=3) + xlab("X") + ylab("Y") +
  geom_vline(xintercept = c, linetype = 2, color = "blue", size = rel(1.2)) +
  geom_hline(yintercept = r, linetype = 2, color = "blue", size = rel(1.2)) +theme_Publication1()
ggsave(filename = "fig2a.jpg", plot = fig2a, width = 12, height = 10, dpi = 300, units = "cm")

x <- seq(0,1,length.out = 5)
y <- rep(0.65, 5)
c <- c(-0.1,0.2,0.4,0.6,0.7,1.1)
r <- c(0.05,0.33,0.8,0.92,1.2)
eg2 <- data.frame(matrix(nrow = 5, ncol = 2))
eg2[,1] <- x
eg2[,2] <- y

fig2b <- ggplot(eg2, aes(x = x, y = y)) + geom_point(size=3) + xlab("X") + ylab("Y") +
  geom_vline(xintercept = c, linetype = 2, color = "blue", size = rel(1.2)) +
  geom_hline(yintercept = r, linetype = 2, color = "blue", size = rel(1.2)) +theme_Publication1()
ggsave(filename = "fig2b.jpg", plot = fig2b, width = 12, height = 10, dpi = 300, units = "cm")

# Figure 3: Comparisons to other measures of dependence/Equitability --------------------------------------------------------------------------------------

library(minerva)  # MIC package
library(FNN)      # Mutual Information Estimate package

sigmoid <-  function(x){
  return(1/(1+exp(-x)))
}

circ <- function(x){
  p <- rbinom(length(x), 1, 0.5)
  y <- p*sqrt(1-x^2)-(1-p)*sqrt(1-x^2)
  
  return(y)
}

broadcast <- function(x, M){
  N <- matrix(ncol = ncol(M), nrow = nrow(M))
  for (i in 1:ncol(M)) {
    N[,i] <- x + M[,i]
  }
  return(N)
}


N <- 1000 # number of data points
L <- 250  # noise levels
# 250 noise levels:

U <- seq(0,1, length.out = L) # range parameter for uniform noise level
V <- seq(0,1, length.out = L) # variance paramater for gaussian noise level 

U_Noise <- matrix(nrow = N, ncol = L)
for(i in 1:250){
  U_Noise[,i] <-runif(N,-U[i],U[i])
}

G_Noise <- matrix(nrow = N, ncol = L)
for(i in 1:250){
  G_Noise[,i] <-rnorm(N, 0, V[i])
}  

# functions: 
  # (1) y= x                [0,1]
  # (2) y= x^2              [-2,2]
  # (3) y= sin(x)           [0,2pi]
  # (4) y= sigmoid(x)       [-6,6]
  # (5) y= circ(x)          [-1,1]

# Dataframes for each (1-5) relationship: 1000 datapoints per noise level, 250 noise levels

linear <- data.frame(matrix(nrow = N, ncol = (L+1) ))
colnames(linear) <- c("X", paste("Y", seq(1,L,1), sep = ""))
linear$X <- seq(0,1,length.out = N)
linear[,2:(L+1)] <- broadcast(linear$X,U_Noise)

quad <- data.frame(matrix(nrow = N, ncol = (L+1) ))
colnames(quad) <- c("X", paste("Y", seq(1,L,1), sep = ""))
quad$X <- seq(-2,2,length.out = N)
quad[,2:(L+1)] <- broadcast((quad$X)^2,U_Noise)

sine <- data.frame(matrix(nrow = N, ncol = (L+1) ))
colnames(sine) <- c("X", paste("Y", seq(1,L,1), sep = ""))
sine$X <- seq(0,2*pi,length.out = N)
sine[,2:(L+1)] <- broadcast(sin(sine$X),U_Noise)

sig <-data.frame(matrix(nrow = N, ncol = (L+1) ))
colnames(sig) <- c("X", paste("Y", seq(1,L,1), sep = ""))
sig$X <- seq(-6,6,length.out = N)
sig[,2:(L+1)] <- broadcast(sigmoid(sig$X),U_Noise)

circle <-data.frame(matrix(nrow = N, ncol = (L+1) ))
colnames(circle) <- c("X", paste("Y", seq(1,L,1), sep = ""))
circle$X <- seq(-1,1,length.out = N)
circle[,2:(L+1)] <- broadcast(circ(circle$X),U_Noise)


# plot data at different noise levels
plot(linear$X, linear$Y25)
plot(quad$X, quad$Y25)
plot(sine$X, sine$Y25)
plot(sig$X, sig$Y250)
plot(circle$X, circle$Y250)

# Measure of noise
# 1-R-squared value for the model f(x)+noise~f(x) is used here as a measure of noise
RSquared <- data.frame(matrix(ncol = 5, nrow = L))
colnames(RSquared) <- c("Linear", "Quadratic", "Sine", "Sigmoidal", "Circle")

for (i in 2:(L+1)) {
  RSquared[i-1,1] <- summary(lm(linear[,i]~linear$Y1))$r.squared
  RSquared[i-1,2] <- summary(lm(quad[,i]~quad$Y1))$r.squared
  RSquared[i-1,3] <- summary(lm(sine[,i]~sine$Y1))$r.squared
  RSquared[i-1,4] <- summary(lm(sig[,i]~sig$Y1))$r.squared
  RSquared[i-1,5] <- summary(lm(circle[,i]~circle$Y1))$r.squared
}

# Measures of Dependence
SpearmanCor <- data.frame(matrix(ncol = 5, nrow = L))
colnames(SpearmanCor) <- c("Linear", "Quadratic", "Sine", "Sigmoidal", "Circle")

MICoef <- data.frame(matrix(ncol = 5, nrow = L))
colnames(MICoef) <- c("Linear", "Quadratic", "Sine", "Sigmoidal", "Circle")

MIKraskov <- data.frame(matrix(ncol = 5, nrow = L))
colnames(MIKraskov) <- c("Linear", "Quadratic", "Sine", "Sigmoidal", "Circle")

MIKraskovL <- data.frame(matrix(ncol = 5, nrow = L))
colnames(MIKraskovL) <- c("Linear", "Quadratic", "Sine", "Sigmoidal", "Circle")

# Squared Spearman Correlation Coefficient
for(i in 2:(L+1)){
  SpearmanCor[i-1,] <- c( (cor.test(x = linear$X, y = linear[,i], method = "spearman")$estimate)^2,
                          (cor.test(x = quad$X, y = quad[,i], method = "spearman")$estimate)^2,
                          (cor.test(x = sine$X, y = sine[,i], method = "spearman")$estimate)^2,
                          (cor.test(x = sig$X, y = sig[,i], method = "spearman")$estimate)^2,
                          (cor.test(x = circle$X, y = circle[,i], method = "spearman")$estimate)^2 )
}

# MIC
for(i in 2:(L+1)){
  MICoef[i-1,] <- c( mine(linear$X, linear[,i])$MIC,
                     mine(quad$X, quad[,i])$MIC,
                     mine(sine$X, sine[,i])$MIC,
                     mine(sig$X, sig[,i])$MIC,
                     mine(circle$X, circle[,i])$MIC)
}

# Kraskov
# k=6 is used for consistnency with Reshef et al. paper (2013) 
# direct = TRUE uses Kraskov et al. estimate of MI


  # Normalized to look at claim of non-MI estimation
for(i in 2:(L+1)){
  MIKraskov[i-1,] <- c( mutinfo(linear$X, linear[,i], k=6, direct = TRUE)/max(abs(entropy(as.matrix(linear[,c(1,i)]), k = 6, algorithm = "brute"))),
                        mutinfo(quad$X, quad[,i], k=6, direct = TRUE)/max(abs(entropy(as.matrix(quad[,c(1,i)]), k = 6, algorithm = "brute"))),
                        mutinfo(sine$X, sine[,i], k=6, direct = TRUE)/max(abs(entropy(as.matrix(sine[,c(1,i)]), k = 6, algorithm = "brute"))),
                        mutinfo(sig$X, sig[,i], k=6, direct = TRUE)/max(abs(entropy(as.matrix(sig[,c(1,i)]), k = 6, algorithm = "brute"))),
                        mutinfo(circle$X, circle[,i], k=6, direct = TRUE)/max(abs(entropy(as.matrix(circle[,c(1,i)]), k = 6, algorithm = "brute"))) )
}


for(i in 2:(L+1)){
  MIKraskov2[i-1,] <- c( mutinfo(linear$X, linear[,i], k=10, direct = TRUE)/max(abs(entropy(as.matrix(linear[,c(1,i)]), k = 10, algorithm = "brute"))),
                        mutinfo(quad$X, quad[,i], k=10, direct = TRUE)/max(abs(entropy(as.matrix(quad[,c(1,i)]), k = 10, algorithm = "brute"))),
                        mutinfo(sine$X, sine[,i], k=10, direct = TRUE)/max(abs(entropy(as.matrix(sine[,c(1,i)]), k = 10, algorithm = "brute"))),
                        mutinfo(sig$X, sig[,i], k=10, direct = TRUE)/max(abs(entropy(as.matrix(sig[,c(1,i)]), k = 10, algorithm = "brute"))),
                        mutinfo(circle$X, circle[,i], k=10, direct = TRUE)/max(abs(entropy(as.matrix(circle[,c(1,i)]), k = 10, algorithm = "brute"))) )
}

# max(abs(entropy(as.matrix(circle[,c(1,2)]), k = 6, algorithm = "brute")))

  # Linfoot correlation

for(i in 2:(L+1)){
  MIKraskovL[i-1,] <- c( 1-2^(-2*mutinfo(linear$X, linear[,i], k=6, direct = TRUE)),
                         1-2^(-2*mutinfo(quad$X, quad[,i], k=6, direct = TRUE)),
                         1-2^(-2*mutinfo(sine$X, sine[,i], k=6, direct = TRUE)),
                         1-2^(-2*mutinfo(sig$X, sig[,i], k=6, direct = TRUE)),
                         1-2^(-2*mutinfo(circle$X, circle[,i], k=6, direct = TRUE)) )
}

# Plot dependence measure vs. noise

# Squared Spearman Rank Correlation
df.fig3a <- data.frame(matrix(ncol = 3, nrow = L*5))
colnames(df.fig3a) <- c("Relationship", "1-Rsquared", "Spearman")
MeltR <- melt(RSquared)
MeltS <- melt(SpearmanCor)
df.fig3a$Relationship <- MeltR$variable
df.fig3a$`1-Rsquared`<- 1-MeltR$value
df.fig3a$Spearman <- MeltS$value


fig3a <- ggplot(df.fig3a, aes(x =`1-Rsquared` , y = Spearman, color = Relationship)) + geom_point(size = rel(3)) + xlab("1-Coefficient of Determination") + 
  ylab("Squared Spearman Correlation") + geom_hline(yintercept = 0.4, linetype = 2, color = "black", size = rel(1.2))+theme_Publication2()+
  scale_colour_manual(values=cbPalette) 

ggsave(filename = "fig3a.jpg", plot = fig3a, width = 20, height = 15, dpi = 300, units = "cm")



# MIC
df.fig3b <- data.frame(matrix(ncol = 3, nrow = L*5))
colnames(df.fig3b) <- c("Relationship", "1-Rsquared", "MIC")
MeltMIC <- melt(MICoef)
df.fig3b$Relationship <- MeltR$variable
df.fig3b$`1-Rsquared`<- 1-MeltR$value
df.fig3b$MIC <- MeltMIC$value

fig3b <- ggplot(df.fig3b, aes(x =`1-Rsquared` , y = MIC, color = Relationship)) + geom_point(size = rel(3)) + xlab("1-Coefficient of Determination") + 
  scale_colour_manual(values=cbPalette) + ylab("MIC") + geom_hline(yintercept = 0.4, linetype = 2, color = "black", size = rel(1.2))+theme_Publication2()

ggsave(filename = "fig3b.jpg", plot = fig3b, width = 20, height = 15, dpi = 300, units = "cm")


# Kraskov Linfoot Correlation
df.fig3c <- data.frame(matrix(ncol = 3, nrow = L*5))
colnames(df.fig3c) <- c("Relationship", "1-Rsquared", "KraskovLinfoot")
MeltK <- melt(MIKraskovL)
df.fig3c$Relationship <- MeltR$variable
df.fig3c$`1-Rsquared`<- 1-MeltR$value
df.fig3c$KraskovLinfoot <- MeltK$value


fig3c <- ggplot(df.fig3c, aes(x =`1-Rsquared` , y = KraskovLinfoot, color = Relationship)) + geom_point(size = rel(3)) + 
  xlab("1-Coefficient of Determination") +  ylab("Kraskov MI (k=6)") +  scale_colour_manual(values=cbPalette) +
  geom_hline(yintercept = 0.4, linetype = 2, color = "black", size = rel(1.2))+theme_Publication2()

ggsave(filename = "fig3c.jpg", plot = fig3c, width = 20, height = 15, dpi = 300, units = "cm")


# Figure 4: Gene Expression Data --------------------------------------------------------------------------------------------------------------------------

# gene_expression is a dataframe tabulating the gene expression (RNA-Seq) levels in a panel of 10 cancer cell lines
# each line belongs to one of 5 tissue types, with two lines per tissue
# within each tissue one line expresses the 9p21 locus and the other lacks the locus and exhibits the co-deletion of the mtap/cdkn2a genes
# rows are cell lines and columns are genes

gene_expression <- read.csv(file.choose()) 
by_lines <- t(gene_expression[,2:ncol(gene_expression)])
colnames(by_lines) <- gene_expression[,1]
MIC.lines <- mine(by_lines)
write.csv(MIC.lines$MIC, "miclines.csv")


# Heatmap clustering may be performed with line below. 
# The poster figure was produced using Gene-e from the Broad Institute with 1-Spearman rank correlation

heatmic <- superheat(MIC.lines$MIC,
                     # change the size of the labels
                     left.label.size = 0.3,
                     bottom.label.size = 0.3, 
                     row.dendrogram = TRUE,
                     col.dendrogram = TRUE,
                     #  n.clusters.rows = 10,
                     bottom.label.text.angle = 90,
                     title = "Hierarchical Clustering/Heatmap of MIC values",
                     title.size = 5)


# Same analysis is done using the spearman correlation between cell lines

library(Hmisc)

cor_lines <- rcorr(by_lines, type = "spearman")
write.csv(cor_lines$r, "corlines.csv")

heatcor <-  superheat(cor_lines$r,
                      # change the size of the labels
                      left.label.size = 0.1,
                      bottom.label.size = 0.3, 
                      row.dendrogram = TRUE,
                      col.dendrogram = TRUE,
                      #  n.clusters.rows = 10,
                      bottom.label.text.angle = 90,
                      title = "Hierarchical Clustering/Heatmap of Pearson r values",
                      title.size = 5,
                      bottom.label.text.alignment = "right")

