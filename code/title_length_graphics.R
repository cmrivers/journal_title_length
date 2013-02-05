big4 <- read.csv("~/Dropbox/Open_source_journals/big4_titlelength.csv")
us <- big4[big4$Country == 'United States',]
germ <- big4[big4$Country == 'Germany',]
neth <- big4[big4$Country == 'Netherlands',]
uk <- big4[big4$Country == 'United Kingdom',]

ust <- tapply(us$X2011.SNIP2, us$title_wordcnt, mean, na.rm=TRUE)[1:7]
ukt <- tapply(uk$X2011.SNIP2, uk$title_wordcnt, mean, na.rm=TRUE)[1:7]
germt <- tapply(germ$X2011.SNIP2, germ$title_wordcnt, mean, na.rm=TRUE)[1:7]
netht <- tapply(neth$X2011.SNIP2, neth$title_wordcnt, mean, na.rm=TRUE)[1:7]
combot <- data.frame(ust, ukt, germt, netht)
combot$index <- c(1:7)

usv <- tapply(us$X2011.SNIP2, us$title_wordcnt, var, na.rm=TRUE)[1:7]
ukv <- tapply(uk$X2011.SNIP2, uk$title_wordcnt, var, na.rm=TRUE)[1:7]
germv <- tapply(germ$X2011.SNIP2, germ$title_wordcnt, var, na.rm=TRUE)[1:7]
nethv <- tapply(neth$X2011.SNIP2, neth$title_wordcnt, var, na.rm=TRUE)[1:7]
vart <- data.frame(usv, ukv, germv, nethv)
vart$index <- c(1:7)


hist_cut <- ggplot(big4, aes(x=title_wordcnt, fill=Country))
hist_cut + geom_histogram() + ggtitle("Journal title length") +  xlim(0, 15) +  xlab('Title length') +  scale_fill_brewer(palette="Spectral") +
  ylab('Frequency') + theme(legend.justification=c(1,1), legend.position=c(1,1))
  theme(axis.title.x = element_text(colour="#333333"), axis.title.y = element_text(colour='#333333'))

scores <- ggplot(big4, aes(x=X2011.SNIP2, fill=Country)) 
scores + geom_histogram() +  xlim(0, 8) +  ggtitle("Journal impact") + xlab('Impact (2011 SNIP)') + 
  ylab('Frequency') + theme(legend.justification=c(1,1), legend.position=c(1,1)) +
theme(axis.title.x = element_text(colour="#333333"), axis.title.y = element_text(colour='#333333'))

ggplot(combot) + 
  ggtitle("Mean journal impact by title length") +  
    geom_line(aes(x=combot$index, y=combot$ust, colour='United States')) +
    geom_line(aes(x=combot$index, y=combot$ukt, colour='United Kingdom')) +
    geom_line(aes(x=combot$index, y=combot$germt, colour='Germany')) +
    geom_line(aes(x=combot$index, y=combot$netht, colour='Netherlands')) +
  xlab('Title length') + labs(colour = "Country") +
  ylab('Mean impact (2011 SNIP2)') +
  theme(axis.title.x = element_text(colour='#333333'), axis.title.y = element_text(colour="#333333"))
