library(wordcloud2)
library(tm)
library(Hmisc)


words <- c(rep("Amyris", 19),
           rep("Artemisinin", 7),
           rep("Hot", 17), 
           rep('Fifty', 17), 
           rep("BiofuelsDigest", 11),
           rep("biotechnology", 9),
           rep("chromatography", 6),
           rep("CRISPR", 8),
           rep("CAS9", 8),
           rep("DNA", 7),
           rep("RNA", 7),
           rep("Brotas", 7),
           rep("Brazil", 7),
           rep("DSM", 8),
           rep("Total", 10),
           rep("JetFuel", 7),
           rep("California", 5),
           rep("Data", 8),
           rep("Emeryville", 4),
           rep("genetics", 11),
           rep("energy", 9),
           rep("Fuel", 8),
           rep("Novvi", 10),
           rep("molecule", 8),
           rep("chemicals", 8),
           rep("DARPA", 10),
           rep("Growth", 9),
           rep("flavors", 9),
           rep("fragrances", 9),
           rep("KoolAid", 10),
           rep("Fene", 10),
           rep("MegaBio", 10),
           rep("Lynx", 10),
           rep("Hydra", 10),
           rep("VICE", 10),
           rep("ArtforAll", 10),
           rep("Radha", 10),
           rep("M2K", 11),
           rep("Santana", 10),
           rep("Wolverine", 10),
           rep("Manatee", 10),
           rep("Biofene", 8),
           rep("DOE", 10),
           rep("sustainable", 7),
           rep("renewable", 6),
           rep("molecular biology", 5),
           rep("farnesene", 6),
           rep("Biossance", 8),
           rep("HTS", 11),
           rep("chemistry", 8),
           rep("biology", 7),
           rep("fermentation", 6),
           rep("nature", 8),
           rep("squalane", 9),
           rep("screening", 8),
           rep("engineering", 7),
           rep("science", 10),
           rep("2018", 14),
           rep("yeast", 12),
           rep("genes", 11),
           rep("sequencing", 9),
           rep("stitches", 9),
           rep("RaBITS", 8),
           rep("ASE", 8),
           rep("FLO", 9))

word_corp <- VCorpus(VectorSource(words))

dtm <- DocumentTermMatrix(word_corp)

word_matrix <- as.matrix(dtm)             

v <- sort(colSums(word_matrix), decreasing = T)

df <- data.frame(words = names(v), freq = v)


df$words <- as.character(df$words)

df$words <- capitalize(df$words)

df$words[df$words == "Hot"] <- "HOT"
df$words[df$words == "Doe"] <- "DOE"
df$words[df$words == "Hts"] <- "HTS"
df$words[df$words == "Flo"] <- "FLO"
df$words[df$words == "Ase"] <- "ASE"
df$words[df$words == "Cas9"] <- "CAS 9"
df$words[df$words == "Dsm"] <- "DSM"
df$words[df$words == "Dna"] <- "DNA"
df$words[df$words == "Rna"] <- "RNA"
df$words[df$words == "M2k"] <- "M2K"
df$words[df$words == "Biofuelsdigest"] <- "Biofuels Digest"

pal <- c("#e67e22","#FF0000", "#FF0000", rep("#27ae60", 67))

set.seed(5)
letterCloud(data = df,
           word = "A",
           size = 0.21,
           gridSize = 0.23,
           color = pal,
           shuffle = F,
           fontFamily = "Times",
           minRotation = -pi/2,
           maxRotation = pi/2,
           rotate = .6, 
           #letterFont = "Courier",
           backgroundColor = "black"
           )

wordcloud2(data = word_df, size = 0.21, word = "HOT", color = pal, backgroundColor = "black", )


