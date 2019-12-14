library(shipunov)

s1 <- scan("input/lists/berazain2014_annot_sp_list_el_verde_trns.txt", sep="\t", what="char")
s2 <- scan("input/lists/mowbray2012_trees_el_yunque_index.txt", sep="\t", what="char")
sy <- read.table("input/filters/synonyms.txt", sep="\t", as.is=T)
fa <- read.table("input/filters/families.txt", sep="\t", as.is=T)
faa <- read.table("input/filters/families_alternative_names.txt", sep="\t", as.is=T)
faf <- read.table("input/filters/families_ferns_corrections.txt", sep="\t", as.is=T)
fas <- read.table("input/filters/families_syang_corrections.txt", sep="\t", as.is=T)
su <- read.table("input/filters/families_supra.txt", sep="\t", as.is=T)
co <- read.table("input/filters/species_common_names.txt", sep="\t", quote="", as.is=T)
au <- read.table("input/filters/species_authorship.txt", sep="\t", quote="", as.is=T)
ph1 <- read.table("input/photos/flora_el_verde/descript.ion", sep="\t", quote="", as.is=T)
ph2 <- read.table("input/photos/descript.ion", sep="\t", quote="", as.is=T)

# SYNONYMS
sp <- c(s1, s2, ph2[,2]) # so descript.ion of the second photo part serves as species list
sp1 <- Recode(sp, sy[,1], sy[,2])
sp2 <- sort(unique(sp1))

# GENERA
sp3 <- gsub(" x ", " x_", sp2)
GEN <- do.call(rbind, strsplit(sp3, " "))[,1]
write(file="output/reports/top_10_genera.txt", rev(sort(table(GEN)))[1:10]) # output top 10 genera

# FAMILIES
fa1 <- Recode4(GEN, fa[,1], fa[,2])
fa2 <- Recode(fa1, faa[,1], faa[,2])
fa3 <- Recode(fa2, faf[,1], faf[,2])
FAM <- Recode(fa3, fas[,1], fas[,2])
write(file="output/reports/top_15_families.txt", rev(sort(table(FAM)))[1:15]) # output top 15 families

# HIGHER GROUPS
su1 <- Recode4(fa1, su[,1], su[,2])
SUP <- factor(su1, levels=c("Pteridophyta", "Liliidae", "Magnoliidae", "Rosidae", "Asteridae", ""), ordered=T) # fragile, change if new group appears
write(file="output/reports/higher_groups_stat.txt", rev(sort(table(SUP)))) # output higher groups' statistics

# JOIN AND ORDER
CHL <- data.frame(SUP, FAM, SPC=sp2, stringsAsFactors=F) # sp1 (joint species list) is used
CHL <- CHL[order(CHL$SUP, CHL$FAM, CHL$SPC),] # first by suprageneric, then by families, then by species

# SPECIES: PREPARE TEX OUTPUT
SPC1 <- paste("\\K{", CHL$SPC, "}", sep="") # italicize
SPC2 <- gsub(" sp.}", "} sp.", SPC1) # work with "sp."
SPC3 <- gsub(" x ", " \\\\(\\\\times\\\\)", SPC2)
au1 <- Recode4(CHL$SPC, au[,1], au[,2])
SPC4 <- paste(SPC3, ifelse(au1 != "", " ", ""), au1, sep="") # authorships
SPC5 <- gsub("&", "\\\\D\\\\", SPC4)
co1 <- Recode4(CHL$SPC, co[,1], co[,2])
SPC6 <- paste(SPC5, ifelse(co1 != "", ", ", ""), co1, sep="") # common names
SPC7 <- paste("\\S{", SPC6, "}", sep="")

# FAMILIES AS TITLES: REMOVE ALL OCCURRENCES EXEPT FIRST
FAM1 <- CHL$FAM
tmp <- FAM1[1]
for (i in 2:(length(FAM1))) 
{
if (FAM1[i] == tmp)
	{
	FAM1[i] <- ""
	}
else
	{
	tmp <- FAM1[i]
	}
}
# FAMILIES: PREPARE TEX OUTPUT
FAM2 <- ifelse(FAM1 != "", paste("\\F{", FAM1, "}", sep=""), "")

# SUPRAGENERIC GROUPS AS CHAPTERS
SUP1 <- as.character(CHL$SUP)
tmp <- SUP1[1]
for (i in 2:(length(SUP1))) 
{
if (SUP1[i] == tmp)
	{
	SUP1[i] <- ""
	}
else
	{
	tmp <- SUP1[i]
	}
}
# SUPILIES: PREPARE TEX OUTPUT
SUP2 <- ifelse(SUP1 != "", paste("\\C{", SUP1, "}", sep=""), "")

# PHOTOS: PREPARE TEX OUTPUT
IMG1a <- gsub("^", "../../input/photos/flora_el_verde/thumbs/", ph1[,1])
IMG1b <- gsub("^", "../../input/photos/thumbs/", ph2[,1])
IMG2 <- aggregate(c(IMG1a, IMG1b), list(c(ph1[,2], ph2[,2])), function(.x) paste("\\I{", .x, "}", sep="", collapse=" "))
IMG3 <- Recode4(CHL$SPC, IMG2[,1], IMG2[,2])
IMG4 <- ifelse(IMG3 != "", paste("\\II{", IMG3, "}", sep=""), IMG3)

write.table(file="output/pdf/body", data.frame(SUP2, FAM2, SPC7, IMG4), quote=F, row.names=F, col.names=F, sep=" ", eol="\n\n")

# REPORTS
write(file="output/reports/no_species_authorship.txt", CHL$SPC[au1 == ""])
write(file="output/reports/no_photo.txt", CHL$SPC[IMG3 == ""])
write(file="output/reports/no_family.txt", CHL$SPC[CHL$FAM == ""])
write(file="output/reports/no_higher_group.txt", CHL$FAM[CHL$SUP == ""])
write(file="output/reports/no_common_name.txt", CHL$SPC[co1 == ""])
