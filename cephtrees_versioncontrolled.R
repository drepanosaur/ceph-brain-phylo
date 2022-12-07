library(ape)
library(phytools)
library(dplyr)
#list of species for which we have brain data
braindata_species$Taxon

#file path ~/nonhumans/di_cephproject/phylos/

options(scipen=999)
branching.times(lind2012)
View(lind2012$edge.length)

#proper matches w/ lindgren2012
#create list of species to keep on lindgren tree
lindkeep <- as.data.frame(braindata_species[c(1,2)])
write.csv(lindkeep, file="~/lindkeep.csv")
lindkeep <- read.csv("~/Desktop/lindkeep1.csv")
lindkeep$lind2012 %in% lind2012$tip.label

lindbackbone160922 <- keep.tip(lind2012, lindkeep$lind2012)
lindbackbone160922 #contains 35 species on lindgren tree for which we have brain data
#CHANGES: Octopus_vulgaris_unspecified changed to Octopus_vulgaris, Pterygioteuthis_giardi changed to Pterygioteuthis_giardi_hoylei
plot(lindbackbone160922) 
write.nexus(lindbackbone160922, file="lindbackbone160922.nex")
write.tree(lindbackbone160922, file="~/nonhumans/lindbackbone.txt")
lindbackbone160922$edge
plotTree(lindbackbone160922, fsize=0.56, node.numbers=T, ultrametric=TRUE)
plot.phylo(lindbackbone160922, type="phylogram", use.edge.length = TRUE)

View(branching.times(lindbackbone160922))
lindbackbone160922$edge.length
is.ultrametric(lindbackbone160922)

#need phylo for these 35 species 
needphy <- read.csv("~/nonhumans/di_cephproject/phylos/brainnotinlind.csv")

#ok. E. moschata and E. cirrhosa are sisters, from lopez-cordova 2022 diverging about 35mil

plot(lindbackbone160922)
edgelabels(round(lindbackbone160922$edge.length, digits=2), cex=0.5)

#making the bold step of adding a single species
#current lindgren backbone in newick:

plotTree(lindbackbone160922, node.numbers=T, cex=0.4)
which(lindbackbone160922$tip.label=="Eledone_cirrhosa")
lindbackbone260922 <- bind.tip(lindbackbone160922, "Eledone_moschata", edge.length=0.35, where=15)
plot(lindbackbone260922)

#uhh ok that technically worked but they appear to form a vertical branch and I don't know what that means
View(lindbackbone260922$edge.length)
write.nexus(lindbackbone260922, file="~/nonhumans/di_cephproject/phylos/lindbackbone260922.nex")
#all right I edited the split between moschata and cirrhosa to be 0.35 and subtracted that from the length of the branch splitting w/ E. dofleini. 
#no idea if that's correct
lindbackbone260922 <- read.tree(text="(((Cirrothauma_murrayi:1.258289268,((((Octopus_cyanea:0.2587354827,
                                Octopus_vulgaris:0.2298880296):0.02414536915,
                                Hapalochlaena_maculosa:0.2185500487):0.1504123437,
                                ((Vitreledonella_richardi:0.1697376951,
                                (Japetella_diaphana:0.07032929413,
                                Bolitaena_pygmaea:0.05840129681):0.1868675194):0.1115459812,
                                ((Eledone_cirrhosa:0.35,Eledone_moschata:0.35):0.310492,
                                Enteroctopus_dofleini:0.2733696093):0.03411134877):0.07293646064):0.06490643981,
                                (Tremoctopus_violaceus:0.3157350472,
                                Haliphron_atlanticus:0.3185967595):0.2524401908):0.3919037556):0.212799331,
                                Vampyroteuthis_infernalis:0.5681822075):0.5219330219,
                                (Sepietta_obscura:0.3417163464,
                                (((Lolliguncula_brevis:0.2586438078,
                                (Loligo_vulgaris:0.07481413798,
                                Loligo_forbesii:0.05991031616):0.0996676951):0.2284549308,
                                (Spirula_spirula:0.2980111626,((Bathyteuthis_abyssicola:0.2546799834,
                                Chtenopteryx_sicula:0.1688346575):0.06861698003,
                                (((Taonius_pavo:0.08615211965,Teuthowenia_megalops:0.2089150079):0.08438960941,
                                Cranchia_scabra:0.2733861578):0.07084226746,
                                (((Pterygioteuthis_giardi_hoylei:0.4578683631,
                                (Onychoteuthis_banksii:0.2329809862,
                                Gonatus_fabricii:0.2628152996):0.01003543753):0.02489881901,
                                ((Lycoteuthis_lorigera:0.2189566998,(Discoteuthis_laciniosa:0.2113527365,
                                (Joubiniteuthis_portieri:0.223142566,
                                (Grimalditeuthis_bonplandi:0.1726735617,
                                Chiroteuthis_veranyi:0.1705331777):0.06655236466):0.04170656954):0.02283367085):0.01191909964,
                                (Neoteuthis_thielei:0.1305724971,Architeuthis_dux:0.1394245892):0.08008312883):0.02839006206):0.01090029589,
                                Histioteuthis_miranda:0.2711635153):0.01682319922):0.04889282398):0.08147689972):0.02575051746):0.0366439316,
                                (Sepia_officinalis:0.1886095277,Sepiella_japonica:0.1161219329):0.3562701358):0.0404534956):0.0614491775);")
plot(lindbackbone260922)
#this was incredibly inefficient to add a single taxon
plot(lindbackbone260922, use.edge.length=FALSE, font=1)

is.ultrametric(lindbackbone260922)
#editing tree, 08.10.22
mini <- read.tree(text="((((((Helicocranchia_papillata), Galiteuthis_glacialis), Taonius_pavo), Teuthowenia_megalops), 
                  Megalocranchia_maxima), (Leachia_dislocata, Cranchia_scabra));")
plot(mini)

#Jan sent this 09.10.22
allcock2010 <- read.tree(text="((Ocythoe_tuberculata, (Argonauta_argo)), (Tremoctopus_violaceus, Haliphron_atlanticus));")
plot(allcock2010)

#all right just need to add ocythoe and argonauts as sister clade to T. violaceus and H. atlanticus on current tree
#ok I'm going to get rid of the branch lengths, because even if the lengths in the tree Annie sent are in mya 
#(which I don't think they are, they are described in paper as a topology) they aren't ultrametric)
cephtree111022 <- lindbackbone260922
cephtree111022$edge.length <- NULL
plot(cephtree111022)
write.tree(cephtree111022, file="~/nonhumans/di_cephproject/phylos/cephtree111022.tre")
write.nexus(cephtree111022, file="~/nonhumans/di_cephproject/phylos/cephtree111022.nex")

cephtree111022 <- read.tree(text="(((Cirrothauma_murrayi,((((Octopus_cyanea,Octopus_vulgaris),
                            Hapalochlaena_maculosa),((Vitreledonella_richardi,(Japetella_diaphana,Bolitaena_pygmaea)),
                            ((Eledone_cirrhosa,Eledone_moschata),Enteroctopus_dofleini))),
                            (Tremoctopus_violaceus,Haliphron_atlanticus))),Vampyroteuthis_infernalis),
                            (Sepietta_obscura,(((Lolliguncula_brevis,(Loligo_vulgaris,Loligo_forbesii)),
                            (Spirula_spirula,((Bathyteuthis_abyssicola,Chtenopteryx_sicula),
                            (((Taonius_pavo,Teuthowenia_megalops),Cranchia_scabra),
                            (((Pterygioteuthis_giardi_hoylei,(Onychoteuthis_banksii,Gonatus_fabricii)),
                            ((Lycoteuthis_lorigera,(Discoteuthis_laciniosa,(Joubiniteuthis_portieri,
                            (Grimalditeuthis_bonplandi,Chiroteuthis_veranyi)))),
                            (Neoteuthis_thielei,Architeuthis_dux))),Histioteuthis_miranda))))),
                            (Sepia_officinalis,Sepiella_japonica))));")


#SUCCESFULLY BOUND ARGONAUTIDS----
#ok, prune off H. atlanticus so it's just a single tip, then try replacing
cephtree111022 <- drop.tip(cephtree111022, "Haliphron_atlanticus")
library(tidytree)
View(as_tibble(cephtree111022))
#ok T. violaceus is node 11
cephtree111022 <- bind.tree(cephtree111022, allcock2010, where=11)
plot(cephtree111022)
#great ok that only took like 4 hours and I'm pretty sure I just re-figured out how to something I'd done 6 months ago
write.tree(cephtree111022, file="~/nonhumans/di_cephproject/phylos/cephtree111022.tre")
write.nexus(cephtree111022, file="~/nonhumans/di_cephproject/phylos/cephtree111022.nex")

#TREE AS OF 11 OCT 2022, 19:11----
cephtree111022_ <- read.tree(file="~/nonhumans/di_cephproject/phylos/cephtree111022.tre")
plot(cephtree111022_, font=1)

#according to fernandez-alvarez 2022 the Ommastrephidae (+Thysanoteuthidae) are sister to Cranchiidae
#I. illecebrosus is the only ommastreph we have, so will add it as sister to cranchiids on our tree

#sanchez 2018 species (not added to tree as of 20.10.22)
sanchez2018 <- read.tree(text="((((((Helicocranchia_papillata), Galiteuthis_glacialis), Taonius_pavo), 
                         Teuthowenia_megalops), Megalocranchia_maxima), (Leachia_dislocata, Cranchia_scabra));")


#adding sanchez et al. 2021 on sepiola, jan sent 19.10.22
# Neorossia_caroli
# Sepietta_obscura
# Sepietta_oweniana
# Sepiola_rondeleti
# Heteroteuthis_dispar
#they do not include Sepiola rondeleti, which we need to add, but Sepiola(s) affinis, intermedia, and robusta, so putting it there
sanchez2021 <- read.tree(text="(((Sepietta_oweniana, Sepietta_obscura), Sepiola_rondeleti),(Heteroteuthis_dispar, Neorossia_caroli));")
plot(sanchez2021)
#ok so replace Sepietta obscura on current tree
View(as_tibble(cephtree111022_))
#S. obscura is node 16
cephtree201022 <- bind.tree(cephtree111022_, sanchez2021, where=16)
plot(cephtree201022, cex=0.7)
#ok that went...really smoothly actually. low bar, but still
write.tree(cephtree201022, file="~/nonhumans/di_cephproject/phylos/cephtree201022.tre")
write.nexus(cephtree201022, file="~/nonhumans/di_cephproject/phylos/cephtree201022.nex")

#ok making new version where I add in some cranchiids from sanchez 2018
#I don't want to take out the cranchia-taonius clade so trying out treemerger or whatever, hopefully it takes care of overlapping taxa

library(RRphylo)
library(manipulate)
library(phylobase)
#let's try a toy phylogeny
ac <-read.tree(text="(((a), ((b, c))));")
ac <- compute.brlen(ac, 1)
plot(ac)
af <-read.tree(text="(((a), ((b, c), (e, f))));")
af <- compute.brlen(af, 1)
plot(af)

#I should get af if I combine the two, they agree
dat <- as.data.frame(rbind(c('e-f', 'b-c', FALSE)))
colnames(dat) <- c("bind", "reference", "poly")
tree.merger(backbone=ac, data=dat, source.tree=af, tip.ages=species.ages, plot=FALSE)
gog <- tree.merger(backbone=ac, data=dat, source.tree=af, plot=FALSE) 
plot(gog)
#all right well that worked I guess

#so backbone is cephtree201022
#source tree is sanchez2018
#sanchez 2018 species (not added to tree as of 20.10.22)
sanchez2018 <- read.tree(text="((((((Helicocranchia_papillata), Galiteuthis_glacialis), Taonius_pavo), Teuthowenia_megalops),
                         Megalocranchia_maxima), (Leachia_dislocata, Cranchia_scabra));")
plot(sanchez2018)

dato <- data.frame(bind=c("Leachia_dislocata", "Megalocranchia_maxima", "Galiteuthis_glacialis-Helicocranchia_papillata"),
                   reference=c("Cranchia_scabra", "Teuthowenia_megalops-Taonius_pavo", "Taonius_pavo"),
                   poly=c(FALSE, FALSE, FALSE)
)
cephtree241022 <- cephtree201022
cephtree241022 <- compute.brlen(cephtree241022, 1)
cephtree241022
sanchez2018 <- compute.brlen(sanchez2018, 1)
jh <- tree.merger(backbone=cephtree241022, data=dato, source.tree=sanchez2018, plot=FALSE)
jh$edge.length <- NULL
plot(jh)
cephtree241022 <- jh
plot(cephtree241022)
#because I somehow didn't do this earlier, re-add species for 241022 tree and write to files because R is going to delete them
write.tree(cephtree241022, file="~/nonhumans/di_cephproject/phylos/cephtree241022.tre")
write.nexus(cephtree241022, file="~/nonhumans/di_cephproject/phylos/cephtree241022.nex")

plot(read.tree(text="(Sepioteuthis sepioidea, (Lolliguncula brevis, ((Loligo forbesi, Loligo vulgaris), (Alloteuthis media, Alloteuthis subulata))));"))
plot(read.tree(text="(Loligo, (Sepioteuthis, (Alloteuthis, Lolliguncula)));"))

#this is the 241022 tree
cephtree291122 <- read.tree(text="(((Cirrothauma_murrayi,((((Octopus_cyanea,Octopus_vulgaris),Hapalochlaena_maculosa),
                            ((Vitreledonella_richardi,(Japetella_diaphana,Bolitaena_pygmaea)),
                            ((Eledone_cirrhosa,Eledone_moschata),Enteroctopus_dofleini))),
                            ((Ocythoe_tuberculata,(Argonauta_argo)),
                            (Tremoctopus_violaceus,Haliphron_atlanticus)))),
                            Vampyroteuthis_infernalis),((((Sepietta_oweniana,Sepietta_obscura),Sepiola_rondeleti),
                            (Heteroteuthis_dispar,Neorossia_caroli)),
                            (((Lolliguncula_brevis,(Loligo_vulgaris,Loligo_forbesii)),
                            (Spirula_spirula,((Bathyteuthis_abyssicola,Chtenopteryx_sicula),
                            (((((Taonius_pavo,(Helicocranchia_papillata,Galiteuthis_glacialis)),
                            Teuthowenia_megalops),Megalocranchia_maxima),
                            (Cranchia_scabra,Leachia_dislocata)),(((Pterygioteuthis_giardi_hoylei,
                            (Onychoteuthis_banksii,Gonatus_fabricii)),
                            ((Lycoteuthis_lorigera,(Discoteuthis_laciniosa,
                            (Joubiniteuthis_portieri,(Grimalditeuthis_bonplandi,Chiroteuthis_veranyi)))),
                            (Neoteuthis_thielei,Architeuthis_dux))),Histioteuthis_miranda))))),
                            (Sepia_officinalis,Sepiella_japonica))));")
#now add in other taxa
#then add Amphitretus pelagicus as a sister to Vitreledonelidae
#and Cirroteuthis_muelleri sister to Cirrothauma_murrayi
#(both Sanchez et al. 2018)
#and Mastigoteuthis_schmidti sister to Joubiniteuthis_portieri (fernandez-alvarez2022 & sanchez2018)
cephtree291122 <- read.tree(text="((((Cirrothauma_murrayi,Cirroteuthis_muelleri),((((Octopus_cyanea,Octopus_vulgaris),Hapalochlaena_maculosa),
                            (((Vitreledonella_richardi,Amphitretus_pelagicus) ,(Japetella_diaphana,Bolitaena_pygmaea)),
                            ((Eledone_cirrhosa,Eledone_moschata),Enteroctopus_dofleini))),((Ocythoe_tuberculata,(Argonauta_argo)),
                            (Tremoctopus_violaceus,Haliphron_atlanticus)))),Vampyroteuthis_infernalis),
                            ((((Sepietta_oweniana,Sepietta_obscura),Sepiola_rondeleti),(Heteroteuthis_dispar,Neorossia_caroli)),
                            (((Lolliguncula_brevis,(Loligo_vulgaris,Loligo_forbesii)),(Spirula_spirula,((Bathyteuthis_abyssicola,Chtenopteryx_sicula),
                            (((((Taonius_pavo,(Helicocranchia_papillata,Galiteuthis_glacialis)),Teuthowenia_megalops),Megalocranchia_maxima),
                            (Cranchia_scabra,Leachia_dislocata)),(((Pterygioteuthis_giardi_hoylei,(Onychoteuthis_banksii,Gonatus_fabricii)),
                            ((Lycoteuthis_lorigera,(Discoteuthis_laciniosa,((Joubiniteuthis_portieri,Mastigoteuthis_schmidti),
                            (Grimalditeuthis_bonplandi,Chiroteuthis_veranyi)))),(Neoteuthis_thielei,Architeuthis_dux))),
                            Histioteuthis_miranda))))),(Sepia_officinalis,Sepiella_japonica))));")
plot.phylo(cephtree291122, cex=0.7)
write.tree(cephtree291122, file="~/nonhumans/di_cephproject/phylos/cephtree291122.tre")
write.nexus(cephtree291122, file="~/nonhumans/di_cephproject/phylos/cephtree291122.nex")

#Add Bathothauma_lyromma and Egea_inermis from fernández-alvarez2022
plot(read.tree(text="(((Bathothauma_lyromma,Teuthowenia_megalops), 
  (Helicocranchia_papillata, Taonius_pavo)),(Megalocranchia_maxima,Egea_inermis));"))
#placing Megalocranchia_maxima where Megalocranchia c.f. oceanica is on their tree
#and Helicocranchia_papillata where they have Helicocranchia sp.
#Teuthowenia_megalops where is T. pellucida
#Bathothauma_lyromma as sister to Teuthowenia_megalops; Egea_inermis sister to Megalocranchia_maxima

cephtree071222 <- read.tree(text="((((Cirrothauma_murrayi,Cirroteuthis_muelleri),((((Octopus_cyanea,Octopus_vulgaris),Hapalochlaena_maculosa),
                            (((Vitreledonella_richardi,Amphitretus_pelagicus) ,(Japetella_diaphana,Bolitaena_pygmaea)),
                            ((Eledone_cirrhosa,Eledone_moschata),Enteroctopus_dofleini))),((Ocythoe_tuberculata,(Argonauta_argo)),
                            (Tremoctopus_violaceus,Haliphron_atlanticus)))),Vampyroteuthis_infernalis),
                            ((((Sepietta_oweniana,Sepietta_obscura),Sepiola_rondeleti),(Heteroteuthis_dispar,Neorossia_caroli)),
                            (((Lolliguncula_brevis,(Loligo_vulgaris,Loligo_forbesii)),(Spirula_spirula,((Bathyteuthis_abyssicola,Chtenopteryx_sicula),
                            (((((Taonius_pavo,(Helicocranchia_papillata,Galiteuthis_glacialis)), (Teuthowenia_megalops, Bathothauma_lyromma) ), (Megalocranchia_maxima, Egea_inermis) ),
                            (Cranchia_scabra,Leachia_dislocata)),(((Pterygioteuthis_giardi_hoylei,(Onychoteuthis_banksii,Gonatus_fabricii)),
                            ((Lycoteuthis_lorigera,(Discoteuthis_laciniosa,((Joubiniteuthis_portieri,Mastigoteuthis_schmidti),
                            (Grimalditeuthis_bonplandi,Chiroteuthis_veranyi)))),(Neoteuthis_thielei,Architeuthis_dux))),
                            Histioteuthis_miranda))))),(Sepia_officinalis,Sepiella_japonica))));")
plot.phylo(cephtree071222, cex=0.7)



