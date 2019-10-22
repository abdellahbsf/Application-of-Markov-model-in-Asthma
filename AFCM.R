g=glm(CTR~rhnite+concomitantes+dyspnee+reveil+duirne_bin,family=binomial,data=d)

library("FactoMineR")
library("factoextra")
d=read.table(file.choose(),header=TRUE,sep="\t")
d=na.omit(d)
poison.active <- d[1:290, 2:35]
res.mca <- MCA (poison.active, graph = FALSE)
library("factoextra")
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))
fviz_cos2(res.mca, choice = "var", axes = 1:2)

ind <- get_mca_ind (res.mca)
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())
fviz_mca_ind (res.mca,
             label = "none", # masquer le texte des individus
             habillage = "reveil", # colorer par groupes
             palette = c ("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal ())
# Visualiser les catégories de variables avec cos2> = 0.4
fviz_mca_var (res.mca, select.var = list (cos2 = 0.4))
# Top 10 des variables actives avec le cos2 le plus elevé
fviz_mca_var (res.mca, select.var = list (cos2 = 10))
# Sélectionner par noms
name <- list(name = c("Fever_n", "Abdo_y", "Diarrhea_n",
                      "Fever_Y", "Vomit_y", "Vomit_n"))
fviz_mca_var (res.mca, select.var = name)
# Top 5 des categories de variables les plus contributifs
fviz_mca_biplot (res.mca, select.ind = list (contrib = 5),
               select.var = list (contrib = 5),
               ggtheme = theme_minimal ())