library(tidyverse)
library(haven)

# transforme le code de chaque pays dans la base de données en son nom complet
transformer_code_pays <- function(code_pays) {
  case_when(code_pays == "at" ~ "Autriche",
            code_pays == "au" ~ "Australie",
            code_pays == "be" ~ "Belgique",
            code_pays == "ca" ~ "Canada",
            code_pays == "ch" ~ "Suisse",
            code_pays == "cl" ~ "Chili",
            code_pays == "cz" ~ "République tchèque",
            code_pays == "de" ~ "Allemagne",
            code_pays == "dk" ~ "Danemark",
            code_pays == "ee" ~ "Estonie",
            code_pays == "es" ~ "Espagne",
            code_pays == "fi" ~ "Finlande",
            code_pays == "fr" ~ "France",
            code_pays == "gb" ~ "Royaume-Uni",
            code_pays == "gr" ~ "Greece",
            code_pays == "hu" ~ "Hongrie",
            code_pays == "ie" ~ "Irlande",
            code_pays == "il" ~ "Israël",
            code_pays == "is" ~ "Islande",
            code_pays == "it" ~ "Italie",
            code_pays == "jp" ~ "Japon",
            code_pays == "kr" ~ "Corée du Sud",
            code_pays == "lu" ~ "Luxembourg",
            code_pays == "mx" ~ "Mexique",
            code_pays == "nl" ~ "Pays-Bas",
            code_pays == "no" ~ "Norvège",
            code_pays == "nz" ~ "Nouvelle-Zélande",
            code_pays == "pl" ~ "Pologne",
            code_pays == "pt" ~ "Portugal",
            code_pays == "se" ~ "Suède",
            code_pays == "sk" ~ "Slovaquie",
            code_pays == "tr" ~ "Turquie",
            code_pays == "us" ~ "États-Unis",
            TRUE ~ NA_character_)
}

# source : http://www.impic-project.eu/data/
impic <- read_dta("impic2016.dta") %>%
  filter(cntry != "ed" & cntry != "er") %>%
  mutate(cntry = transformer_code_pays(cntry)) %>%
  select(cntry, year, AvgS_ImmPol)

# calcule la moyenne de l'indice pour l'ensemble des pays de l'OCDE chaque année
ocde <- impic %>%
  group_by(year) %>%
  summarize(AvgS_ImmPol = mean(AvgS_ImmPol)) %>%
  mutate(cntry = "OCDE")

# ajoute la moyenne de l'OCDE aux données
données <- bind_rows(impic, ocde)

# crée une structure avec les données pour la France et l'OCDE
données_france <- données %>%
  filter(cntry %in% c("France", "OCDE"))

# crée une structure avec les données pour une sélection de pays de l'OCDE
données_autres <- données %>%
  filter(cntry %in% c("Allemagne", "Suède", "Danemark", "États-Unis", "Italie", "Israël", "OCDE", "Royaume-Uni"))

ggplot(données_france, aes(x = year, y = AvgS_ImmPol, group = cntry, color = cntry)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution de la dureté de la politique d'immigration en France et dans les pays de l'OCDE entre 1980 et 2010") +
  xlab("Année") +
  ylab("Dureté") +
  scale_color_discrete(name = "Pays") +
  scale_x_continuous(breaks = seq(1980, 2010, 1)) +
  scale_y_continuous(breaks = seq(0, max(données_france$AvgS_ImmPol), 0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Immigration Policies in Comparison (http://www.impic-project.eu) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution de la dureté de la politique d'immigration en France et dans les pays de l'OCDE entre 1980 et 2010.png", width = 15, height = 10)

ggplot(données_autres, aes(x = year, y = AvgS_ImmPol, group = cntry, color = cntry)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution de la dureté de la politique d'immigration dans quelques pays de l'OCDE entre 1980 et 2010") +
  xlab("Année") +
  ylab("Dureté") +
  scale_color_discrete(name = "Pays") +
  scale_x_continuous(breaks = seq(1980, 2010, 1)) +
  scale_y_continuous(breaks = seq(0, max(données_autres$AvgS_ImmPol), 0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Immigration Policies in Comparison (http://www.impic-project.eu) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution de la dureté de la politique d'immigration dans quelques pays de l'OCDE entre 1980 et 2010.png", width = 15, height = 10)
