# Mayotte Reef Temp Monitor

**Mayotte Reef Temp Monitor** est une application web interactive construite avec [Shiny pour R](https://shiny.rstudio.com/). Elle est conçue pour visualiser les données environnementales de températures et de profondeur de sondes autour dans le lagon de l'île de Mayotte dans l'Océan indien.

## Accessibilité
La version actuelle de l'application web est disponible en ligne à l'adresse [Mayotte Reef Temp Monitor](https://sebeyes.shinyapps.io/MayotteReefTempMonitor_V2-2).

---

## Structure du projet

Le projet est organisé en plusieurs dossiers et fichiers, chacun ayant un rôle bien défini dans le développement et la maintenance de l'application.

### 1. `code/`
Ce dossier contient les scripts R nécessaires pour modulariser la logique de l'application. Il est divisé en sous-dossiers pour une meilleure organisation :
- **`code_dependencies/`** : Fichiers sources utilisés par l'application principale pour encapsuler des fonctions réutilisables.
  - `00-read-write_HDF5.R` : Gère la lecture et l'écriture de fichiers au format HDF5, probablement pour manipuler de grandes quantités de données.
  - `01-temporal_selection.R` : Implémente des fonctionnalités pour filtrer les données selon des plages temporelles spécifiques.
  - `02-graph_display.R` : Contient les fonctions nécessaires pour générer et afficher des graphiques de série temporelle.
  - `03-ACF_plot.R` : Génère des graphes d'auto-corrélation (ACF) pour l'analyse statistique des données.
  - `04-ui.R` : Définit les éléments de l'interface utilisateur de l'application Shiny.
  - `05-new_data_file_cleaning.R` : Fournit des outils pour nettoyer et formater les fichiers de données brutes avant leur utilisation.

---

### 2. `data/`
Ce dossier contient les données utilisées par l'application :
- **`data_station.h5` et `data_station-save.h5`** : Fichiers HDF5 contenant la base de données de l'application ainsi qu'une sauvegarde publiée au 29 Novembre 2024.

---

### 3. `docs/`
Ce dossier stocke la documentation et les éléments visuels pour la communication ou la présentation :
- **`Logo/`** : Contient divers fichiers de logos en formats PNG et PSD, utilisés pour l'interface ou la documentation.
- **`UX_explication.pptx`** : Présentation PowerPoint expliquant l'expérience utilisateur.

---

### 4. `www/`
Ce dossier contient les fichiers statiques nécessaires à l'application web, comme des feuilles de style et des ressources visuelles :
- `app.css` : Feuille de style personnalisée pour définir l'apparence de l'interface utilisateur.
- Divers fichiers PNG : Images utilisées dans l'application, telles que les logos et les illustrations.

---

### 5. Fichiers principaux
- **`app.R`** : Le fichier principal de l'application Shiny. Il intègre l'interface utilisateur (UI) et la logique serveur, en s'appuyant sur les modules définis dans `code/code_dependencies/`.
- **`.gitignore`** : Définit les fichiers ou dossiers à exclure du suivi par Git.
- **`ReefWarmMonitor.Rproj`** : Fichier de projet RStudio permettant de structurer le projet dans l'environnement RStudio.

---

## Comment exécuter l'application en local ?

1. **Prérequis** :
   - Installez [R](https://cran.r-project.org/) et [RStudio](https://www.rstudio.com/).
   - Assurez-vous que les bibliothèques nécessaires sont installées. Vous pouvez utiliser le fichier de dépendances suivant pour les installer :

     ```r
     install.packages(c("shiny", "dplyr", "tidyr", "rhdf5", "ggplot2", "lubridate", 
                        "ggthemes", "dipsaus", "waiter", "DT", "bslib", "shinyBS", "shinyjs"))
     ```

2. **Lancer l'application** :
   - Ouvrez le fichier `app.R` dans RStudio.
   - Cliquez sur **"Run App"** pour lancer l'application.

---

## Fonctionnalités principales

- **Visualisation interactive** :
  - Graphiques générés dynamiquement à partir des données importées.
  - Analyse temporelle et calcul d'auto-corrélation.

- **Nettoyage de données** :
  - Des outils intégrés permettent de formater et nettoyer des fichiers bruts.

- **Interface personnalisée** :
  - Un design optimisé grâce à des thèmes personnalisés (`bslib`) et des outils comme `waiter` pour fluidifier l'expérience utilisateur.

---

## Contributeurs
Développé par Sébastien Lhoumeau avec le soutien de Vyctoria Marillac.

---

## Copyright
Accessibilité des données
