---
author / auteur : Hyeongsuk Jin
---

# Shiny app for interprovincial mobility of journeypersons in Canada

[https://shiny.covid.cloud.statcan.ca/rais_siai/en](https://shiny.covid.cloud.statcan.ca/rais_siai/en)

A data visualization application to help understand trends and patterns in the mobility of certified journeypersons in Canada.

Data from Statistics Canada [Table 37-10-0154-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710015401), **Movements of journeypersons by province or grouped territories of certification (origin) and residence (destination), one and three years after certification**

For concepts and definitions used, please see the [Reference Guide](https://www150.statcan.gc.ca/n1/en/catalogue/37200001).

The chord diagram is generated using the [circlize](https://cran.r-project.org/package=circlize) package, made by Zugang Gu.
For more information about the package, please see his book [Circular Visualization in R](https://jokergoo.github.io/circlize_book/book/).
The code for the diagram is based on the [example](https://github.com/guyabel/migest/blob/master/demo/cfplot_reg2.R) from [Guy Abel](https://guyabel.com/).


# Une application Shiny pour la mobilité interprovinciale des compagnons au canada

[https://shiny.covid.cloud.statcan.ca/rais_siai/fr](https://shiny.covid.cloud.statcan.ca/rais_siai/fr)

Une application de visualisation des données pour aider à comprendre les tendances et les schémas de la mobilité des compagnons nouvellement certifiés au Canada.

Données du Statistics Canada [Tableau 37-10-0154-01](https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=3710015401&request_locale=fr), **Mouvements des compagnons selon la province ou les territoires regroupés de certification (origine) et de résidence (destination), un et trois ans après leur certification.**

Pour les concepts et définitions utilisés, veuillez consulter le [guide de référence](https://www150.statcan.gc.ca/n1/fr/catalogue/37200001).

Le diagramme à cordes est généré à l'aide du paquet [circlize](https://cran.r-project.org/package=circlize), réalisé par Zugang Gu. Pour plus d'informations sur le paquet, veuillez consulter son livre [Circular Visualization in R](https://jokergoo.github.io/circlize_book/book/) (en anglais).
Le code du diagramme est basé sur l'[exemple](https://github.com/guyabel/migest/blob/master/demo/cfplot_reg2.R) de [Guy Abel](https://guyabel.com/) (en anglais).
