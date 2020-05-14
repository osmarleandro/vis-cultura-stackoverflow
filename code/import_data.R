library(tidyverse)

so = read_csv(here::here("data/raw/stackoverflow.csv"))
su = read_csv(here::here("data/raw/superuser.csv"))

ambos = bind_rows(
    "StackOverflow" = so, 
    "SuperUser" = su,
    .id = "site"
) %>% 
    select(
        site,
        country, 
        PDI, 
        IDV, 
        MAS, 
        UAI, 
        usuarios = num_u, 
        responderam_prop = perc_a, 
        perguntaram_prop = perc_q, 
        editaram_prop = perc_tp_e,
        comentaram_prop = perc_tp_c,
        GNI, Internet, EPI
    )

geographies = read_csv(here::here("data/raw/Data Geographies - v1 - by Gapminder - List of countries.csv"))

ambos %>% 
    left_join(geographies, by = c("country" = "name")) %>% 
    select(-members_oecd_g77, -`World bank region`, -`UN member since`) %>% 
    write_csv(here::here("data/participation-per-country.csv"))



# Estamos interessados na relação entre quanto as pessoas de diferentes países comentam em questões dos outros. 
# A proporção das pessoas do país que comentou nas questões de outros está medido na variável `comentaram_prop`. 
# Considerando essa variável, queremos examinar a relação entre ela e o quão hierárquicas são as relações em um país (`PDI`). Queremos também levar em conta o quanto as pessoas daquele país têm acesso à Internet (`Internet`) e qual o tamanho da base de dados que detectamos daquele país (`usuarios`). 

## Examinando essa relação
# Faça uma visualização que usa os princípios de eficácia no projeto de visualizações para facilitar as comparações 
# que você acha que são as mais importantes para entendermos esse contexto. 


