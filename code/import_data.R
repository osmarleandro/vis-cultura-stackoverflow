library(tidyverse)

library(plotly)
library(viridis)
library(hrbrthemes)

display.brewer.all()

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

ppc = read_csv(here::here("data/participation-per-country.csv"))

# Estamos interessados na relação entre quanto as pessoas de diferentes países comentam em questões dos outros. 
# A proporção das pessoas do país que comentou nas questões de outros está medido na variável `comentaram_prop`. 
# Considerando essa variável, queremos examinar a relação entre ela e o quão hierárquicas são as relações em um país (`PDI`). 
# Queremos também levar em conta o quanto as pessoas daquele país têm acesso à Internet (`Internet`) 
# e qual o tamanho da base de dados que detectamos daquele país (`usuarios`). 

## Examinando essa relação
# Faça uma visualização que usa os princípios de eficácia no projeto de visualizações para facilitar as 
# comparações que você acha que são as mais importantes para entendermos esse contexto. 

# R. Vamos verificar um sumário de cada variável.
summary(ppc)

# Considerando que o conjunto de regiões possui valores não disponíveis, vamos filtrar do nosso conjunto de dados.
ppc <- ppc %>% 
    filter(!is.na(six_regions))
    # filter(!is.na(Internet)) %>% 
    # group_by(eight_regions) %>% 
    # summarise(
    #     comentaram_prop = mean(comentaram_prop),
    #     Internet = mean(Internet),
    #     usuarios = sum(usuarios)
    #     )

# Quais são as comparações mais importantes nesse contexto?
ppc %>%
    group_by(eight_regions) %>% 
    summarise(comentaram_prop = mean(comentaram_prop)) %>%
    
    ggplot(aes(x = reorder(eight_regions, comentaram_prop), y = comentaram_prop)) +
    geom_jitter(width = .1, alpha = .2) 


ppc %>%
  mutate(text = paste("Country: ", country, "\nUsers: ", usuarios, "\nInternet: ", Internet, "\nSite: ", site, sep="")) %>%
  
  ggplot(aes( x = comentaram_prop, y = PDI, shape = six_regions, color = Internet, text=text )) +
  geom_point(size = 2) +
  
  scale_size(range = c(1, 7)) +
  # scale_colour_gradientn(colours = terrain.colors(10)) +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  # scale_color_viridis(discrete=TRUE) +
  
  ylab("Proporção de comentários entre países") +
  xlab("Relação hierárquica no país")
  

ppc %>%
  mutate(text = paste("Country: ", country, "\nUsers: ", usuarios, "\nInternet: ", Internet, "\nSite: ", site, sep="")) %>%
  
  ggplot(aes( x = comentaram_prop, y = PDI, fill = six_regions, size = Internet, text=text )) +
  geom_point(alpha = 0.65) +
  
  scale_size(range = c(1, 7)) +
  scale_fill_viridis(discrete=TRUE) +
  
  theme( legend.title = element_blank() ) +
  ylab("Proporção de comentários entre países") +
  xlab("Relação hierárquica no país")

ggplotly(tooltip="text", 
         dynamicTicks = TRUE,
         originalData = TRUE,
         layerData = TRUE) %>% 
  add_annotations( text="six_regions", xref="paper", yref="paper",
                   x=1.02, xanchor="left",
                   y=0.8, yanchor="bottom",    # Same y as legend below
                   legendtitle=TRUE, showarrow=FALSE ) %>%
  layout( legend=list(y=0.8, yanchor="top" ) )
  # layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>% 
  # style(legendgroup = NULL)



ppc %>% 
    summarise(pearson = cor(comentaram_prop, PDI, method = "pearson"), 
              spearman = cor(comentaram_prop, PDI, method = "spearman"), 
              kendall = cor(comentaram_prop, PDI, method = "kendall"), )

