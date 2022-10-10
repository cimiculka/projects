library(GGally)
library(ggraph)
library(igraph)
library(Matrix)
library(showtext)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)

# Visualising networks using LEGO minifigure data
# Data Source: Rebrickable
# Tutorial source: University of Queensland LADAL: https://ladal.edu.au/net.html#Network_Visualization

minifigs <- read_csv('data/minifigs.csv')
star_wars_minifigs <- minifigs |>
  filter(theme == 'Star Wars')
star_wars_minifigs <- star_wars_minifigs |>
  mutate(character_name = str_split_fixed(name,',',2)[,1],.before = 'name')

unnamed <- c('Resistance Soldier','Battle Droid Pilot','Battle Droid Commander','Droideka / Destroyer Droid','Battle Droid Commander','Snowtrooper','Clone Trooper','Stormtrooper','Battle Droid','TIE Pilot','Rebel Trooper in Hoth Uniform','Clone Pilot','Super Battle Droid')

fig_count <- star_wars_minifigs |>
  filter(!character_name %in% unnamed) |>
  group_by(character_name) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  filter(count >= 10)

star_wars_minifigs <- star_wars_minifigs |>
  filter(character_name %in% fig_count$character_name) |>
  select(set_name,character_name)
rm(fig_count)

star_wars_cross <- crossprod(table(star_wars_minifigs[1:2]))
diag(star_wars_cross) <- 0

star_wars_cross <- as.data.frame(star_wars_cross)

sw_vertices <- star_wars_cross %>%
  mutate(minifig = rownames(.),
         appearances = rowSums(.)) |>
  select(minifig,appearances)

sw_edges <- star_wars_cross %>%
  mutate(from = rownames(.)) |>
  gather(to,co_occurrences,'Anakin Skywalker':'Yoda') |>
  mutate(co_occurrences = ifelse(co_occurrences == 0, NA,co_occurrences))

ig <- igraph::graph_from_data_frame(d=sw_edges, vertices=sw_vertices, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) |> 
  tidygraph::activate(nodes)

tg |>
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10",
                 max.overlaps = Inf) +
  theme_graph(background = "white") +
  guides(edge_width = 'none',
         edge_alpha = 'none')

vertex_size <- V(tg)$appearances

jedi <- c('Anakin Skywalker','Luke Skywalker','Mace Windu','Obi-Wan Kenobi','Qui-Gon Jinn', 'Rey','Yoda')
sith <- c('Darth Vader','Darth Maul','Emperor Palpatine')
alliance <- c('BB-8','C-3PO','Chewbacca','Han Solo','Poe Dameron','Princess Leia','R2-D2')

alignments <- data.frame(name = V(tg)$name) %>%
  mutate(alignment = case_when(
    name %in% jedi ~ 'jedi',
    name %in% alliance ~ 'alliance',
    name %in% sith ~ 'sith',
    TRUE ~ 'none'
  ))
V(tg)$alignment <- alignments$alignment
E(tg)$line_weights <- E(tg)$co_occurrences
font_add_google("Open Sans")
showtext_auto()

sw_graph <- tg |>
  ggraph(layout = "nicely") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = line_weights,
                    alpha = .1)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"),
                 colour="gray10",
                 max.overlaps = Inf) +
  geom_node_point(size = log(vertex_size)*2,
                  alpha = 0.7,
                  aes(colour = alignment)) +
  scale_colour_manual(values = c('jedi' = 'chartreuse','sith' = 'firebrick3','alliance' = 'orange','none' = 'slateblue3')) +
  theme_graph(background = "white",
              base_family = 'Open Sans',
              title_size = 22,
              subtitle_size = 18,
              caption_size = 16
              ) +
  labs(title = 'Luke Skywalker and R2-D2 are paired the most frequently of all LEGO Star Wars minifigures',
       subtitle = 'Pairings of named Star Wars characters appearing in at least 10 LEGO sets',caption = 'Graph by Cameron Miculka \nSource: Rebrickable')+ 
  guides(edge_width = 'none',
         edge_alpha = 'none')+
  theme(legend.position = 'none')
  
ggsave('star_wars_network.pdf',height = 8, width = 10)