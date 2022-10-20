library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(grid)
theme_set(theme_bw())
select <- dplyr::select
filter <- dplyr::filter
# tuesdata <- tidytuesdayR::tt_load(x = 2022, week = 42)
# saveRDS(tuesdata, here::here('robjs', 'tidytuesday-2022-week-42.rds'))
tuesdata <- readRDS(here::here('robjs', 'tidytuesday-2022-week-42.rds'))
episodes <- tuesdata$episodes
dialogue <- tuesdata$stranger_things_all_dialogue


# Font shenanigans --------------------------------------------------------

# systemfonts::register_font(
#   "Vi Van Tho Hoa", 
#   plain = '/Users/alvaro/Library/Fonts/VANTHO1H.TTF'
# )
# systemfonts::reset_font_cache()

# fonts <- systemfonts::registry_fonts()
# fonts <- systemfonts::system_fonts()
# fonts %>% filter(str_detect(family, '[Vv]an')) %>% View() pull(family)

# exploratory ---------------------------------------------------------------------

str_count(dialogue$stage_direction, '\\[') %>% table()
# Some stage directions contain more than one indication, split into individual ones
stage_dir <- na.omit(dialogue$stage_direction)
n_stage_dir <- str_count(stage_dir, '\\[')
single_stage_dir <- stage_dir[n_stage_dir == 1]

many_stage_dir <- stage_dir[n_stage_dir > 1]
many_stage_dir_split <- str_split(many_stage_dir, "\\][[:space:]]*\\[") 
lengths(many_stage_dir_split) %>% table() # all 2
many_stage_dir_mat <- do.call(rbind, many_stage_dir_split)
many_stage_dir_mat[, 1] <- paste0(many_stage_dir_mat[, 1], ']')
many_stage_dir_mat[, 2] <- paste0('[', many_stage_dir_mat[, 2])
stage_dir_vec <- c(single_stage_dir, many_stage_dir_mat)

stage_direction_freq <- table(stage_dir_vec) %>% sort(decreasing = TRUE)
head(stage_direction_freq, 50) %>% names()
character_names <- paste0(
  "[",
  c("Dustin", "Mike", "Lucas", "Will", "Eleven", "Max",
    "Steve", "Nancy", "Robin", "Jonathan", "Eddie",
    "Hopper","Joyce", "Murray"),
  "]"
)
top_dirstage <- stage_direction_freq[!(names(stage_direction_freq) %in% character_names)] %>% head(9)
top_dirstage_pat <- str_replace_all(names(top_dirstage), c("\\[" = "\\\\[", "\\]" = "\\\\]")) %>% paste(collapse = "|")

# How often does each stage direction appear? -----------------------------
stage_dir_per_episode <- (
  dialogue 
  %>% filter( str_detect(stage_direction, top_dirstage_pat) )
  %>% select(season, episode, stage_direction)
  %>% mutate(
    stagedir_split = str_split(stage_direction, "\\][[:space:]]*\\["),
    stagedir_complete = map(stagedir_split, function(x){
      if (length(x) == 1){
        return(x)
      }
      return(c(
        paste0(x[1], ']'),
        paste0('[', x[2])
      ))
    })
    )
  %>% unchop(stagedir_complete) # separate list-columns into different rows
  %>% select(-stagedir_split)
  %>% filter( str_detect(stagedir_complete, top_dirstage_pat) )
  %>% count(season, episode, stagedir_complete)
)

stage_dir_per_episode# %>% count(season, episode, stagedir_complete) %>% print(n = 100)


# Enrich episode info -------------------------------------------------------------

episode_n <- episodes %>% select(season, episode) %>% mutate(episode_n = row_number())
stage_dir_per_episode <- left_join(stage_dir_per_episode, episode_n, by = c("season", "episode"))

# Theme elements ----------------------------------------------------------
# gray20 = #202020
red_pal <- scales::brewer_pal(palette = 'Reds')(5)[2:5]

p_top_stagedir <- (
  stage_dir_per_episode
  %>% ggplot(aes(episode_n, n))
  + geom_point(aes(color = ordered(season)), show.legend = FALSE)
  + geom_line(aes(color = ordered(season)),  show.legend = FALSE)
  + facet_wrap(~stagedir_complete, ncol = 3)
  + scale_color_manual(values = red_pal)
  + theme_minimal()
  + labs(x = "Episode number (absolute)", 
         y = "# Times stage direction appears in script")
  + theme(
      plot.background = element_rect(fill = 'black'),
      panel.background = element_rect(fill = 'black'), #color = red_pal[[4]], size = 3,
      panel.spacing = unit(1, 'lines'),
      panel.grid = element_line(color = '#999999'),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_rect(fill = red_pal[[4]],),
      text = element_text(color = 'white'),
      strip.text = element_text(
        color = 'white',  family = "VI Van Tho Hoa",
        size = 15, face = "plain" # same as bold lol
      ),
      axis.text = element_text(color = 'white'),
      axis.title.x = element_text(margin = margin(t = 1.5, unit = 'line')),
      axis.title.y = element_text(margin = margin(r = 1.5, unit = 'line')),
    )
)

# Assemble plot with logo? ------------------------------------------------
{
  logo <- png::readPNG(here::here('misc','Stranger_Things_logo.png'))
  logo_grob <- grid::rasterGrob(logo)
  
  text_col <- 'white'
  
  title_grob <- grid::textGrob(
    "[Screams in tidyverse]",
    gp = grid::gpar(fontfamily = "VI Van Tho Hoa", 
                    col = text_col, fill = text_col, fontsize = 24)
  )
  subtitle_text <- str_glue(
    "We need proper subtitles to understand whether someone is gasping, <br>",
    "screaming or, you know, in Russian. Here are the 9 annotations that <br>",
    "appear most often: from <span style='color:{red_pal[[1]]}'>Season 1</span>",
    " to <span style='color:{red_pal[[4]]}'>Season 4</span>",
  )
  subtitle_grob <- gridtext::richtext_grob(
    subtitle_text,
    gp = grid::gpar(fontfamily = "VI Van Tho", 
                    col = text_col, fill = text_col, fontsize = 12)
  )
  
  p_stranger <- (
    (
      wrap_plots(wrap_plots(title_grob, subtitle_grob, ncol = 1),
                 logo_grob, 
                 nrow = 1) / 
        p_top_stagedir
    ) 
    + plot_layout(ncol = 1, heights = c(1, 4)) 
    # + plot_annotation(title = "[Screams in tidyverse]")
    & theme(
        plot.background = element_rect(
          fill = 'black', color = NULL, linetype = 'blank'
        ),
      )
    )
  ggsave('plots/2022-42-stranger-things.png', plot = p_stranger,
         width = 4, height = 3, scale = 2.5)
}

# ggsave(
#   "tmp.png",
#   plot = wrap_plots(title_grob, subtitle_grob, ncol = 1),
#   bg = 'black',
#   width = 4, height = 6
#   )

# TidyTuesday graph: A three by three grid of line graphs that show the number of occurrences of stage directions throughout the episodes of Stranger Things. Stage directions are descriptions of non-verbal elements like chuckles, gasps, grunts, and so on. Sho
# Chuckles, gasps, grunts, in russian, panting, scoffs, screaming, sighs and stammers. Grunts and chuckles have an increasing trend. Sighs and stammers have a decreasing trend. In Russian only appears since season 3. The rest remain constant