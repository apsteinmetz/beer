# Hop analysis demo

library(tidyverse)
library(tidytext)
library(pdftools)
library(googlesheets4)
# ML Libraries
library(tidymodels)
library(tidypredict)
library(randomForest)
library(vip)
library(rpart)
# source: brewcabin.com and yakima chief hops
sheet <- "https://docs.google.com/spreadsheets/d/1qNedHmXqhqzpqt1vgzjUJ0NYtY_7Lov_Vue_k_hZPf8/edit#gid=1675045246"
# public sheet so we don't neet authorization
googlesheets4::gs4_deauth()
hops_raw <- read_sheet(sheet,sheet="Hops (Uploaded)",
                       col_types = "ccciiiiiiiiiiiiiiccccccc") %>%
  mutate(Origin = as.factor(Origin),Type = as.factor(Type))

save(hops_raw,file="./data/hops_raw.rdata")
# ------------------------------------------------
# DATA CLEANING
# tidy up names
tidy_names <- function(col_names){
  col_names <- tolower(col_names) %>%
    str_replace_all(c(" " = "_" , "," = "" )) %>%
    str_replace_all("\\(%\\)","pct") %>%
    str_replace_all("\\(ml\\/100g\\)","ml_per_100g") %>%
    str_replace_all("\\(%_of_total_oil\\)","oil_fraction") %>%
    str_remove_all("_\\(ychhops.com\\)") %>%

    {.}
  return(col_names)
}

# function to handle range strings like "1 - 2" or "< 1"
# < 1 is treated as 0.5
str_parse_range <- Vectorize(function(range_str){
  frac_reg <- "(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)"
  value = NA
  if(is.na(range_str)) return(NA)
  if(str_detect(range_str,paste0(frac_reg," - ",frac_reg))){
    values = str_extract_all(range_str,frac_reg)
    value = (as.numeric(values[[1]][1]) + as.numeric(values[[1]][3]))/2
  }
  if(str_detect(range_str,"< 1")){
    value = 0.5
  }
  return(value)
} )

hops_clean <- hops_raw %>%
  select(-starts_with("sources")) %>%
  rename_with(tidy_names)

# --------------------------------------
# INITIALIZE hops DATA FRAME

# make a data set with just averages of high/low component ranges
# is this representative of the true averages for these cultivars?
# we don't know
hops <- hops_clean %>% group_by(variety) %>%
  transmute(variety,
            type,
           origin,
           beer_styles,
           aroma,
           substitutions,
           alpha=mean(c_across(contains("alpha"))),
         beta=mean(c_across(contains("beta"))),
         cohumulone=mean(c_across(contains("co-humulone"))),
         myrcene=mean(c_across(contains("myrcene"))),
         caryophyllene=mean(c_across(contains("caryophyllene"))),
         humulene=mean(c_across(contains("humulene"))),
         farnesene = str_parse_range(farnesene_oil_fraction),
         total_oil=mean(c_across(starts_with("total_oil")))
  ) %>%
  ungroup()

# --------------------------------------------
# get all the aroma words

# unhelpful aroma descriptors.
brew_stop_words <- tibble(word = c("aromas","aroma","mild","pleasant","characteristics",
                    "american","exceptional","exceptionally","Flavor",
                    "complement","intense","balance","complex","notes",
                    "top","tones","overtones","understated","lots",
                    "needles","clean","cut","crushed","hints",
                    "character","characters","flavor","hop",
                    "undercurrents"),
                    lexicon = "brewing")
# --------------------------------------------
# custom word stemmer
# i.e. wood, woody and woodsy become wood
brew_stem_words <-
  tribble(
    ~term, ~stem,
    "woodsy","wood",
    "woody","wood",
    "piney","pine",
    "earthy","earth",
    "resinous","resin",
    "zesty","zest",
    "zested","zest",
    "bittering","bitter",
    "bitterness","bitter"
  )

brew_stem_words <-
  tribble(
    ~term, ~stem,
    "woodsy","wood",
    "woody","wood",
    "piney","pine",
    "earthy","earth",
    "resinous","resin",
    "zesty","zest",
    "zested","zest",
    "bittering","bitter",
    "bitterness","bitter"
  )
stem <- Vectorize(function(term) {
  i <- match(term, brew_stem_words$term)
  if (is.na(i)) {
    stem <- term
  } else {
    stem <- brew_stem_words$stem[[i]]
  }
  return(stem)
})

hops_aromas <- hops_clean %>%
  select(variety,aroma) %>%
  mutate(aroma = tolower(aroma)) %>%
  tidytext::unnest_tokens("aroma","aroma") %>%
  filter(!(aroma %in% stop_words$word)) %>%
  filter(!(aroma %in% brew_stop_words$word)) %>%
  mutate(aroma = stem(aroma)) %>%
  # remove_missing() %>%
  mutate(aroma = as.factor(aroma)) %>%
  {.}

# Since "noble" is an aroma word of some non-noble hops,
# make sure that is an attribute of actual noble hops
# German Hallertauer Mittelfrüh, Tettnang, Spalt and Czech Saaz
appendix <- tribble(
  ~variety,~aroma,
  "Saaz","noble",
  "Spalter","noble",
  "Spalter Select","noble",
  "Hallertauer Mittelfrüh","noble"
  )
hops_aromas <- bind_rows(hops_aromas,appendix)

# Put the word vectors back into the data.
hops <- hops %>%
  select(-aroma) %>%
  left_join(nest(hops_aromas,aroma=aroma),by="variety")
# --------------------------------------------
# get all the style names
# and bring some consistency to them.  Ugh.
hops_styles <- hops_clean %>%
  select(variety,beer_styles) %>%
  tidytext::unnest_tokens("beer_styles","beer_styles",
                          token="regex",pattern="\n|,|&") %>%
  mutate(beer_styles = trimws(beer_styles)) %>%
  filter(!(beer_styles %in% stop_words$word)) %>%
  mutate(beer_styles = str_replace(beer_styles,"('s|s)$","")) %>%
  mutate(beer_styles = str_replace(beer_styles,"india pale ale","ipa")) %>%
  mutate(beer_styles = str_replace(beer_styles,"extra special bitter","esb")) %>%
  mutate(beer_styles = str_replace(beer_styles,"kolsch","kölsch")) %>%
  mutate(beer_styles = str_replace(beer_styles,"pilsener lagers\\.|pilsner","pilsener")) %>%
  mutate(beer_styles = str_remove(beer_styles,"-style")) %>%
  mutate(beer_styles = str_remove(beer_styles,"/[a-z]+")) %>%
  mutate(beer_styles = str_remove(beer_styles,"\\*")) %>%
  mutate(beer_styles = str_remove(beer_styles," beer")) %>%
  mutate(beer_styles = str_remove(beer_styles," (esb)")) %>%
  mutate(beer_styles = str_replace(beer_styles,"us ","american ")) %>%
  mutate(beer_styles = ifelse(str_detect(beer_styles,"wei"),"hefeweizen",beer_styles)) %>%
  mutate(beer_styles = ifelse(str_detect(beer_styles,"borwn"),"brown ale",beer_styles)) %>%
  mutate(beer_styles = as.factor(beer_styles)) %>%
  {.}

# Put the word vectors back into the data.
hops <- hops %>%
  select(-beer_styles) %>%
  left_join(nest(hops_styles,beer_styles=beer_styles),by="variety")

# --------------------------------------------
# get all the substitute names
# big trouble with German spellings. Make consistant
hops_subs <- hops_clean %>%
  select(variety,substitutions) %>%
  tidytext::unnest_tokens("substitutions","substitutions",
                          token="regex",pattern="\n|,|&") %>%
  filter(!(substitutions %in% stop_words$word)) %>%
  filter(!(substitutions %in% c("??"))) %>%
  mutate(substitutions = str_replace(substitutions,"'s","s")) %>%
  mutate(substitutions = str_replace(substitutions,"haller[a-z]+","hallertau")) %>%
  mutate(substitutions = str_replace(substitutions,"tett[a-z]+","tettnanger")) %>%
  mutate(substitutions = str_replace(substitutions,"mt(.)? ","mount ")) %>%
  mutate(substitutions = str_remove(substitutions,"®|™")) %>%
  mutate(substitutions = str_remove(substitutions,"variety")) %>%
  mutate(substitutions = str_replace(substitutions,"goldings","golding")) %>%
  mutate(substitutions = str_replace(substitutions,"^kent","east kent")) %>%
  mutate(substitutions = str_replace(substitutions,"spalter","spalt")) %>%
  mutate(substitutions = str_replace(substitutions,"willametter","willamette")) %>%
  mutate(substitutions = str_replace(substitutions,"herkules","hercules")) %>%
  # get rid of origins to simplify. Some may not agree with this decision
  mutate(substitutions = str_remove(substitutions,"( )?\\([a-z]+\\)")) %>%
  mutate(substitutions = str_remove(substitutions,"us |uk |u\\.s\\. |u\\.k\\. ")) %>%
  mutate(substitutions = str_trim(substitutions)) %>%
  filter(!str_detect(tolower(variety),substitutions)) %>%
  mutate(substitutions = as.factor(substitutions)) %>%
  {.}

# Put the word vectors back into the data.
hops <- hops %>%
  select(-substitutions) %>%
  left_join(nest(hops_subs,substitutions=substitutions),by="variety")

# --------------------------------------------
#  EXPLORATORY DATA ANALYSIS
# show origins of hops
hops %>% ggplot(aes(origin,fill=type)) + geom_bar() +
  coord_flip()

hops %>% select(origin,type) %>% table()

# plot alpha vs oil by bittering type

hops %>%
  filter(type != "Dual Purpose") %>%
  ggplot(aes(alpha,total_oil)) +
  geom_point(aes(color=type,shape=type)) +
  geom_smooth(method = "lm",se=FALSE,color = "black")

hops %>%
  ggplot(aes(alpha,total_oil)) +
  geom_point(aes(color=type,shape=type)) +
  geom_smooth(method = "lm",se=FALSE,color = "black")

# use K-means to create three clusters using alpha and oil
hop_type <- hops %>%
  ungroup() %>%
#  filter(type != "Dual Purpose") %>%
  select(-variety) %>%
#  select(alpha_acid_mean,total_oil_mean) %>%
  mutate(cluster = as.character(kmeans(cbind(alpha,total_oil),centers=3)$cluster))


# plot alpha vs oil by bittering type
hop_type %>%
  ggplot(aes(alpha,total_oil,color=cluster,shape=type)) +
  geom_point()

# -----------------------------------------------
# Plot Ranges of components
# make data long first

hops %>% pivot_longer(names_to = "component",cols=where(is.double)) %>%
  filter(component %in% c("alpha","beta")) %>%
  ggplot(aes(component,value,color=type)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(labels=c("Alpha","Beta")) +
#  coord_flip() +
  labs(x="Acids (%)",y="Quantity") +
  facet_grid(~type)

hops %>% pivot_longer(names_to = "component",cols=where(is.double)) %>%
  filter(component %in% c("cohumulone")) %>%
  ggplot(aes(component,value,color=type)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(labels=c("")) +
  labs(x="Co-humulone",y="Percent of Alpha Acids") +
  facet_grid(~type)

hops %>% pivot_longer(names_to = "component",cols=where(is.double)) %>%
  filter(component %in% c("total_oil")) %>%
  ggplot(aes(component,value,color=type)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(labels=c("")) +
  labs(x="Total Oil (mg/100g)",y="Quantity") +
  facet_grid(~type)

# not much here
hops %>% pivot_longer(names_to = "component",cols=where(is.double)) %>%
  filter(!(component %in% c("alpha","beta","total_oil","cohumulone"))) %>%
  ggplot(aes(component,value,color=type)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  facet_grid(~type) +
  coord_flip() +
  labs(x="Oil",y="Percent of Total Oil")

# -----------------------------------------
# show descriptors by popularity

# show style popularity
hops_styles %>%
  count(beer_styles) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(fct_reorder(beer_styles,n),n)) + geom_col() +
  coord_flip() +
  labs(x= "Beer Style",y="Suggested Hop Cultivars")

# show popular descriptions for aromas
hops_aromas %>%
  count(aroma) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(fct_reorder(aroma,n),n)) + geom_col() +
  coord_flip() +
  labs(x= "Aroma",y="Hop Cultivars With that Descriptor")

# Descriptions for hops are like wine. Every judge is different and
# descriptions are often questionable. Consider the classic Saaz and
# a Saaz hybrid.  From the aroma they could not be considered substitutes,
# yet they are.
hops %>% filter(str_detect(variety,"Saaz")) %>%
  select(variety,aroma,substitutions) %>% knitr::kable()

# setup for ML
# Different projects
# 1. are quantitative assays useful for predicting what type of hop,
# bittering, aroma or dual-purpose?
# 2. are subjective descriptors consistant with hop type?
# 3. are quantitative assays useful for predicting what what
# qualitative descriptors will be used?
# An unanswered question is: Is knowledge of the type of hop an
# influence on the reviewers choice of descriptors.

# create subset features to be used
hops_quant_only <- hops %>% select(variety,type,alpha,beta,cohumulone,myrcene,
                               caryophyllene,humulene,farnesene,total_oil)
#add back qualitative features
# create "long" data set
hops_all_features <- hops %>%
  select(variety,type,origin,alpha,beta,
         cohumulone,myrcene,caryophyllene,
         humulene,farnesene,total_oil) %>%
  full_join(hops_aromas,by="variety") %>%
  full_join(hops_styles,by="variety") %>%
  full_join(hops_subs) %>%
  select(-variety)



# --------------------------------------------------------------
# Simple partition models
hops_quant_dt <- rpart(type ~.,method = "class",data = hops_quant_only[,-1])

# visualize decision tree
rpart.plot::rpart.plot(hops_quant_dt)

# view accuracy
wrong <- predict(hops_quant_dt,hops_quant_only,type="class") %>%
  enframe(name= NULL,value = "prediction") %>%
  bind_cols(hops_quant_only) %>%
  group_by(type,prediction) %>%
  tally() %>%
  arrange(desc(n)) %>%
#  expand(type,prediction) %>%
#  full_join(wrong) %>%
  unique %>%
  replace_na(list(n=0)) %>%
  mutate(correct = (type==prediction))

accuracy = wrong %>% group_by(correct) %>% tally(n)



# show model accuracy
wrong %>%  ggplot(aes(type,prediction,size=n,color=correct)) + geom_point() +
  scale_size(range=c(5,30)) +
  scale_color_manual(values=c("red","green")) +
  labs(x="Actual Type",
       y="Predicted Type",
       title="Model Accuracy")
#  geom_label(aes(label=n,size=1))


# create training and test sets
# since alpha is the dominant hop feature make sure the split
# is balanced on alpha.
hops_split <- initial_split(hops_all_features, prob = 0.80, strata = alpha)

hops_train <- training(hops_split)
hops_test <- testing(hops_split)

# Build a model. Since we want to predict a categorical variable,
# hop type, linear regression won't work.  We use random forest.


set.seed(1)
model <- randomForest(type~.,data=hops_quant_only,
                      localImp = TRUE,
#                      replace = FALSE,
#                     maxnodes = 12,
                      importance = TRUE,
                      na.action = na.exclude)
tree_func(model,1)
model
vip::vip(model)

predicted_type <- model$confusion %>% as_tibble(rownames = "actual_type")
tidypredict_fit(model)[[1]]
# need to prune factor variables to top 50
#model <- randomForest(type ~.,data=hops_all_features[,-1],na.action = na.exclude)
getTree(model,labelVar = TRUE)

tree_func(model,1)

# plot random forest tree
library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model,
                      tree_num) {

  # get tree by index
  tree <- randomForest::getTree(final_model,
                                k = tree_num,
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))

  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")

  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))

  # plot
  plot <- ggraph(graph, 'dendrogram') +
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE,
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))

  print(plot)
}
