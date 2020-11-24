# Hop analysis demo

library(tidyverse)
library(tidytext)
library(tidymodels)
library(tidypredict)
library(randomForest)
library(pdftools)
library(googlesheets4)

# source: brewcabin.com and yakima chief hops
sheet <- "https://docs.google.com/spreadsheets/d/1qNedHmXqhqzpqt1vgzjUJ0NYtY_7Lov_Vue_k_hZPf8/edit#gid=1675045246"
# public sheet so we don't neet authorization
googlesheets4::gs4_deauth()
hops_raw <- read_sheet(sheet,sheet="Hops (Uploaded)",
                       col_types = "ccciiiiiiiiiiiiiiccccccc") %>%
  mutate(Origin = as.factor(Origin),Type = as.factor(Type))

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

hops <- hops_raw %>%
  select(-starts_with("sources")) %>%
  rename_with(tidy_names)

hops <- hops %>% group_by(variety) %>%
  mutate(alpha=mean(c_across(contains("alpha"))),
         beta=mean(c_across(contains("beta"))),
         cohumulone=mean(c_across(contains("co-humulone"))),
         myrcene=mean(c_across(contains("myrcene"))),
         caryophyllene=mean(c_across(contains("caryophyllene"))),
         humulene=mean(c_across(contains("humulene"))),
         farnesene = str_parse_range(farnesene_oil_fraction),
         total_oil=mean(c_across(starts_with("total_oil")))
  ) %>%
  ungroup()



# unhelpful aroma descriptors.
# "noble" is excluded beacuse the word does NOT appear in the
# aroma words of actual noble hop varieties
brew_stop_words <- tibble(word = c("noble","aromas","aroma","mild","pleasant","characteristics",
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

style_stem_words <-
  tribble(
    ~term, ~stem,
    "india pale ale","ipa",
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


# --------------------------------------------
# get all the aroma words
hops_aromas <- hops %>%
  select(variety,aroma) %>%
  mutate(aroma = tolower(aroma)) %>%
  tidytext::unnest_tokens("aroma","aroma") %>%
  filter(!(aroma %in% stop_words$word)) %>%
  filter(!(aroma %in% brew_stop_words$word)) %>%
  mutate(aroma = stem(aroma)) %>%
  mutate(aroma = as.factor(aroma)) %>%
  {.}
# order factor levels by frequency
levels(hops_aromas$aroma) <-
  hops_aromas %>%
  count(aroma) %>%
  mutate(aroma = fct_reorder(aroma,n,.desc = TRUE)) %>%
  pull(aroma) %>%
  levels()

# --------------------------------------------
# get all the style names
# and bring some consistency to them.  Ugh.
hops_styles <- hops %>%
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
# order factor levels by frequency
levels(hops_styles$beer_styles) <-
  hops_styles %>%
  count(beer_styles) %>%
  mutate(beer_styles = fct_reorder(beer_styles,n,.desc = TRUE)) %>%
  pull(beer_styles) %>%
  levels()

# --------------------------------------------
# get all the substitute names
hops_subs <- hops %>%
  select(variety,substitutions) %>%
  tidytext::unnest_tokens("substitutions","substitutions") %>%
  filter(!(substitutions %in% stop_words$word)) %>%
  {.}

# --------------------------------------------
#  Exploratory data analysis
# show origins of hops
hops %>% ggplot(aes(origin,fill=type)) + geom_bar() +
  coord_flip()

hops %>% select(origin,type) %>% table()

# plot alpha vs oil by bittering type
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
  ggplot(aes(alpha,total_oil,color=cluster)) +
  geom_point()

# show style popularity
hops_styles %>%
  count(beer_styles) %>%
  arrange(desc(n)) %>%
  mutate(beer_styles = as_factor(beer_styles)) %>%
  head(15) %>%
  ggplot(aes(beer_styles,n)) + geom_col() +
  coord_flip() +
  labs(x= "Beer Style",y="Suggested Hop Cultivars")

# show popular descriptions for aromas
hops_aromas %>%
  count(aroma) %>%
  arrange(desc(n)) %>%
  mutate(aroma = fct_reorder(aroma,n,.desc = TRUE))

 head(20) %>%
 ggplot(aes(aroma,n)) + geom_col() +
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
hops_quant_only <- hops %>% select(variety,type,origin,alpha,beta,cohumulone,myrcene,
                               caryophyllene,humulene,farnesene,total_oil)
#add back qualitative features
# create "long" data set
hops_all_features <- hops_quant_only %>%
  full_join(hops_aromas,by="variety") %>%
  full_join(hops_styles,by="variety") %>%
  full_join(hops_subs)


# create training and test sets
# since alpha is the dominant hop feature make sure the split
# is balanced on alpha.
hops_split <- initial_split(hops_all_features, prob = 0.80, strata = alpha)

hops_train <- training(hops_split)
hops_test <- testing(hops_split)

# Build a model. Since we want to predict a categorical variable,
# hop type, linear regression won't work.  We use random forest.

model <- randomForest(type ~.,data=hops_all_features[,-1],na.action = na.exclude)

model$confusion

model <- randomForest(type ~.,data=hops_quant_only[,-1],na.action = na.exclude)
