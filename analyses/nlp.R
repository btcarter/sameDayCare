# packages ####
library(dplyr)
library(tidytext)
library(tm)
library(widyr)
library(topicmodels)
library(ggplot2)
library(progress)


# paths
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "data")

# data

df.flat.name <- file.path(data.dir.path, "sdc.flat.xlsx")

df.flat <- readxl::read_xlsx(df.flat.name)


# nlp of Reason for Visit ####
df.nlp <- df.flat %>% 
  select(
    EncounterID,
    ReasonForVisit_all
  )

# document term matrix
text.matrix <- df.nlp %>% 
  select(
    EncounterID,
    ReasonForVisit_all
  ) %>% 
  unnest_tokens(
    output = "word",
    input = ReasonForVisit_all,
    token = "words"
  ) %>%
  anti_join(stop_words) %>% # remove meaningless words
  mutate(word = SnowballC::wordStem(word)) %>%  # stem words
  count(EncounterID, word) %>%
  cast_dtm(
    document = EncounterID,
    term = word,
    value = n,
    weighting = tm::weightTf
  )

# for loop for multiple groups
lda.list <- list()

iterations <- c(90:110)

pb <- progress_bar$new(
  format = "  Running LDA [:bar] :percent eta: :eta\n Elapsed :elapsedfull",
  total = length(iterations), clear = FALSE)

for (i in iterations){
  
  pb$tick()
  
  text.lda <- LDA(
    text.matrix,
    k = i,
    method = "Gibbs",
    control = list(seed = 1111)
  )
  
  text.betas <- tidy(text.lda, matrix = "beta") # how related are the words and which topic are they?
  
  text.gammas <- tidy(text.lda, matrix = "gamma") %>%
    group_by(document) %>%
    arrange(
      desc(gamma)
    ) %>%
    slice(1) %>%
    group_by(topic)
  
  text.perplexity <- perplexity(text.lda, text.matrix)
  
  name <- paste("run_", i, sep = "")
  
  # lda.list[[name]] <- list(
  #   "lda" = text.lda,
  #   "betas" = text.betas,
  #   "gammas" = text.gammas
  # )
  
  lda.list$perplexity <- data.frame("k" = integer(),
                                    "perplexity" = numeric())
  df.perp <- data.frame("k" = i, 
                        "perplexity" = text.perplexity)
  
  lda.list$perp_df <- rbind(lda.list$perp_df, df.perp)
  print(lda.list$perp_df) # can comment this line out if you want.
  
}

perp.plot <- ggplot(lda.list$perp_df,
       aes(k, perplexity)) +
  geom_point() +
  geom_line()


jpeg(
  file.path(out.dir.path, "perplexityPlot_ReasonForVisit.jpeg")
)
perp.plot
dev.off()


# saveRDS(lda.list,
#         file = file.path(out.dir.path, "ldaList.rds"))
