#Złotek Sebastian 166721, Koczynasz Jonasz 166658 L5
#Wnioskowanie w Warunkach Niepewności - PROJEKT

# Loading the packages
packages <- c("BiocManager", "graph", "RBGL", "Rgraphviz")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")
BiocManager::install("RBGL")

library(readxl)
library(bnlearn)
library(lattice)
library(gRain)
library(gridExtra)
library(gRbase)

# Loading the data
#setwd("C:/Users/Sebastian/Desktop/studia/III rok 2 semestr/Wn_W_War_Niepewnosci_PROJEKT")
df <- read_excel("astma.xlsx")
df <- as.data.frame(df)

variables <- c("sex", "age", "urbanization", "education", "geographic_area", "allergy", "smoke", "sedentary", "asthma")

for (variable in variables) {
  df[[variable]] <- as.factor(df[[variable]])
}

new_names <- c("SEX", "AGE", "URB", "EDU", "GEO", "ALG", "SMK", "SED", "ASTHMA")

# Renaming the variables
names(df) <- new_names

# Printing the updated variable names
names(df)

variables <- c("EDU", "SEX", "ASTHMA")

# Creating frequency tables for variables
for (variable in variables) {
  cat("Frequency table for", variable, ":\n")
  print(table(df[[variable]]))
}

# Checking for missing values in the data frame
cat("Missing value table:\n")
print(table(is.na.data.frame(df)))


# Hill Climbing Algorithm

hc_network <- hc(df)
graphviz.plot(hc_network, shape = "rectangle", 
              highlight = list(nodes = names(df), fill = "yellow"))

# Scores
score(hc_network,data=df,type="bic")
score(hc_network,data=df,type="bde",iss=5)
score(hc_network,data=df,type="aic")

(list <- matrix(c(score(hc_network,data=df,type="bic"),
                    score(hc_network,data=df,type="bde",iss=5),
                    score(hc_network,data=df,type="aic"))))
colnames(list) <- "Score points - siec hc"
rownames(list) <- c("bic", "bde", "aic")
View(list)

# Defining the function to calculate the score
calculate_score <- function(network, data, type, iss = NULL) {
  if (type == "bic") {
    # Calculate BIC score
    score <- score(network, data, type = "bic")
  } else if (type == "bde") {
    # Calculate BDE score
    score <- score(network, data, type = "bde", iss = iss)
  } else if (type == "aic") {
    # Calculate AIC score
    score <- score(network, data, type = "aic")
  } else {
    stop("Invalid score type.")
  }
  
  return(score)
}

# Creating the hc_network object and the df data frame

# Calculating the scores
score_bic <- calculate_score(hc_network, data = df, type = "bic")
score_bde <- calculate_score(hc_network, data = df, type = "bde", iss = 5)
score_aic <- calculate_score(hc_network, data = df, type = "aic")

# Creating the score list matrix
score_list <- matrix(c(score_bic, score_bde, score_aic), nrow = 3, ncol = 1)
colnames(score_list) <- "Score points - hc_network"
rownames(score_list) <- c("bic", "bde", "aic")

# Displaying the score list
View(score_list)


# Checking the model string
model_string <- modelstring(hc_network)
print(model_string)

# Checking the arcs
arcs <- arcs(hc_network)
print(arcs)

# Checking the arc strength
arc_strength <- arc.strength(hc_network, data = df)
print(arc_strength)

# Fitting the network to the data and calculate the probability distribution
distribution <- bn.fit(hc_network, df)
print(distribution)

# Plotting the probability distribution using graphviz
graphviz.chart(distribution, bar.col = "blue", grid = TRUE, type = "barprob")



# MMHC Algorithm

mmhc_network <- mmhc(df)
graphviz.plot(mmhc_network, shape = "rectangle", 
              highlight = list(nodes = names(df), fill = "yellow"))

# Scores
score(mmhc_network,data=df,type="bic")
score(mmhc_network,data=df,type="bde",iss=5)
score(mmhc_network,data=df,type="aic")

(list2 <- matrix(c(score(mmhc_network,data=df,type="bic"),
                  score(mmhc_network,data=df,type="bde",iss=5),
                  score(mmhc_network,data=df,type="aic"))))
colnames(list2) <- "Score points - mmhc_network"
rownames(list2) <- c("bic", "bde", "aic")
View(list2)


# Calculating the scores
score_bic2 <- calculate_score(mmhc_network, data = df, type = "bic")
score_bde2 <- calculate_score(mmhc_network, data = df, type = "bde", iss = 5)
score_aic2 <- calculate_score(mmhc_network, data = df, type = "aic")

# Creating the score list matrix
score_list2 <- matrix(c(score_bic2, score_bde2, score_aic2), nrow = 3, ncol = 1)
colnames(score_list2) <- "Score points - mmhc_network"
rownames(score_list2) <- c("bic", "bde", "aic")

# Displaying the score list
View(score_list2)


# Checking the model string
model_string2 <- modelstring(mmhc_network)
print(model_string2)

# Checking the arcs
arcs2 <- arcs(mmhc_network)
print(arcs2)

# Checking the arc strength
arc_strength2 <- arc.strength(mmhc_network, data = df)
print(arc_strength2)

# Fitting the network to the data and calculate the probability distribution
distribution2 <- bn.fit(mmhc_network, df)
print(distribution2)

# Plotting the probability distribution using graphviz
graphviz.chart(distribution2, bar.col = "blue", grid = TRUE, type = "barprob")




# H2PC Algorithm

h2pc_network <- h2pc(df)
graphviz.plot(mmhc_network, shape = "rectangle", 
              highlight = list(nodes = names(df), fill = "yellow"))

# Scores
score(h2pc_network,data=df,type="bic")
score(h2pc_network,data=df,type="bde",iss=5)
score(h2pc_network,data=df,type="aic")

(list3 <- matrix(c(score(h2pc_network,data=df,type="bic"),
                   score(h2pc_network,data=df,type="bde",iss=5),
                   score(h2pc_network,data=df,type="aic"))))
colnames(list3) <- "Score points - h2pc_network"
rownames(list3) <- c("bic", "bde", "aic")
View(list3)


# Calculating the scores
score_bic3 <- calculate_score(h2pc_network, data = df, type = "bic")
score_bde3 <- calculate_score(h2pc_network, data = df, type = "bde", iss = 5)
score_aic3 <- calculate_score(h2pc_network, data = df, type = "aic")

# Creating the score list matrix
score_list3 <- matrix(c(score_bic3, score_bde3, score_aic3), nrow = 3, ncol = 1)
colnames(score_list3) <- "Score points - h2pc_network"
rownames(score_list3) <- c("bic", "bde", "aic")

# Displaying the score list
View(score_list3)


# Checking the model string
model_string3 <- modelstring(h2pc_network)
print(model_string3)

# Checking the arcs
arcs3 <- arcs(h2pc_network)
print(arcs3)

# Checking the arc strength
arc_strength3 <- arc.strength(h2pc_network, data = df)
print(arc_strength3)

# Fitting the network to the data and calculate the probability distribution
distribution3 <- bn.fit(h2pc_network, df)
print(distribution3)

# Plotting the probability distribution using graphviz
graphviz.chart(distribution3, bar.col = "blue", grid = TRUE, type = "barprob")


# Calculating BIC scores for different networks
networks <- c("hc_network", "h2pc_network", "mmhc_network")
scores <- sapply(networks, function(network) {
  score(get(network), data = df, type = "bic")
})

# Creating the comparison matrix
comparison <- matrix(scores, nrow = length(networks), ncol = 1)
rownames(comparison) <- networks
colnames(comparison) <- "score"

# Displaying the comparison matrix
View(comparison)

# Plotting conditional probability distributions

# Defining the nodes for which you want to plot the distributions
nodes <- c("SEX", "AGE", "URB", "EDU", "GEO", "SMK", "SED", "ALG", "ASTHMA")

# Plotting the barcharts for each node
for (node in nodes) {
  bn.fit.barchart(distribution3[[node]], main = paste("Conditional probability for a node", node),
                  xlab = "Probability", ylab = "Criteria")
}


# Calculating probabilities
probabilities <- list(
  SEX = c(female = sum(df$SEX == "female"), male = sum(df$SEX == "male")) / nrow(df),
  URB = c(medium = sum(df$URB == "medium"), low = sum(df$URB == "low"), high = sum(df$URB == "high")) / nrow(df),
  AGE = c(adult = sum(df$AGE == "adult"), old = sum(df$AGE == "old"), young = sum(df$AGE == "young")) / nrow(df),
  GEO = c(norht = sum(df$GEO == "north"), "south/islands" = sum(df$GEO == "south/islands"), centre = sum(df$GEO == "centre")) / nrow(df),
  SED = c(yes = sum(df$SED == "yes"), no = sum(df$SED == "no")) / nrow(df),
  SMK = c(yes = sum(df$SMK == "yes"), no = sum(df$SMK == "no")) / nrow(df),
  ASTHMA = c(yes = sum(df$ASTHMA == "yes"), no = sum(df$ASTHMA == "no")) / nrow(df),
  EDU = c(high = sum(df$EDU == "high")) / nrow(df),
  ALG = c(no = sum(df$ALG == "no"), yes = sum(df$ALG == "yes")) / nrow(df)
)

# Displaying the probabilities
for (variable in names(probabilities)) {
  cat(variable, ":\n")
  cat("---------------------\n")
  for (category in names(probabilities[[variable]])) {
    prob <- probabilities[[variable]][[category]]
    cat(category, ": ", prob, "\n")
  }
  cat("\n")
}



# Probability that a random person is from south/islands and has asthma 

# Probability of being from "south/islands"
prob_south_islands <- sum(df$GEO == "south/islands")/length(df$GEO)

# Probability of having asthma
prob_asthma <- sum(df$ASTHMA == "yes")/length(df$GEO)

# Probability of being from "south/islands" and having asthma
prob_both <- prob_south_islands * prob_asthma

# Displaying the result
cat("Probability of being from south/islands and having asthma:", prob_both, "\n")
distribution3$GEO
distribution3$ASTHMA

junction <-compile(as.grain(distribution3))
querygrain(junction, nodes = "GEO")$GEO
querygrain(junction, nodes = "ASTHMA")$ASTHMA

# Probability that a randomly selected person that he does not lead a sedentary lifestyle if their level of education is at a high and is an old person

# Calculating the probability
prob_condition <- sum(df$SED == "no" & df$EDU == "high" & df$AGE == "old") / sum(df$EDU == "high" & df$AGE == "old")

# Displaying the probability
cat("The probability of SED = no given EDU = high and AGE = old: ", prob_condition, "\n")

distribution3$SED
distribution3$AGE

condition <- setEvidence(junction, nodes = c("EDU", "AGE"), states=c("high", "old"))
querygrain(condition, nodes = "SED")$SED
querygrain(condition, nodes = "AGE")$AGE


#Probability that a person is a men, is smoking, doesn't have asthma and lives in the north with a low urbanization

querygrain(junction, nodes = c("SEX","SMK", "ASTHMA","GEO", "URB"), type="joint")


#Probability that a person is a young women, has asthma and allergy, lives in a high urbanization

condition2 <- setEvidence(junction,nodes=c("AGE","SEX","ASTHMA","URB"),states=c("young","female","yes","high"))
querygrain(condition2, nodes = c("ALG"))$ALG


#Hypothetical tests

#Testing the hypothesis of the relationship between EDU and URB variables 
ci.test("EDU","URB",test="x2",data=df) 

#Testing the hypothesis of the relationship between URB and AGE variables
ci.test("URB","AGE",test="x2",data=df) 

#Testing the hypothesis of the relationship between GEO and ASTHMA variables
ci.test("GEO","ASTHMA",test="x2",data=df) 

#Testing the hypothesis of the correlation of EDU and AGE variables under the condition of SCM
ci.test("EDU","SMK","AGE",test="x2",data=df) 

#Testing the hypothesis of dependency between variables "ALG" and "ASTHMA"
ci.test("ALG", "ASTHMA",test="x2",data=df)

