
# https://www.lendingclub.com/resource-center/personal-loan/what-is-debt-consolidation
# https://www.openintro.org/data/index.php?data=loans_full_schema

# ----1. LECTURE  DES DONNEES--- ####

Data<- read.csv("C:/Users/USER/Downloads/Loan_club_group3 (1).csv",header=T, sep=",")
sum(is.na(Data))

#/ CREATION DE LA VARIABLE DEFAUT 
Data$default <- ifelse(Data$loan_status == "Charged Off", 1, 0)

#/ CONVERSIONS EN FACTEUR
# EMPLOYMENT LENGTH
Data$emp_length_fact <- factor(
  Data$emp_length,
  levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years",
             "6 years", "7 years", "8 years", "9 years", "10+ years", "n/a")
)

library(dplyr)

Data$emp3 <- case_when(
  Data$emp_length_fact %in% c("< 1 year", "1 year", "2 years", "3 years", "4 years") ~ "< 5 years",
  Data$emp_length_fact %in% c("5 years", "6 years", "7 years", "8 years", "9 years", "10+ years") ~ ">= 5 years",
  Data$emp_length_fact == "n/a" ~ "unknown"
)
Data$emp3 <- factor(Data$emp3, levels = c("< 5 years", ">= 5 years", "unknown"))

# LOAN STATUS
Data$loan_status <- factor(Data$loan_status)


# HOME OWNERSHIP 
Data$home_ownership <- factor(Data$home_ownership)

# REGION
Data$addr_state <- factor(Data$addr_state)
Data$region <- with(Data, 
                    ifelse(addr_state %in% c("CT","ME","MA","NH","RI","VT","NJ","NY","PA"), "Nord-Est",
                           ifelse(addr_state %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD"), "Midwest",
                                  ifelse(addr_state %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX"), "Sud",
                                         ifelse(addr_state %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA"), "Ouest", NA)))))
Data$region <- factor(Data$region, levels = c("Sud", "Nord-Est", "Midwest", "Ouest"))

Data$term<- factor(Data$term)
Data$verification_status<- factor(Data$verification_status)


Data$purpose_group <- with(Data,
                           ifelse(purpose == "debt_consolidation", "Consolidation",
                                  ifelse(purpose %in% c("credit_card","car","major_purchase","wedding"), "Credit_Consommation",
                                         "Travaux_Autres")))


Data$purpose_group <- as.factor(Data$purpose_group)



#-----2. ANALYSES EXPLORATOIRES ---####

# DISTRIBUTION DU MONTANT DU PRET 

library(ggplot2)
library(patchwork)

p1 <- ggplot(Data, aes(x = loan_amnt)) +
  geom_histogram(
    bins = 50,
    fill = "#1E90FF",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    color = "#FF0000",
    linewidth = 1
  ) +
  labs(
    title = "Distribution du montant du prêt",
    x = "Montant du prêt ($)",
    y = "Fréquence"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

p2 <- ggplot(Data, aes(y = loan_amnt, x = "")) +
  geom_boxplot(
    fill = "#FF0000",
    color = "#1E90FF",
    outlier.color = "black",
    outlier.size = 1.5
  ) +
  labs(
    title = "Boxplot du montant du prêt",
    x = "",
    y = "Montant du prêt ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p1 + p2 + plot_layout(widths = c(2, 1))

wilcox.test(loan_amnt ~ loan_status, data = Data)

# DISTRIBUTION DU STATUT DU PRET SELON LE MONTANT 
library(ggplot2)
ggplot(Data, aes(x = loan_status, y = loan_amnt, fill = loan_status)) +
  geom_boxplot() +
  labs(
    title = "Montant du prêt en fonction du statut du prêt",
    x = "Statut du prêt",
    y = "Montant du prêt"
  ) +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()

wilcox.test(loan_amnt ~ loan_status, data = Data)


# ANCIENNETE VS DEFAUT 

for(year in levels(Data$emp_length_fact)) {
  cat(year, "years in this position:\n")
  tab <- table(Data$loan_status[Data$emp_length_fact == year])
  print(tab / sum(tab))
  cat('==========================================\n') }

prop_tab <- prop.table(table(Data$emp3, Data$loan_status), margin = 1)

barplot(
  t(prop_tab),
  beside = TRUE,
  col = c("red", "green"),
  legend.text = colnames(prop_tab),
  main = " statut du pret selon anciennete(en %)",
  xlab = "Anciennete",
  ylab = "Proportion")

chisq.test(table(Data$emp3, Data$loan_status))

# REVENU  ANNUEL VS DEFAUT 
boxplot(
  annual_inc ~ loan_status,
  data = Data,
  main = "Distribution du revenu annuel selon le statut du prêt",
  xlab = "Statut du prêt",
  ylab = "Revenu annuel",
  col = c("red", "green"),
  ylim = c(0, 200000))

wilcox.test(annual_inc ~ loan_status, data = Data)


#VERFICATION DU STATUT VS STATUT DU PRET
library(ggplot2)
library(dplyr)

library(dplyr)
library(ggplot2)

df_verif <- Data %>%
  group_by(verification_status, loan_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(verification_status) %>%
  mutate(pct = n / sum(n) * 100)

tab <- table(Data$verification_status, Data$loan_status)
chi <- chisq.test(tab)

ggplot(df_verif, aes(x = verification_status, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(pct, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Statut de prêt selon vérification du revenu", 
       x = "Statut de vérification", 
       y = "Pourcentage d'emprunteurs") +
  scale_fill_manual(values = c("red", "green")) +
  annotate("text", x = 1.5, y = -5, 
           label = paste0("p-value Chi² = ", signif(chi$p.value, 3)), hjust = 0) +
  theme_minimal() +
  ylim(0, max(df_verif$pct) + 10)

# VISEE DU PRET
library(ggplot2)
library(dplyr)

# Chi²
tab <- table(Data$purpose_group, Data$loan_status)
chi <- chisq.test(tab)

# Préparer les données
df_purpose <- Data %>%
  group_by(purpose_group, loan_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(purpose_group) %>%
  mutate(pct = n / sum(n) * 100)

# Graphique
ggplot(df_purpose, aes(x = purpose_group, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Statut de prêt selon le motif du prêt", 
       x = "Motif du prêt", 
       y = "Pourcentage par catégorie") +
  scale_fill_manual(values = c("red", "green")) +
  annotate("text", x = 2.5, y = -5, label = paste0("p-value Chi² = ", signif(chi$p.value, 3)), hjust = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(df_purpose$pct) + 15)


# STATUT DU PRET VS DTI 
library(ggplot2)
library(dplyr)

ggplot(Data, aes(x = dti)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white", alpha = 0.7) +
  geom_density(aes(y = 30 * ..count..), color = "black", size = 1) +
  labs(title = "Distribution du DTI", x = "DTI (%)", y = "Nombre d’emprunteurs") +
  theme_minimal()

wilcox.test(dti ~ loan_status, data = Data)

library(dplyr)
library(ggplot2)


# PROPRIETE DE MAISON VS STATUT DU PRET 

library(ggplot2)
library(dplyr)

# Calcul des % par catégorie de home_ownership et statut de prêt
df_home <- Data %>%
  group_by(home_ownership, loan_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(home_ownership) %>%
  mutate(pct = n / sum(n) * 100)

# Test du Chi²
chi_home <- chisq.test(table(Data$home_ownership, Data$loan_status))

# Graphique
ggplot(df_home, aes(x = home_ownership, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(pct, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  labs(title = "Statut de prêt selon propriété de maison",
       x = "Propriété",
       y = "Pourcentage par catégorie") +
  scale_fill_manual(values = c(
    "Charged Off" = "red",
    "Fully Paid" = "green",
    "Current" = "green",
    "In Grace Period" = "green"
  )) +
  annotate("text",
           x = ceiling(length(unique(df_home$home_ownership))/2),
           y = -5,
           label = paste0("p-value Chi² = ", signif(chi_home$p.value, 3)),
           hjust = 0.5) +
  theme_minimal() +
  ylim(0, max(df_home$pct) + 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# STATUT DU PRET VS VISEE DU PRET
library(ggplot2)
library(dplyr)

df_purpose <- Data %>%
  group_by(purpose, loan_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(purpose) %>%
  mutate(pct = n / sum(n) * 100)

chisq.test(table(Data$purpose, Data$loan_status))



# VS TERM
term_loan_table <- table(Data$term, Data$loan_status)
term_loan_prop <- prop.table(term_loan_table, margin = 1)
barplot(t(term_loan_prop),
        main = "Répartition du statut de prêt selon la durée (term)",
        xlab = "Durée du prêt",
        ylab = "Proportion",
        col = c("red", "green"),
        legend.text = c("Charged Off", "Fully Paid"),
        args.legend = list(x = "topright"),
        cex.names = 0.8)
chisq.test(term_loan_table)



# STATAUT DU PRET VS ETAT 
# --- TABLEAU DE CONTINGENCE ---
state_loan_table <- table(Data$addr_state, Data$loan_status)

# --- CALCUL DE LA PROPORTION DE DÉFAUT PAR ÉTAT ---
prop_default <- state_loan_table[, "Charged Off"] / rowSums(state_loan_table)

# --- TRI DÉCROISSANT SELON LA PROPORTION DE DÉFAUT ---
sorted_states <- names(sort(prop_default, decreasing = TRUE))
state_loan_table <- state_loan_table[sorted_states, ]

# --- TABLEAU DE PROPORTION PAR ÉTAT ---
state_loan_prop <- prop.table(state_loan_table, margin = 1)

# --- BARPLOT EMPILÉ EN POURCENTAGE ---
barplot(t(state_loan_prop),
        main = "Proportion des statuts de prêts par État (classés par défaut décroissant)",
        xlab = "État",
        ylab = "Proportion",
        col = c("red", "green"), 
        legend.text = c("Charged Off", "Fully Paid"),
        args.legend = list(x = "topright"),
        cex.names = 0.6,
        las = 2)
chisq.test(table(Data$loan_status, Data$addr_state))

# --- TOP 10 des États avec le plus de défauts ---
top10 <- head(sort(prop_default, decreasing = TRUE), 10)

# --- TABLE filtrée pour ces États ---
state_loan_table_top10 <- state_loan_table[names(top10), ]
state_loan_prop_top10 <- prop.table(state_loan_table_top10, margin = 1)

# --- BARPLOT ---
barplot(t(state_loan_prop_top10),
        main = "Top 10 États avec le plus fort taux de défaut",
        xlab = "État",
        ylab = "Proportion",
        col = c("red", "green"),
        legend.text = c("Charged Off", "Fully Paid"),
        args.legend = list(x = "topright"),
        cex.names = 0.8,
        las = 2)
# --- TOP 10 États les plus fiables (taux de défaut le plus bas) ---
bottom10 <- tail(sort(prop_default, decreasing = TRUE), 10)
round(bottom10 * 100, 1)


library(dplyr)
library(purrr)
library(tidyr)

# REGION VS LOAN STATUS 
library(dplyr)
library(ggplot2)

region_loan_table <- table(Data$region, Data$loan_status)
region_loan_prop <- prop.table(region_loan_table, margin = 1)

barplot(t(region_loan_prop),
        main = "Proportion des statuts de prêts par région",
        xlab = "Région",
        ylab = "Proportion",
        col = c("red", "green"),
        legend.text = c("Charged Off", "Autres"),
        args.legend = list(x = "topright"),
        cex.names = 0.8,
        las = 2)

chisq.test(table(Data$loan_status, Data$region))


# VARIABLES À RÉSUMER VS STATUT DE PRET 
numeric_vars <- c("delinq_2yrs","inq_last_6mths","pub_rec","pub_rec_bankruptcies",
                  "acc_now_delinq","num_accts_ever_120_pd",
                  "avg_cur_bal","num_actv_bc_tl","num_bc_sats",
                  "chargeoff_within_12_mths","delinq_amnt")

# CALCUL DES STATISTIQUES
summary_df <- map_dfr(numeric_vars, function(var) {
  Data %>%
    group_by(loan_status) %>%
    summarise(
      mean = mean(.data[[var]]),
      median = median(.data[[var]]),
      sd = sd(.data[[var]]),
      min = min(.data[[var]]),
      max = max(.data[[var]])
    ) %>%
    dplyr::mutate(variable = var) %>%
    dplyr::select(loan_status, variable, everything())
})

print(summary_df)

# VARIABLES À TESTER 
vars_to_test <- c("delinq_2yrs", "inq_last_6mths", "pub_rec", "acc_now_delinq",
                  "avg_cur_bal", "chargeoff_within_12_mths", "delinq_amnt",
                  "num_accts_ever_120_pd", "num_actv_bc_tl", "num_bc_sats",
                  "pub_rec_bankruptcies", "term", "emp3")

# TESTS D'ASSOCIATION
results <- map_dfr(vars_to_test, function(var) {
  x <- Data[[var]]
  
  if(is.numeric(x)){
    test_res <- wilcox.test(x ~ loan_status, data = Data)
    tibble(variable = var, type = "numeric", test = "Wilcoxon", p_value = test_res$p.value)
  } else {
    x <- as.factor(x)
    tab <- table(x, Data$loan_status)
    if(all(dim(tab) > 1)){
      test_res <- chisq.test(tab)
      tibble(variable = var, type = "factor", test = "Chi²", p_value = test_res$p.value)
    } else {
      NULL
    }
  }
}) %>%
  arrange(p_value)

# COMBINAISON SUMMARY + P-VALUE
summary_results <- summary_df %>%
  left_join(results, by = c("variable")) %>%
  dplyr::select(loan_status, variable, mean, median, sd, min, max, test, p_value)

summary_results


# # --- SYNTHESE  ---
# PARMI LES VARIABLES FINANCIÈRES LIÉES À L’HISTORIQUE DE CRÉDIT, SEULES CERTAINES 
#SONT SIGNIFICATIVEMENT ASSOCIÉES AU RISQUE DE DÉFAUT :
# 1. inq_last_6mths : PLUS DE DEMANDES DE CRÉDIT RÉCENTES → PLUS DE RISQUE DE DÉFAUT
# 2. avg_cur_bal : SOLDE COURANT MOINS ÉLEVÉ → PLUS DE RISQUE DE DÉFAUT
# 3. num_actv_bc_tl : NOMBRE DE COMPTES CRÉDITS ACTIFS LÉGÈREMENT PLUS ÉLEVÉ → RISQUE ACCRU
# LES AUTRES VARIABLES (retards passés, incidents publics, comptes en retard, montants de retard)
#NE SONT PAS SIGNIFICATIVES

# ASSOCIATIONS ENTRE LES VARIABLES QUANTITATIVES 


library(ggplot2)
library(tidyr)
library(dplyr)

quant_vars <- c("loan_amnt", "annual_inc", "dti",
                "delinq_2yrs", "inq_last_6mths", "pub_rec", "acc_now_delinq",
                "avg_cur_bal", "chargeoff_within_12_mths", "delinq_amnt",
                "num_accts_ever_120_pd", "num_actv_bc_tl", "num_bc_sats",
                "pub_rec_bankruptcies")

df_quant <- Data[, quant_vars]

cor_mat <- cor(df_quant, use = "complete.obs")

cor_df <- as.data.frame(as.table(cor_mat))

ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0,
                       limits = c(-1,1), name = "Corrélation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Heatmap des corrélations entre variables quantitatives",
       x = "", y = "")

# : CORRELATIONS FORTES 
#pub_rec et pub_rec_bankruptcies = 0.862
#num_actv_bc_tl et num_bc_sats = 0.852


# ASSOCIATIONS ENTRE LES VARIABLES QUALITATIVES

vars <- c("term", "home_ownership", "emp3", "purpose")

# Fonction pour vérifier tableau et lancer chi²
test_chi2 <- function(var1, var2, data) {
  tab <- table(data[[var1]], data[[var2]])
  
  cat("Tableau croisé :", var1, "vs", var2, "\n")
  print(tab)
  
  low_counts <- which(tab < 5, arr.ind = TRUE)
  if (length(low_counts) > 0) {
    cat("Certaines cases ont moins de 5 observations, p-value simulée\n")
    test <- chisq.test(tab, simulate.p.value = TRUE)
  } else {
    test <- chisq.test(tab)
  }
  
  print(test)
  cat("\n----------------------------\n")
}

# Boucle sur toutes les combinaisons de variables
for(i in 1:(length(vars)-1)) {
  for(j in (i+1):length(vars)) {
    test_chi2(vars[i], vars[j], Data)}}

# LES ASSO LES PLUS FORTES 
# home_ownership vs emp3 (Chi² = 578.58)

#term vs home_ownership (Chi² = 282.03)


#-----3. DISCRETISATION -----------

# RETARDS SUR 2 ANS
Data$delinq_2yrs <- factor(ifelse(Data$delinq_2yrs > 0, "1+", "0"),
                           levels = c("0","1+"))

# DEMANDES DE CREDIT SUR 6 MOIS
Data$inq_last_6mths_bin <- factor(ifelse(Data$inq_last_6mths > 0, "1+", "0"),
                                  levels = c("0","1+"))

# RATIO DETTE/REVENU (DTI)
q_dti <- quantile(Data$dti, probs = seq(0,1,0.25), na.rm = TRUE)
labels_dti <- c(
  paste0("[", round(q_dti[1],1), " ; ", round(q_dti[2],1), "]"),
  paste0("(", round(q_dti[2],1), " ; ", round(q_dti[3],1), "]"),
  paste0("(", round(q_dti[3],1), " ; ", round(q_dti[4],1), "]"),
  paste0("(", round(q_dti[4],1), " ; ", round(q_dti[5],1), "]")
)
Data$dti_q <- cut(Data$dti, breaks = q_dti, include.lowest = TRUE, labels = labels_dti)
Data$dti_q <- factor(Data$dti_q, levels = labels_dti)

# PUBREC
Data$pub_rec_bin <- factor(ifelse(Data$pub_rec == 0, "0", "1+"), levels = c("0", "1+"))

# COMPTES RADIES SUR 12 MOIS
Data$chargeoff_bin <- factor(ifelse(Data$chargeoff_within_12_mths == 0, "0", "1+"), 
                             levels = c("0","1+"))

# NB DE COMPTES AYANT ETE EN RETARD > 120 JOURS
Data$num_accts_ever_120_pd_cat <- factor(ifelse(Data$num_accts_ever_120_pd == 0, "0", "1+"),
                                         levels = c("0","1+"))

# NB DE COMPTES ACTIFS
Data$num_actv_bc_tl_cat <- factor(cut(Data$num_actv_bc_tl,
                                      breaks = c(-1,2,5,Inf),
                                      labels = c("0-2","3-5","6+")))

# NB DE COMPTES DE CREDIT SATISFAITS
Data$num_bc_sats <- cut(Data$num_bc_sats,
                        breaks = c(-1,3,6,Inf),
                        labels = c("0-3","4-6","7+"))
Data$num_bc_sats <- factor(Data$num_bc_sats, levels = c("0-3","4-6","7+"))

# MONTANT DU PRET EN QUARTILES
q <- quantile(Data$loan_amnt, probs = seq(0, 1, 0.25), na.rm = TRUE)
q <- round(q, 0)
labels_q <- c(
  paste0("[", q[1], " ; ", q[2], "]"),
  paste0("(", q[2], " ; ", q[3], "]"),
  paste0("(", q[3], " ; ", q[4], "]"),
  paste0("(", q[4], " ; ", q[5], "]")
)
Data$loan_amnt_q <- cut(Data$loan_amnt, breaks = q, include.lowest = TRUE, labels = labels_q)
Data$loan_amnt_q <- factor(Data$loan_amnt_q, levels = labels_q)

# REVENU ANNUEL EN QUARTILES
q_inc <- quantile(Data$annual_inc, probs = seq(0, 1, 0.25), na.rm = TRUE)
q_inc <- round(q_inc, 0)
labels_inc <- c(
  paste0("[", format(q_inc[1], big.mark=" "), " ; ", format(q_inc[2], big.mark=" "), "]"),
  paste0("(", format(q_inc[2], big.mark=" "), " ; ", format(q_inc[3], big.mark=" "), "]"),
  paste0("(", format(q_inc[3], big.mark=" "), " ; ", format(q_inc[4], big.mark=" "), "]"),
  paste0("(", format(q_inc[4], big.mark=" "), " ; ", format(q_inc[5], big.mark=" "), "]")
)
Data$annual_inc_q <- cut(Data$annual_inc, breaks = q_inc, include.lowest = TRUE, labels = labels_inc)
Data$annual_inc_q <- factor(Data$annual_inc_q, levels = labels_inc)

# SOLDE MOYEN DES COMPTES EN QUARTILES
q_bal <- quantile(Data$avg_cur_bal, probs = seq(0, 1, 0.25), na.rm = TRUE)
q_bal <- round(q_bal, 0)
labels_bal <- c(
  paste0("[", format(q_bal[1], big.mark=" "), " ; ", format(q_bal[2], big.mark=" "), "]"),
  paste0("(", format(q_bal[2], big.mark=" "), " ; ", format(q_bal[3], big.mark=" "), "]"),
  paste0("(", format(q_bal[3], big.mark=" "), " ; ", format(q_bal[4], big.mark=" "), "]"),
  paste0("(", format(q_bal[4], big.mark=" "), " ; ", format(q_bal[5], big.mark=" "), "]")
)
Data$avg_cur_bal_q <- cut(Data$avg_cur_bal, breaks = q_bal, include.lowest = TRUE, labels = labels_bal)
Data$avg_cur_bal_q <- factor(Data$avg_cur_bal_q, levels = labels_bal)

#
Data$pub_rec_bankruptcies <- ifelse(Data$pub_rec_bankruptcies == 0, "0", "1+")
Data$pub_rec_bankruptcies <- factor(Data$pub_rec_bankruptcies, levels = c("0", "1+"))

#-----4. MODIFICATION DES REF -----

Data$annual_inc_q <- relevel(Data$annual_inc_q, ref = "(86 000 ; 1e+06]")
Data$loan_amnt_q <- relevel(Data$loan_amnt_q, ref = "[1000 ; 8325]")
Data$avg_cur_bal_q <- relevel(Data$avg_cur_bal_q, ref = "(20 286 ; 329 278]")
Data$region <- relevel(Data$region, ref = "Ouest")
Data$chargeoff_bin <- relevel(as.factor(Data$chargeoff_bin), ref = "1+")

#-----5. SEPARATION EN TRAIN & TEST SETS --- ####

# 0. CHARGER LES PACKAGES
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")

library(dplyr)
library(caret)

# 1. SPLIT TRAIN/TEST (60/40)
set.seed(123)
train_index <- createDataPartition(Data$default, p = 0.6, list = FALSE)
train_data <- Data[train_index, ]  
test_data  <- Data[-train_index, ] 

# 2. CALCUL DES POIDS DES CLASSES
prop <- prop.table(table(train_data$default))
w_pos <- 1 / prop["1"]
w_neg <- 1 / prop["0"]
train_weights <- ifelse(train_data$default == 1, as.numeric(w_pos), as.numeric(w_neg))

cat("=== POIDS DES CLASSES ===\n")
cat("LONGUEUR :", length(train_weights), "\n")
cat("NOMBRE DE NA :", sum(is.na(train_weights)), "\n")
cat("TABLE DES POIDS :\n")
print(table(train_weights))


#-----6. SELECTION DES LOGITS  ####

# RELEVEL DES FACTEURS POUR LE MODELE
train_data$pub_rec_bin   <- relevel(as.factor(train_data$pub_rec_bin), ref = "1+")

train_data$purpose_group <- relevel(as.factor(train_data$purpose_group), ref = "Credit_Consommation")

train_data$num_bc_sats<- relevel(as.factor(train_data$num_bc_sats), ref = "7+")

train_data$emp3<- relevel(as.factor(train_data$emp3), ref = ">= 5 years")

train_data$verification_status <- relevel(as.factor(train_data$verification_status), ref = "Verified")

train_data$region <- relevel(as.factor(train_data$region), ref = "Ouest")
train_data$avg_cur_bal_q <- relevel(as.factor(train_data$avg_cur_bal_q), ref = "(20 286 ; 329 278]")

train_data$annual_inc_q <- relevel(as.factor(train_data$annual_inc_q), ref = "(86 000 ; 1e+06]")
train_data$chargeoff_bin <- relevel(as.factor(train_data$chargeoff_bin), ref = "1+")
train_data$verification_status <- relevel(as.factor(train_data$verification_status), ref = "Not Verified")



# MODELE 1 : ORIGINAL AVEC POIDS
mod_orig <- glm(default ~ term 
                + home_ownership 
                + verification_status 
                + purpose_group
                + delinq_2yrs
                + num_bc_sats
                + emp3
                + region
                + inq_last_6mths_bin 
                + dti_q
                + pub_rec_bin 
                + chargeoff_bin
                + num_accts_ever_120_pd_cat
                + num_actv_bc_tl_cat
                + loan_amnt_q 
                + annual_inc_q 
                + avg_cur_bal_q
                ,
                data = train_data,
                family = binomial,
                weights = train_weights)
summary (mod_orig)


# STEPWISE
library(MASS)
mod_base <- glm(default ~ 1, data = train_data, family = binomial, weights = train_weights)

#STEPWISE BIDIRECTIONEL
mod_step <- step(mod_base,
                 scope = list(lower = mod_base, upper = mod_orig),
                 direction = "both")
summary(mod_step)

# COMAPRAISON DES 2 MODELES 

if (!require(stargazer)) {
  install.packages("stargazer")
  library(stargazer)
} else {
  library(stargazer)
}

stargazer(mod_orig, mod_step,
          type = "text",          
          title = "Comparaison : Modèle Original vs Stepwise",
          dep.var.labels = "Default",
          no.space = TRUE, 
          single.row = TRUE)



# ON RETIENT LE MODELE STEPWISE 
######## ON AJOUTE AU MODELE STEP   DES INTERRACTIONS
# TERM X LOAN AMOUNT 
mod_step1 <- glm(default ~ term*loan_amnt_q
                 + home_ownership 
                 + verification_status 
                 + purpose_group
                 + num_bc_sats
                 + emp3
                 + region
                 + inq_last_6mths_bin 
                 + dti_q
                 + chargeoff_bin
                 + num_accts_ever_120_pd_cat
                 + num_actv_bc_tl_cat
                 
                 + annual_inc_q 
                 + avg_cur_bal_q
                 ,
                 data = train_data,
                 family = binomial,
                 weights = train_weights)
summary (mod_step1)

#avg_cur_bal_q × num_actv_bc_tl_cat
mod_step2<- glm(default ~ term 
                + home_ownership 
                + verification_status 
                + purpose_group
                + num_bc_sats
                + emp3
                + region
                + inq_last_6mths_bin 
                + dti_q
                + chargeoff_bin
                + num_accts_ever_120_pd_cat
                
                + loan_amnt_q 
                + annual_inc_q 
                + avg_cur_bal_q*num_actv_bc_tl_cat
                ,
                data = train_data,
                family = binomial,
                weights = train_weights)
summary (mod_step2)
# emp3 × home_ownership
mod_step3<- glm(default ~ term 
                
                + verification_status 
                + purpose_group
                + num_bc_sats
                + emp3*home_ownership
                + region
                + inq_last_6mths_bin 
                + dti_q
                + chargeoff_bin
                + num_accts_ever_120_pd_cat
                + num_actv_bc_tl_cat
                + loan_amnt_q 
                + annual_inc_q 
                + avg_cur_bal_q
                ,
                data = train_data,
                family = binomial,
                weights = train_weights)
summary (mod_step3)


#
# TERM X LOAN AMOUNT X EMPX HOMEOWNESHIP
mod_step4<- glm(default ~ term*loan_amnt_q
                + home_ownership*emp3
                + verification_status 
                + purpose_group
                + num_bc_sats
                
                + region
                + inq_last_6mths_bin 
                + dti_q
                + chargeoff_bin
                + num_accts_ever_120_pd_cat
                + num_actv_bc_tl_cat
                
                + annual_inc_q 
                + avg_cur_bal_q
                ,
                data = train_data,
                family = binomial,
                weights = train_weights)
summary (mod_step4)



# COMPARAISON DES 4 MODELES 

library(stargazer)
library(pscl)

# Liste des modèles
models <- list(mod_step1, mod_step2, mod_step3, mod_step4)
model_names <- c("mod_step1", "mod_step2", "mod_step3", "mod_step4")

# Boucle pour récupérer AIC, BIC et pseudo-R²
aic_vals <- numeric(length(models))
bic_vals <- numeric(length(models))
mcfadden_vals <- numeric(length(models))
r2ml_vals <- numeric(length(models))
r2cu_vals <- numeric(length(models))

for(i in seq_along(models)){
  m <- models[[i]]
  r2 <- pR2(m)
  aic_vals[i] <- AIC(m)
  bic_vals[i] <- BIC(m)
  mcfadden_vals[i] <- r2["McFadden"]
  r2ml_vals[i] <- r2["r2ML"]
  r2cu_vals[i] <- r2["r2CU"]
}

# Créer un stargazer avec notes de bas de tableau
stargazer(mod_step1, mod_step2, mod_step3, mod_step4,
          type = "text",
          title = "Comparaison des modèles avec interactions",
          dep.var.labels = "Default",
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(
            c("AIC", round(aic_vals,2)),
            c("BIC", round(bic_vals,2)),
            c("Pseudo-R2 McFadden", round(mcfadden_vals,3)),
            c("Pseudo-R2 r2ML", round(r2ml_vals,3)),
            c("Pseudo-R2 r2CU", round(r2cu_vals,3))
          ))


# SUR LA BASE DES METRIQUES TROUVEES , ON RETIENT LE MODELE : md_step4   INTERACTIONS 





#7 INTERPRETATION DES COEFFICIENTS ####

# Odds Ratios
or_step4 <- exp(coef(mod_step4))
or_ci <- exp(confint(mod_step4))

# Tableau complet
or_table <- data.frame(
  Variable = names(or_step4),
  Odds_Ratio = round(or_step4, 3),
  CI_2.5 = round(or_ci[, 1], 3),
  CI_97.5 = round(or_ci[, 2], 3),
  Interprétation = ifelse(or_step4 > 1, 
                          paste0("+", round((or_step4-1)*100, 1)),
                          paste0(round((1-or_step4)*100, 1)))
)

# Affichage complet
print(or_table, row.names = FALSE)

#SCORING
mod_step4<- glm(default ~
                  + verification_status 
                + purpose_group
                + num_bc_sats
                + region
                + inq_last_6mths_bin 
                + dti_q
                + chargeoff_bin
                + num_accts_ever_120_pd_cat
                + num_actv_bc_tl_cat
                + annual_inc_q 
                + avg_cur_bal_q
                ,
                data = train_data,
                family = binomial,
)
summary (mod_step4)

modele_final <- mod_step4
summary(modele_final)

mf_train <- model.frame(modele_final)
tt <- terms(modele_final)
needed_vars <- all.vars(delete.response(tt))
for (v in needed_vars) {
  if (is.factor(mf_train[[v]])) {
    test_data[[v]] <- factor(test_data[[v]], levels = levels(mf_train[[v]]))
  }
}

# Prédictions proba ----------
test_data$prob_defaut <- predict(modele_final, newdata = test_data, type = "response")

# Scoring normalisé (β×X sans intercept/interaction) ----------
# (Profil REF = 0 ; profil max risque = 100)
coefs <- coef(modele_final)
coefs_no_inter <- coefs[names(coefs) != "(Intercept)"]
coefs_main <- coefs_no_inter[!grepl(":", names(coefs_no_inter), fixed = TRUE)]

vars_main <- attr(tt, "term.labels")
vars_main <- vars_main[!grepl(":", vars_main, fixed = TRUE)]

# Contribution MAX par variable (réf = 0)
var_max <- vapply(vars_main, function(v){
  idx  <- startsWith(names(coefs_main), paste0(v))
  vals <- unname(coefs_main[idx])
  if (length(vals) == 0 || all(!is.finite(vals))) 0 else max(c(0, vals), na.rm = TRUE)
}, numeric(1))
denom <- sum(var_max, na.rm = TRUE)

# Matrice design du test, mêmes contrastes
contrs <- attr(model.matrix(modele_final), "contrasts")
mm_test <- model.matrix(delete.response(tt),
                        data = test_data,
                        contrasts.arg = contrs,
                        na.action = na.pass)

# Aligner colonnes sur les coefs principaux
cols_needed <- names(coefs_main)
missing_cols <- setdiff(cols_needed, colnames(mm_test))
if (length(missing_cols) > 0) {
  mm_test <- cbind(
    mm_test,
    matrix(0, nrow = nrow(mm_test), ncol = length(missing_cols),
           dimnames = list(NULL, missing_cols))
  )
}
mm_test_main <- mm_test[, cols_needed, drop = FALSE]
coefs_use <- coefs_main[cols_needed]

# XB sans intercept et score
xb_no_inter <- as.numeric(mm_test_main %*% coefs_use)
test_data$score_100 <- if (is.finite(denom) && denom > 0) round(100 * pmax(xb_no_inter, 0) / denom) else NA_integer_

# =========================================
# FICHE DE SCORING + CATEGORIES + COMPARAISONS
# =========================================

# FICHE DE SCORING (points par modalité) -----
coefs <- coef(modele_final)
coefs_no_inter <- coefs[names(coefs) != "(Intercept)"]
# on ne prend que les effets principaux (pas d'interactions)
coefs_main <- coefs_no_inter[!grepl(":", names(coefs_no_inter), fixed = TRUE)]

tt         <- terms(modele_final)
vars_main  <- attr(tt, "term.labels")
vars_main  <- vars_main[!grepl(":", vars_main, fixed = TRUE)]
mf_train   <- model.frame(modele_final)

# Contribution MAX par variable (réf = 0)
var_max <- vapply(vars_main, function(v){
  idx  <- startsWith(names(coefs_main), paste0(v))
  vals <- unname(coefs_main[idx])
  if (length(vals) == 0 || all(!is.finite(vals))) 0 else max(c(0, vals), na.rm = TRUE)
}, numeric(1))
denom <- sum(var_max, na.rm = TRUE)

# Grille variable / modalité / coef / points
scorecard_list <- lapply(vars_main, function(v){
  # niveaux de la variable (si facteur) pour identifier la référence
  lv <- if (is.factor(mf_train[[v]])) levels(mf_train[[v]]) else c("ref","autre")
  # coefficients des modalités non-référence (= colonnes qui commencent par v)
  idx <- startsWith(names(coefs_main), paste0(v))
  coefs_v <- coefs_main[idx]
  # retrouver le libellé de la modalité en retirant le préfixe variable
  mod_names <- substring(names(coefs_v), nchar(v) + 1)
  
  # table ref (0) + autres modalités
  data.frame(
    variable = v,
    level    = c(lv[1], mod_names),
    coef     = c(0, unname(coefs_v)),
    points   = if (is.finite(denom) && denom > 0)
      round(100 * pmax(c(0, unname(coefs_v)), 0) / denom, 2)
    else NA_real_,
    stringsAsFactors = FALSE
  )
})
scorecard <- do.call(rbind, scorecard_list)
scorecard <- scorecard[order(scorecard$variable, -scorecard$points, scorecard$level), ]

cat("\n=== FICHE DE SCORING (points par modalité) ===\n")
print(scorecard, row.names = FALSE)
summary(test_data$score_100)

grade_levels <- c("A","B","C","D","E","F","G")
cuts_fixed   <- c(-Inf, 54, 59, 62, 69, 75, 80, Inf)  # ajuste si besoin

s <- as.numeric(test_data$score_100)
s <- pmin(pmax(s, 0), 100)  # clip de sécurité

test_data$grade_pred <- cut(
  s,
  breaks = cuts_fixed,
  labels = grade_levels,
  right = FALSE,          # intervalles [a, b)
  include.lowest = TRUE
)

# COMPARAISON AVEC grade
if ("grade" %in% names(test_data)) {
  test_data$grade_ref <- factor(as.character(test_data$grade), levels = grade_levels)
  
  ok <- !is.na(test_data$grade_pred) & !is.na(test_data$grade_ref)
  cm <- table(Pred = test_data$grade_pred[ok],
              Obs  = test_data$grade_ref[ok])
  
  cat("\n=== MATRICE DE CONFUSION (catégorie prédite vs grade) ===\n")
  print(cm)
  
  cat("\n=== Pourcentages par grade observé (colonnes à 100%) ===\n")
  cm_pct_by_obs <- prop.table(cm, margin = 2) * 100
  print(round(cm_pct_by_obs, 1))
} else {
  cat("\n[Info] Colonne 'grade' absente de test_data → comparaison non effectuée.\n")
}

# GRAPHE : Score vs Catégorie prédite -----
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

df_plot <- data.frame(
  grade_pred = test_data$grade_pred,
  score_100  = s
)

g <- ggplot(df_plot, aes(x = grade_pred, y = score_100)) +
  geom_boxplot(outlier.alpha = 0.15, width = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.2) +
  labs(
    title = "Distribution du score par catégorie (A \u2192 G)",
    x = "Catégorie prédite",
    y = "Score (0–100)"
  ) +
  theme_classic(base_size = 12)
print(g)

# write.csv(scorecard, "fiche_scoring.csv", row.names = FALSE)
modele_final$coefficients

# ===========================
# Distribution de score_100
# ===========================

if (!("score_100" %in% names(test_data))) {
  stop("score_100 est introuvable dans test_data.")
}

# vecteur propre (sans NA), borné dans [0,100] par sécurité
s <- as.numeric(test_data$score_100)
s <- s[is.finite(s)]
s <- pmin(pmax(s, 0), 100)

# stats
cat("n =", length(s), 
    "| mean =", round(mean(s),2), 
    "| sd =", round(sd(s),2), 
    "| min =", round(min(s),2), 
    "| max =", round(max(s),2), "\n")
print(quantile(s, probs = c(.05,.25,.5,.75,.95)))

# histogramme + densité
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

df_plot <- data.frame(score_100 = s)

g <- ggplot(df_plot, aes(x = score_100)) +
  geom_histogram(bins = 30, color = "white") +
  geom_density(aes(y = ..count..), linewidth = 1) +
  geom_vline(xintercept = mean(s), linetype = 2) +
  labs(title = "Distribution du score (0–100)",
       x = "score_100", y = "Fréquence") +
  theme_classic(base_size = 12)
print(g)

plot(ecdf(s), main = "ECDF du score", xlab = "score_100", ylab = "F(s)")


stopifnot("score_100" %in% names(test_data))

# ================================
# Catégorisation A→G par SCORE
# + comparaison à grade 2
# ================================

# 1) Seuils de SCORE
grade_levels <- c("A","B","C","D","E","F","G")
score_cuts   <- c(-Inf, 50, 55, 60, 70, 80, 85, Inf)

# 2) Nettoyage du score et catégorisation
s <- as.numeric(test_data$score_100)
s <- pmin(pmax(s, 0), 100)  # borne de sécurité dans [0,100]

test_data$grade_by_score <- cut(
  s,
  breaks = score_cuts,
  labels = grade_levels,
  right  = FALSE,
  include.lowest = TRUE
)

cat("Répartition des catégories par SCORE :\n")
print(table(test_data$grade_by_score, useNA = "ifany"))

# Comparaison à grade
if ("grade" %in% names(test_data)) {
  test_data$grade_ref <- factor(as.character(test_data$grade), levels = grade_levels)
  
  ok <- !is.na(test_data$grade_by_score) & !is.na(test_data$grade_ref)
  cm <- table(Pred = test_data$grade_by_score[ok],
              Obs  = test_data$grade_ref[ok])
  
  cat("\n=== MATRICE DE CONFUSION (Catégorie par SCORE vs grade) ===\n")
  print(cm)
  
  cat("\n(%) par grade observé (colonnes à 100%)\n")
  print(round(prop.table(cm, margin = 2) * 100, 1))
  
  # exact-match accuracy
  acc <- sum(diag(cm)) / sum(cm)
  cat(sprintf("\nExact match accuracy = %.3f\n", acc))
} else {
  cat("\n[Info] Colonne 'grade' absente : comparaison non effectuée.\n")
}


# Graphique : score vs grade (d'origine) + corrélation Spearman
if ("grade" %in% names(test_data)) {
  # Préparer les données
  df_plot <- data.frame(
    grade_ref = factor(as.character(test_data$grade), levels = grade_levels),
    score_100 = s
  )
  df_plot <- df_plot[!is.na(df_plot$grade_ref) & is.finite(df_plot$score_100), , drop = FALSE]

  g <- ggplot(df_plot, aes(x = grade_ref, y = score_100)) +
    geom_boxplot(outlier.alpha = 0.15, width = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 21, size = 2.2) +
    labs(
      title = "Score (0–100) par grade d'origine (A→G)",
      x = "Grade d'origine",
      y = "Score (0–100)"
    ) +
    theme_classic(base_size = 12)
  print(g)
  
  # Corrélation monotone attendue : A(1) -> G(7) vs score
  df_plot$grade_num <- as.integer(df_plot$grade_ref)
  suppressWarnings({
    ct <- cor.test(df_plot$grade_num, df_plot$score_100, method = "spearman")
    cat(sprintf("\nSpearman(grade_num, score_100) : rho = %.3f, p = %.3g\n",
                unname(ct$estimate), ct$p.value))
  })
} else {
  cat("\n[Info] 'grade' absent : graphique comparaison grade vs score non affiché.\n")
}


# Paramétrage des catégories "bas risque" et "haut risque"
low_risk  <- c("A","B")
high_risk <- c("F","G")
grade_levels <- c("A","B","C","D","E","F","G")

# Données propres
df <- test_data %>%
  transmute(
    y = as.integer(default),                                # 0/1
    grade_ref  = factor(as.character(grade), levels = grade_levels),
    grade_pred = factor(as.character(grade_by_score), levels = grade_levels)
  ) %>%
  filter(!is.na(y))

# Fonction de mesure des erreurs graves pour un schéma donné
severe_metrics <- function(y, cat_fac, scheme_label) {
  d <- data.frame(y = y, cat = cat_fac) %>%
    filter(!is.na(cat))
  n_total <- nrow(d)
  n_pos   <- sum(d$y == 1)  # défauts
  n_neg   <- sum(d$y == 0)  # non-défauts
  
  # M1: défaut (y==1) classé A/B
  m1_count <- sum(d$y == 1 & d$cat %in% low_risk)
  m1_rate  <- ifelse(n_pos > 0, m1_count / n_pos, NA_real_)
  
  # M2: non-défaut (y==0) classé F/G
  m2_count <- sum(d$y == 0 & d$cat %in% high_risk)
  m2_rate  <- ifelse(n_neg > 0, m2_count / n_neg, NA_real_)
  
  # M3: total
  m3_count <- m1_count + m2_count
  m3_rate  <- ifelse(n_total > 0, m3_count / n_total, NA_real_)
  
  tibble(
    Scheme      = scheme_label,
    n_total     = n_total,
    n_pos       = n_pos,
    n_neg       = n_neg,
    M1_count    = m1_count,
    M1_rate_pos = m1_rate,
    M2_count    = m2_count,
    M2_rate_neg = m2_rate,
    M3_count    = m3_count,
    M3_rate_all = m3_rate
  )
}

# Calcul pour les 2 schémas
res_grade   <- severe_metrics(df$y, df$grade_ref,  "grade (observé)")
res_scoring <- severe_metrics(df$y, df$grade_pred, "grade_by_score")

# Tableau comparatif
comp <- bind_rows(res_grade, res_scoring)

# Impression clean
comp_print <- comp %>%
  mutate(
    M1_rate_pos = round(100*M1_rate_pos, 2),
    M2_rate_neg = round(100*M2_rate_neg, 2),
    M3_rate_all = round(100*M3_rate_all, 2)
  )

cat("\n=== Erreurs graves — comparaison ===\n")
print(comp_print %>%
        dplyr::select(Scheme, n_total, n_pos, n_neg,
                      M1_count, M1_rate_pos,
                      M2_count, M2_rate_neg,
                      M3_count, M3_rate_all),
      row.names = FALSE)

# Graphique comparatif
plot_df <- comp %>%
  dplyr::select(Scheme, M1_rate_pos, M2_rate_neg, M3_rate_all) %>%
  tidyr::pivot_longer(cols = -Scheme, names_to = "Metric", values_to = "Rate") %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("M1_rate_pos","M2_rate_neg","M3_rate_all"),
                    labels = c("Défaut classé A/B (M1)",
                               "Payeur classé F/G (M2)",
                               "Total (M3)")),
    RatePct = 100 * Rate
  )

g <- ggplot(plot_df, aes(x = Metric, y = RatePct, fill = Scheme)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", RatePct)),
            position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3) +
  labs(title = "Erreurs graves : comparaison des schémas",
       x = NULL, y = "Taux (%)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
print(g)

#Quelques graphiques complémentaires pour présentation des résultats

grade_levels <- c("A","B","C","D","E","F","G")
df_plot <- data.frame(
  grade = factor(as.character(test_data$grade), levels = grade_levels, ordered = TRUE),
  score_100 = as.numeric(test_data$score_100)
)
df_plot <- df_plot[!is.na(df_plot$grade) & is.finite(df_plot$score_100), , drop = FALSE]
mean_score <- mean(df_plot$score_100)

g_zoom <- ggplot(df_plot, aes(x = grade, y = score_100)) +
  geom_boxplot(outlier.alpha = 0.15, width = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.2) +
  geom_hline(yintercept = mean_score, linetype = 2) +
  coord_cartesian(ylim = c(25, 95)) +            # <— zoom sans exclure les données
  scale_y_continuous(breaks = seq(25, 95, 5)) +
  labs(title = "Score par grade — zoom [25,95]", x = "Grade", y = "Score (0–100)") +
  theme_classic(base_size = 12)

print(g_zoom)


g_zoom_40_80 <- ggplot(df_plot, aes(x = grade, y = score_100)) +
  geom_boxplot(outlier.alpha = 0.15, width = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.2) +
  geom_hline(yintercept = mean_score, linetype = 2) +
  coord_cartesian(ylim = c(40, 80)) +               # <-- borne [40, 80]
  scale_y_continuous(breaks = seq(40, 80, 5)) +
  labs(title = "Score (0–100) vs grade ",
       x = "Grade", y = "Score (0–100)") +
  theme_classic(base_size = 12)

print(g_zoom_40_80)

g_zoom_40_80 + geom_jitter(width = 0.15, alpha = 0.15, size = 1)



# ==============================================================
# Fiche de scoring complète (variable / modalité / coefficient / points)
# ==============================================================

stopifnot(exists("modele_final"))

# 1) Coefficients (sans intercept, sans interactions)
coefs <- coef(modele_final)
coefs_no_inter <- coefs[names(coefs) != "(Intercept)"]
coefs_main <- coefs_no_inter[!grepl(":", names(coefs_no_inter), fixed = TRUE)]

# 2) Variables principales présentes dans le modèle
tt <- terms(modele_final)
vars_main <- attr(tt, "term.labels")
vars_main <- vars_main[!grepl(":", vars_main, fixed = TRUE)]

# 3) Dénominateur D = somme des contributions max positives par variable
var_max <- vapply(vars_main, function(v){
  idx  <- startsWith(names(coefs_main), paste0(v))
  vals <- unname(coefs_main[idx])
  if (length(vals) == 0 || all(!is.finite(vals))) 0 else max(c(0, vals), na.rm = TRUE)
}, numeric(1))
D <- sum(var_max, na.rm = TRUE)

# 4) Fiche : variable / niveau / coef / points
mf_train <- model.frame(modele_final)

scorecard_list <- lapply(vars_main, function(v){
  # Niveaux de référence depuis le model.frame (conformes aux contrastes du modèle)
  lv <- if (is.factor(mf_train[[v]])) levels(mf_train[[v]]) else c("ref","autre")
  
  # Coefs des modalités non-référence = colonnes qui commencent par "v"
  idx <- startsWith(names(coefs_main), paste0(v))
  coefs_v <- coefs_main[idx]
  
  # Libellé de modalité en retirant le préfixe "v" (R colle v + level)
  mod_names <- substring(names(coefs_v), nchar(v) + 1)
  
  data.frame(
    variable = v,
    level    = c(lv[1], mod_names),
    coef     = c(0, unname(coefs_v)),
    points   = if (is.finite(D) && D > 0) round(100 * pmax(c(0, unname(coefs_v)), 0) / D, 2) else NA_real_,
    stringsAsFactors = FALSE
  )
})

scorecard <- do.call(rbind, scorecard_list)

# 5) Tri par variable, puis points décroissants 
scorecard <- scorecard[order(scorecard$variable, -scorecard$points, scorecard$level), ]

# 6) Affichage + export CSV
print(scorecard, row.names = FALSE)
utils::write.csv(scorecard, "fiche_scoring_complete.csv", row.names = FALSE)
