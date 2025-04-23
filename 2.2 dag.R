library( here )
library( dagitty )

path_out <- here( "outputs/figs" )

# DAG with collider-stratification bias -----
png(filename = paste0(path_out, "/dag_collider-bias.png"), width = 5, height = 3, units = "in", res = 500)
dag_collider <- 'dag {
bb="0,0,1,1"
"HIV self-report" [selected,pos="0.465,0.399"]
ART [pos="0.462,0.292"]
Stigma [exposure,pos="0.322,0.264"]
VLS [outcome,pos="0.547,0.264"]
ART -> "HIV self-report"
ART -> VLS
Stigma -> "HIV self-report"
Stigma -> ART
Stigma -> VLS
}
'

plot( dagitty::dagitty( dag_collider ) )
dev.off()

# Make two DAGs, one for the community-level outcomes, and one for individual-level
# DAG for community-level outcomes ----
png(filename = paste0(path_out, "/dag_community.png"), width = 9, height = 7, units = "in", res = 500)
dag_community <- 'dag {
bb="0,0,1,1"
"Age (comm)" [adjusted,pos="0.747,0.674"]
"Discriminatory attitudes (comm)" [adjusted,pos="0.496,0.320"]
"Education (comm)" [adjusted,pos="0.300,0.341"]
"HIV knowledge (comm)" [adjusted,pos="0.567,0.752"]
"HIV prevalence (admin 1)" [adjusted,pos="0.408,0.091"]
"Perceived stigma (comm)" [adjusted,pos="0.650,0.440"]
"Shame of association (comm)" [exposure,pos="0.616,0.171"]
"Survey ID" [adjusted,pos="0.774,0.111"]
"Urban/Rural" [adjusted,pos="0.283,0.525"]
"Wealth (comm)" [adjusted,pos="0.368,0.675"]
"Outcome" [outcome,pos="0.829,0.436"]
"Age (comm)" -> "Discriminatory attitudes (comm)"
"Age (comm)" -> "Education (comm)"
"Age (comm)" -> "HIV knowledge (comm)"
"Age (comm)" -> "HIV prevalence (admin 1)"
"Age (comm)" -> "Perceived stigma (comm)"
"Age (comm)" -> "Shame of association (comm)"
"Age (comm)" -> Outcome
"Discriminatory attitudes (comm)" -> "Perceived stigma (comm)"
"Discriminatory attitudes (comm)" -> "Shame of association (comm)"
"Discriminatory attitudes (comm)" -> Outcome
"Education (comm)" -> "Discriminatory attitudes (comm)"
"Education (comm)" -> "HIV knowledge (comm)"
"Education (comm)" -> "Wealth (comm)"
"Education (comm)" -> Outcome
"HIV knowledge (comm)" -> "Discriminatory attitudes (comm)"
"HIV knowledge (comm)" -> "HIV prevalence (admin 1)"
"HIV knowledge (comm)" -> "Perceived stigma (comm)"
"HIV knowledge (comm)" -> "Shame of association (comm)"
"HIV knowledge (comm)" -> Outcome
"HIV prevalence (admin 1)" -> "Discriminatory attitudes (comm)"
"HIV prevalence (admin 1)" -> Outcome
"Perceived stigma (comm)" -> "Shame of association (comm)"
"Perceived stigma (comm)" -> Outcome
"Shame of association (comm)" -> Outcome
"Survey ID" -> "Discriminatory attitudes (comm)"
"Survey ID" -> "Perceived stigma (comm)"
"Survey ID" -> "Shame of association (comm)"
"Survey ID" -> Outcome
"Urban/Rural" -> "Discriminatory attitudes (comm)"
"Urban/Rural" -> "Education (comm)"
"Urban/Rural" -> "HIV knowledge (comm)"
"Urban/Rural" -> "HIV prevalence (admin 1)"
"Urban/Rural" -> "Wealth (comm)"
"Urban/Rural" -> Outcome
"Wealth (comm)" -> "Discriminatory attitudes (comm)"
"Wealth (comm)" -> "HIV knowledge (comm)"
"Wealth (comm)" -> "Perceived stigma (comm)"
"Wealth (comm)" -> "Shame of association (comm)"
"Wealth (comm)" -> Outcome
}

'
plot( dagitty( dag_community ) )

dev.off()

# DAG for individual-level outcomes ----
png( filename = paste0(path_out, "/dag_individual.png"), width = 9, height = 7, units = "in", res = 500 )
dag_individual <- 'dag {
bb="0,0,1,1"
"Anticipated stigma" [pos="0.682,0.463"]
"Discriminatory attitudes (comm)" [adjusted,pos="0.435,0.419"]
"Experienced healthcare discrimination" [pos="0.672,0.626"]
"HIV knowledge" [adjusted,pos="0.579,0.838"]
"HIV prevalence (admin 1)" [adjusted,pos="0.388,0.127"]
"Perceived stigma (comm)" [adjusted,pos="0.484,0.579"]
"Shame of association (comm)" [exposure,pos="0.588,0.331"]
"Survey ID" [adjusted,pos="0.611,0.139"]
"Urban/Rural" [adjusted,pos="0.262,0.617"]
Age [adjusted,pos="0.7,0.765"]
Sex [adjusted,pos="0.75,0.72"]
Education [adjusted,pos="0.245,0.344"]
Outcome [outcome,pos="0.832,0.445"]
Wealth [adjusted,pos="0.396,0.765"]
"Anticipated stigma" -> Outcome
"Discriminatory attitudes (comm)" -> "Anticipated stigma"
"Discriminatory attitudes (comm)" -> "Experienced healthcare discrimination"
"Discriminatory attitudes (comm)" -> "Perceived stigma (comm)"
"Discriminatory attitudes (comm)" -> "Shame of association (comm)"
"Discriminatory attitudes (comm)" -> Outcome
"Experienced healthcare discrimination" -> Outcome
"HIV knowledge" -> "Anticipated stigma"
"HIV knowledge" -> "Experienced healthcare discrimination"
"HIV knowledge" -> Outcome
"HIV prevalence (admin 1)" -> "Discriminatory attitudes (comm)"
"HIV prevalence (admin 1)" -> Outcome
"Perceived stigma (comm)" -> "Anticipated stigma"
"Perceived stigma (comm)" -> "Shame of association (comm)"
"Perceived stigma (comm)" -> Outcome
"Shame of association (comm)" -> "Anticipated stigma"
"Shame of association (comm)" -> Outcome
"Survey ID" -> "Anticipated stigma"
"Survey ID" -> "Discriminatory attitudes (comm)"
"Survey ID" -> "Experienced healthcare discrimination"
"Survey ID" -> "Perceived stigma (comm)"
"Survey ID" -> "Shame of association (comm)"
"Survey ID" -> Outcome
"Urban/Rural" -> "Discriminatory attitudes (comm)"
"Urban/Rural" -> "HIV knowledge"
"Urban/Rural" -> "HIV prevalence (admin 1)"
"Urban/Rural" -> Education
"Urban/Rural" -> Outcome
"Urban/Rural" -> Wealth
Sex -> "Anticipated stigma"
Sex -> "Experienced healthcare discrimination"
Sex -> "HIV knowledge"
Sex -> Education
Sex -> Wealth
Sex -> Outcome
Age -> "Anticipated stigma"
Age -> "Experienced healthcare discrimination"
Age -> "HIV knowledge"
Age -> Education
Age -> Wealth
Age -> Outcome
Education -> "Anticipated stigma"
Education -> "Experienced healthcare discrimination"
Education -> "HIV knowledge"
Education -> Outcome
Education -> Wealth
Wealth -> "Anticipated stigma"
Wealth -> "Experienced healthcare discrimination"
Wealth -> "HIV knowledge"
Wealth -> Outcome
}
'
dag_individual_ <- dagitty( dag_individual)
coordinates( dag_individual_ ) <- coordinates( dag_individual_)
plot( dag_individual_ )

dev.off()

