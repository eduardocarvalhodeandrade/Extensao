# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

dados_sinasc = read.csv("SINASC_2015.csv", header = T, sep = ";")

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = subset(dados_sinasc, select = c(1,4,5,6,7,12,13,14,15,19,21,22,23,24,35,38,44,46,48,59,60,61))
# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

UF = substr(as.character(dados_sinasc_1$CODMUNRES),1,2)
dados_sinasc_2 = dados_sinasc_1[UF == "16",]
# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv
write.csv(dados_sinasc_2 , "dados_sinasc_2.csv")

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCKOMAL, dados_sinasc_2$ESCMAE2010,dados_sinasc_2$RACACORMAE,dados_sinasc_2$TPAPRESENT))
varsNec = c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", "APGAR5", "RACACOR", "IDANOMAL", "ESCMAE2010", "RACACORMAE", "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK", "IDADEMAE", "CONSPRENAT", "PESO", "SEMAGESTAC")
lapply(dados_sinasc_2[varsNec], table, useNA = "always")
# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
NumIndesej = c(9,9,9,9,9,0,9,9,9,11,9,99) #apgar,racacor,racacormae,paridade não precisa.
varsNec2 = c("LOCNASC","ESTCIVMAE","GESTACAO","GRAVIDEZ","PARTO","SEXO", "IDANOMAL", "ESCMAE2010", "TPAPRESENT", "TPROBSON", "KOTELCHUCK", "CONSPRENAT")

transforme = function(df,col,numero){
  df[[col]][df[col] == numero] = NA
  df
}
for (i in 1:length(NumIndesej)){
  dados_sinasc_2 = transforme(dados_sinasc_2,varsNec2[i],NumIndesej[i])
}
# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, c(1,2,3,4,5), c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, c(1,2,3,4,5), c("Solteira","Casada","Viúva","Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, c(1,2,3,4,5,6), c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "41 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, c(1,2,3), c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, c(1,2), c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, c(1,2), c("Masculino","Feminino"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, c(1,2), c("Sim", "Não"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, c(1,2,3,4,5), c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, c(0,1,2,3,4,5), c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, c(1,2,3,4,5), c("Branca","Preta","Amarela","Parda","Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, c(1,2,3), c("Cefálico","Pélvica ou podálica","Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, c(1,2,3,4,5,6,7,8,9,10), c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, c(1,2,3,4,5), c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))
# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator
dados_sinasc_2$F_PESO = as.factor(ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso", ifelse(dados_sinasc_2$PESO < 4000, "Peso normal", "Macrossomia")))
dados_sinasc_2$F_IDADE = cut(dados_sinasc_2$IDADEMAE, breaks=c(-Inf, 15, 20, 25, 30, 35, 40, 45, 50, Inf), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), right = FALSE)
dados_sinasc_2$F_APGAR5 = as.factor(ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal"))
class(dados_sinasc_2$F_PESO)
class(dados_sinasc_2$F_IDADE)
class(dados_sinasc_2$F_APGAR5) #Todos factor
# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header=T, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino","Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2,tabela_pig, by=c("SEMAGESTAC","SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG=ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,ifelse(is.na(dados_sinasc_2$PESO)|is.na(dados_sinasc_2$PESO_P10)|is.na(dados_sinasc_2$PESO_P90),NA,ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10,"PIG", ifelse(dados_sinasc_2$PESO<=dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels=c("PIG","AIG","GIG"))
# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES

#Tarefa 9 e 10 reformulada:

base = data.frame(CODMUNRES=sort(unique(dados_sinasc_2$CODMUNRES)))
length(base)
#ANO:
base = cbind(ANO = 2015, base)

#Total de nascimentos:
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels=base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base,TN,by="CODMUNRES", all.x=TRUE)

#Total de nascimentos com registros completos:
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES),1,2)=="16",]
dados_UF_comp = dados_UF[complete.cases(dados_UF),]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base,TNRC,by="CODMUNRES",all.x = TRUE)

#Total de nascimentos com dados completos nas 22 variaveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "16",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

#TGI:
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c("TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

#TGIF:
anos_filtrados = dados_sinasc_2[dados_sinasc_2$F_IDADE %in% c("15-19", "20-24","25-29","30-34","35-39","40-44","45-49"),]
gif = table(anos_filtrados$CODMUNRES)
TGIF = as.data.frame(gif)
names(TGIF) = c("CODMUNRES","TGIF")
base = merge(base, TGIF, by = "CODMUNRES", all.x = TRUE)

#IM_P:
tmp = aggregate(IDADEMAE ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
tmp_resumo = data.frame(CODMUNRES = tmp$CODMUNRES,tmp$IDADEMAE)
names(tmp_resumo) = c("CODMUNRES","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP")
base = merge(base, tmp_resumo, by = "CODMUNRES", all.x = TRUE)

#EM:
EM = as.data.frame.matrix(table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$ESCMAE2010))
names(EM) = c("EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC")
EM$CODMUNRES = rownames(EM)
base = merge(base,EM, by="CODMUNRES", all.x=TRUE)

#TGRC:
TGRC = as.data.frame.matrix(table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACORMAE))
names(TGRC) = c("TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I")
TGRC$CODMUNRES = rownames(TGRC)
base = merge(base,TGRC, by="CODMUNRES", all.x=TRUE)

#TGSC e TGCC:
parceiro = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"),"TGCC","TGSC")
tab_parceiro = table(dados_sinasc_2$CODMUNRES, parceiro)
df_estado_civil = as.data.frame.matrix(tab_parceiro)
df_estado_civil$CODMUNRES = rownames(df_estado_civil)
base = merge(base, df_estado_civil, by="CODMUNRES", all.x = TRUE)

#TGPRI e TGNPRI:
tab_pri = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$PARIDADE)
df_pri = as.data.frame.matrix(tab_pri)
names(df_pri) = c("TGPRI","TGNPRI")
df_pri$CODMUNRES = rownames(df_pri)
base = merge(base, df_pri, by="CODMUNRES", all.x = TRUE)

#TGU e TGG:
grav = ifelse(dados_sinasc_2$GRAVIDEZ %in% c("Única"), "Única", "Gemelar")
tab_grav = table(dados_sinasc_2$CODMUNRES, grav)
df_gravidez = as.data.frame.matrix(tab_grav)
names(df_gravidez) = c("TGU","TGG")
df_gravidez$CODMUNRES = rownames(df_gravidez)
base = merge(base,df_gravidez,by="CODMUNRES", all.x=TRUE)

#TGD:
tab_temp_grav = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "41 semanas e mais")))
df_temp_grav = as.data.frame.matrix(tab_temp_grav)
names(df_temp_grav) = c("TGD_22","TGD_22_27","TGD_28_31","TGD_32_36", "TGD_37_41","TGD_42")
df_temp_grav$CODMUNRES = rownames(df_temp_grav)
base = merge(base, df_temp_grav, by = "CODMUNRES", all.x = TRUE)

#TGD_PRT, TGD_AT, TGD_PST:
cat_gest = ifelse(dados_sinasc_2$GESTACAO %in% c("Menos de 22 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("22 a 27 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("28 a 31 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("32 a 36 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("37 a 41 semanas"), "TGD_AT", "TGD_PST")))))
cat_gest = factor(cat_gest, levels = c("TGD_PRT","TGD_AT","TGD_PST"))
tab_cat_gest = table(dados_sinasc_2$CODMUNRES, cat_gest)
df_cat_gest = as.data.frame.matrix(tab_cat_gest)
names(df_cat_gest) = c("TGD_PRT","TGD_AT","TGD_PST")
df_cat_gest$CODMUNRES = rownames(df_cat_gest)
base = merge(base, df_cat_gest, by="CODMUNRES",all.x = TRUE)

#DG:
tdg = aggregate(SEMAGESTAC ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
tdg_resumo = data.frame(CODMUNRES = tdg$CODMUNRES,tdg$SEMAGESTAC)
names(tdg_resumo) = c("CODMUNRES","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP")
base = merge(base, tdg_resumo, by = "CODMUNRES", all.x = TRUE)

#TKC:
tab_tkc = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$KOTELCHUCK)
df_tkc = as.data.frame.matrix(tab_tkc)
names(df_tkc) = c("TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD")
df_tkc$CODMUNRES = rownames(df_tkc)
base = merge(base, df_tkc, by = "CODMUNRES", all.x = TRUE)

#TGPRG:
peregrinou = ifelse(dados_sinasc_2$CODMUNRES != dados_sinasc_2$CODMUNNASC, "Peregrinou", "Não peregrinou")
tab_tgprg = table(dados_sinasc_2$CODMUNRES, peregrinou)
df_tgprg = as.data.frame.matrix(tab_tgprg)
names(df_tgprg) = c("TGPRG_S","TGPRG_N")
df_tgprg$CODMUNRES = rownames(df_tgprg)
base = merge(base, df_tgprg, by="CODMUNRES",all.x = TRUE)

#TP:
tab_tp = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$PARTO)
df_tp = as.data.frame.matrix(tab_tp)
names(df_tp) = c("TPV","TPC")
df_tp$CODMUNRES = rownames(df_tp)
base = merge(base,df_tp,by="CODMUNRES",all.x = TRUE)

#TRAP:
tab_trap = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$TPAPRESENT)
df_trap = as.data.frame.matrix(tab_trap)
names(df_trap) = c("TRAP_C","TRAP_P","TRAP_T")
df_trap$CODMUNRES = rownames(df_trap)
base = merge(base,df_trap,by="CODMUNRES",all.x = TRUE)

#TGROB:
tab_TGROB = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$TPROBSON)
df_TGROB = as.data.frame.matrix(tab_TGROB)
names(df_TGROB) = c("TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10")
df_TGROB$CODMUNRES = rownames(df_TGROB)
base = merge(base,df_TGROB,by="CODMUNRES",all.x=TRUE)

#TNLOC:
tab_TNLOC = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$LOCNASC)
df_TNLOC = as.data.frame.matrix(tab_TNLOC)
names(df_TNLOC) = c("TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI")
df_TNLOC$CODMUNRES = rownames(df_TNLOC)
base = merge(base,df_TNLOC,by="CODMUNRES",all.x=TRUE)

#TRS:
tab_TRS = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$SEXO)
df_trs = as.data.frame.matrix(tab_TRS)
names(df_trs) = c("TRS_M","TRS_F")
df_trs$CODMUNRES = rownames(df_trs)
base = merge(base,df_trs,by="CODMUNRES",all.x=TRUE)

#TRRC:
tab_trrc = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACOR)
df_trrc = as.data.frame.matrix(tab_trrc)
names(df_trrc) = c("TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I")
df_trrc$CODMUNRES = rownames(df_trrc)
base = merge(base,df_trrc,by="CODMUNRES",all.x=TRUE)

#TRP:
tab_trp = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$F_PESO)
df_trp = as.data.frame.matrix(tab_trp)
names(df_trp) = c("TRP_BP","TRP_N","TRP_M")
df_trp$CODMUNRES = rownames(df_trp)
base = merge(base,df_trp,by="CODMUNRES",all.x=TRUE)

#PESO:
pes = aggregate(PESO ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
pes_resumo = data.frame(CODMUNRES = pes$CODMUNRES,pes$PESO)
names(pes_resumo) = c("CODMUNRES","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP")
base = merge(base, pes_resumo, by = "CODMUNRES", all.x = TRUE)

#TRPIG:
tab_pig = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$F_PIG) 
df_pig = as.data.frame.matrix(tab_pig) 
names(df_pig) = c("TRPIG_P","TRPIG_A","TRPIG_G") 
df_pig$CODMUNRES = rownames(df_pig) 
base = merge(base,df_pig,by="CODMUNRES",all.x=TRUE) #Você pediu especificadamente gestações únicas, mas já contamos o PIG apenas para gestações únicas, então a contagem vale só para os valores que não são NA

#TRAPG5:
tip_apgar5 = ifelse(dados_sinasc_2$APGAR5 < 7, "TRAPG5_B", "TRAPG5_N")
tab_apgar5 = table(dados_sinasc_2$CODMUNRES, tip_apgar5)
df_apgar5 = as.data.frame.matrix(tab_apgar5)
names(df_apgar5) = c("TRAPG5_B","TRAPG5_N")
df_apgar5$CODMUNRES = rownames(df_apgar5)
base = merge(base,df_apgar5,by="CODMUNRES",all.x=TRUE)

#APG5:
apg5 = aggregate(APGAR5 ~ CODMUNRES,data = dados_sinasc_2, FUN = function(x) c(mean = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))

apg5_resumo = data.frame(CODMUNRES = apg5$CODMUNRES,APG5_MD   = apg5$APGAR5[, "mean"],APG5_DP   = apg5$APGAR5[, "sd"])

base = merge(base, apg5_resumo, by = "CODMUNRES", all.x = TRUE)

#TRA:
tab_tra = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$IDANOMAL)
df_tra = as.data.frame.matrix(tab_tra)
names(df_tra) = c("TRAC","TRSAC")
df_tra$CODMUNRES = rownames(df_tra)
base = merge(base,df_tra,by="CODMUNRES",all.x = TRUE)

#Agora, adicionando o total de todos com o nivel UF
cols_contagem = setdiff(names(base), c("CODMUNRES","ANO","TN","TNRC","TNRCR","TGI_15","TGI_15_19","TGI_20_24","TGI_25_29","TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50","TGIF","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC","TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I","TGCC","TGSC","TGPRI","TGNPRI","TGU","TGG","TGD_22","TGD_22_27","TGD_28_31","TGD_32_36","TGD_37_41","TGD_42","TGD_PRT","TGD_AT","TGD_PST","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD","TGPRG_S","TGPRG_N","TPV","TPC","TRAP_C","TRAP_P","TRAP_T","TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10","TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI","TRS_M","TRS_F","TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I","TRP_BP","TRP_N","TRP_M","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP","TRPIG_P","TRPIG_A","TRPIG_G","TRAPG5_B","TRAPG5_N","APG5_MD","APG5_DP","TRAC","TRSAC"))
base[cols_contagem][is.na(base[cols_contagem])] = 0

linha_estado = base[1,]
linha_estado[,] = NA
cols_contagem = setdiff(names(base),c("CODMUNRES","ANO","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP","APG5_MD","APG5_DP"))
linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm =TRUE),2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE),2)
q = round(quantile(dados_sinasc_2$IDADEMAE,probs = c(0.25,0.5,0.75), na.rm = TRUE),2)
linha_estado$IM_P25 = q[1]
linha_estado$IM_P50 = q[2]
linha_estado$IM_P75 = q[3]

linha_estado$CODMUNRES  = 16
SINASC_AC = rbind(linha_estado, base)
SINASC_AC$NIVEL = c("UF", rep("MUNICIPIO",nrow(SINASC_AC)-1))
SINASC_AC$ANO = 2015
SINASC_AC = SINASC_AC[,c("ANO","NIVEL","CODMUNRES", names(SINASC_AC)[!names(SINASC_AC) %in% c("ANO","NIVEL","CODMUNRES")])]
# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(SINASC_AC, "SINASC_AP.csv")

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

