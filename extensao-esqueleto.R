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
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"

# Professora, eu só li as instruções após ter dado o merge, nomeei o comentario "Alterando orientações da etapa 2"

# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SIM no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim
dados_sim = read.csv("Mortalidade_Geral_2015.csv", header = TRUE, sep=";")


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO,
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
dados_sim_1 = subset(dados_sim, select = c(1,3,4,8,9,10,11,14,17,35,36,37,47,77,84))
# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF
UF_sim = substr(as.character(dados_sim_1$CODMUNRES),1,2)
dados_sim_2 = dados_sim_1[UF_sim == "16",]

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645    
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

write.csv(dados_sim_2, "dados_sim_2.csv")

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

varsNec_sim = c("TIPOBITO", "SEXO", "RACACOR", "TPMORTEOCO", "OBITOGRAV", "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")
lapply(dados_sim_2[varsNec_sim], table, useNA = "always")

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

vars_nec2_sim = c("TPMORTEOCO", "OBITOGRAV", "OBITOPUERP","TPOBITOCOR","MORTEPARTO","IDADE","SEXO","ESC2010")
nums_indesej2 = c(9,9,9,9,9,999,9,9)
transforme_sim = function(df,col,numero){
  df[[col]][df[col] == numero] = NA
  df
}
for (i in 1:length(nums_indesej2)){
  dados_sim_2 = transforme_sim(dados_sim_2,vars_nec2_sim[i],nums_indesej2[i])
}

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2),
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, c(1,2) ,c("Fetal","Não fetal"))
dados_sim_2$SEXO = factor(dados_sim_2$SEXO, c(1,2),c("Masculino","Feminino"))
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, c(1,2,3,4,5),c("Branca","Preta","Amarela","Parda","Indígena"))
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, c(1,2,3,4,5,8),c("Na gravidez","No parto","No abortamento","Até 42 dias após o termino do parto","De 43 dias a 1 ano após o término da gestação", "Não ocorreu nestes períodos"))
dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV, c(1,2), c("Sim","Não"))
dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP, c(1,2,3), c("Sim, até 42 dias após o parto","Sim, de 43 dias a 1 ano","Não"))
dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR, c(1,2,3,4,5,6,7,8,9), c("Durante a gestação","Durante o abortamento","Após o abortamento","No parto ou até 1 hora após o parto", "No puerpério - até 42 dias após o parto", "Entre 43 dias e até 1 ano após o parto", "A investigação não identificou o momento do óbito", "Mais de um ano após o parto","O obito não ocorreu nas circunstancias anteriores"))
dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO, c(1,2,3), c("Antes","Durante","Após"))


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

base_sim = data.frame(CODMUNRES=sort(unique(dados_sim_2$CODMUNRES)))
base_sim = cbind(ANO = 2015, base_sim)

#TO:

TO = as.data.frame(table(factor(dados_sim_2$CODMUNRES, levels=base_sim$CODMUNRES)))
names(TO) = c("CODMUNRES","TO")
base_sim = merge(base_sim,TO,by="CODMUNRES", all.x=TRUE)

#TORC:
dados_UF_sim = dados_sim[substr(as.character(dados_sim$CODMUNRES),1,2)=="16",]
dados_UF_comp_sim = dados_UF_sim[complete.cases(dados_UF_sim),]
TORC = as.data.frame(table(factor(dados_UF_comp_sim$CODMUNRES, levels = base_sim$CODMUNRES)))
names(TORC) = c("CODMUNRES","TORC")
base_sim = merge(base_sim,TORC,by="CODMUNRES",all.x = TRUE)

#TORCR:
dados_UF_1_sim = dados_sim_1[substr(as.character(dados_sim_1$CODMUNRES), 1, 2) == "16",]
dados_UF_1_comp_sim = dados_UF_1_sim[complete.cases(dados_UF_1_sim), ]
TORCR = as.data.frame(table(factor(dados_UF_1_comp_sim$CODMUNRES, levels = base_sim$CODMUNRES)))
names(TORCR) = c("CODMUNRES","TORCR")
base_sim = merge(base_sim, TORCR, by = "CODMUNRES", all.x = TRUE)

#TO_NN:
to_nn_filtrado = dados_sim_2[grepl("^[VWXY]", dados_sim_2$CAUSABAS), ]
to_nn_tab = table(to_nn_filtrado$CODMUNRES)
df_to_nn = as.data.frame(to_nn_tab)
names(df_to_nn) = c("CODMUNRES","TO_NN")
base_sim = merge(base_sim, df_to_nn, by = "CODMUNRES", all.x = TRUE)
#TO_N:
to_n_filtrado = dados_sim_2[!grepl("^[VWXY]", dados_sim_2$CAUSABAS), ]
to_n_tab = table(to_n_filtrado$CODMUNRES)
df_to_n = as.data.frame(to_n_tab)
names(df_to_n) = c("CODMUNRES","TO_N")
base_sim = merge(base_sim, df_to_n, by = "CODMUNRES", all.x = TRUE)

#TO_CB_I:

to_cb_i_filtrado = dados_sim_2[grepl("^[AB]", dados_sim_2$CAUSABAS), ]
to_cb_i_tab = table(to_cb_i_filtrado$CODMUNRES)
df_to_cb_i = as.data.frame(to_cb_i_tab)
names(df_to_cb_i) = c("CODMUNRES","TO_CB_I")
base_sim = merge(base_sim, df_to_cb_i, by = "CODMUNRES", all.x = TRUE)

#TO_CB_N:
to_cb_n_filtrado = dados_sim_2[grepl("^[CD]", dados_sim_2$CAUSABAS), ]
to_cb_n_tab = table(to_cb_n_filtrado$CODMUNRES)
df_to_cb_n = as.data.frame(to_cb_n_tab)
names(df_to_cb_n) = c("CODMUNRES","TO_CB_N")
base_sim = merge(base_sim, df_to_cb_n, by = "CODMUNRES", all.x = TRUE)

#TO_CB_C:
to_cb_c_filtrado = dados_sim_2[grepl("^[I]", dados_sim_2$CAUSABAS), ]
to_cb_c_tab = table(to_cb_c_filtrado$CODMUNRES)
df_to_cb_c = as.data.frame(to_cb_c_tab)
names(df_to_cb_c) = c("CODMUNRES","TO_CB_C")
base_sim = merge(base_sim, df_to_cb_c, by = "CODMUNRES", all.x = TRUE)

#TO_CB_R:

to_cb_r_filtrado = dados_sim_2[grepl("^[J]", dados_sim_2$CAUSABAS), ]
to_cb_r_tab = table(to_cb_r_filtrado$CODMUNRES)
df_to_cb_r = as.data.frame(to_cb_r_tab)
names(df_to_cb_r) = c("CODMUNRES","TO_CB_R")
base_sim = merge(base_sim, df_to_cb_r, by = "CODMUNRES", all.x = TRUE)

#TO_CB_O:

to_cb_o_filtrado = dados_sim_2[!grepl("^[ABCDIJVWXY]", dados_sim_2$CAUSABAS), ]
to_cb_o_tab = table(to_cb_o_filtrado$CODMUNRES)
df_to_cb_o = as.data.frame(to_cb_o_tab)
names(df_to_cb_o) = c("CODMUNRES","TO_CB_O")
base_sim = merge(base_sim, df_to_cb_o, by = "CODMUNRES", all.x = TRUE)

#TO_M e TO_F:

tab_TOM = table(dados_sim_2$CODMUNRES, dados_sim_2$SEXO)
df_tom = as.data.frame.matrix(tab_TOM)
names(df_tom) = c("TO_M","TO_F")
df_tom$CODMUNRES = rownames(df_tom)
base_sim = merge(base_sim,df_tom,by="CODMUNRES",all.x=TRUE)

#TO_F_IF:

to_f_filtrado = dados_sim_2[dados_sim_2$IDADE >= 415 & dados_sim_2$IDADE <= 449, ]
tab_to_f_if = table(to_f_filtrado$CODMUNRES)
df_to_f_if = as.data.frame(tab_to_f_if)
names(df_to_f_if) = c("CODMUNRES", "TO_F_IF")
base_sim = merge(base_sim, df_to_f_if, by = "CODMUNRES", all.x = TRUE)

#TO_FT:
todos_ob = unique(dados_sim_2$CODMUNRES)

df_to_ft = data.frame(CODMUNRES = todos_ob, Freq = 0) #Tive que fazer isso pois não existe nenhum caso com morte fetal. Não da pra fazer table.
names(df_to_ft) = c("CODMUNRES","TO_FT")
base_sim = merge(base_sim, df_to_ft, by = "CODMUNRES", all.x = TRUE) 

#TO_NT:

to_nt_filtrado = dados_sim_2[dados_sim_2$IDADE >= 200 & dados_sim_2$IDADE <= 227 | dados_sim_2$IDADE <= 123, ]
tab_to_nt = table(to_nt_filtrado$CODMUNRES)
df_to_nt = as.data.frame(tab_to_nt)
names(df_to_nt) = c("CODMUNRES", "TO_NT")
base_sim = merge(base_sim, df_to_nt, by = "CODMUNRES", all.x = TRUE)

#TO_NT_P e T:

to_nt_pt_filtrado = ifelse(to_nt_filtrado$IDADE <= 206 & to_nt_filtrado$IDADE >= 200 | to_nt_filtrado$IDADE <= 123, "TO_NT_P", "TO_NT_T")
tab_to_nt_pt = table(to_nt_filtrado$CODMUNRES, to_nt_pt_filtrado)
tab_to_nt_pt
df_to_nt_pt = as.data.frame.matrix(tab_to_nt_pt)
names(df_to_nt_pt) = c("TO_NT_P", "TO_NT_T")
df_to_nt_pt$CODMUNRES = rownames(df_to_nt_pt)
base_sim = merge(base_sim, df_to_nt_pt, by = "CODMUNRES", all.x = TRUE)


#TO_PNT:

to_pnt_filtrado = dados_sim_2[dados_sim_2$IDADE >= 228 & dados_sim_2$IDADE <= 231 | dados_sim_2$IDADE >= 301 & dados_sim_2$IDADE <= 312, ]
tab_to_pnt = table(to_pnt_filtrado$CODMUNRES)
df_to_pnt = as.data.frame(tab_to_pnt)
names(df_to_pnt) = c("CODMUNRES", "TO_PNT")
base_sim = merge(base_sim, df_to_pnt, by = "CODMUNRES", all.x = TRUE)

#TO_MT_G:
to_mt_g_filtrado = dados_sim_2[dados_sim_2$TPMORTEOCO == "Na gravidez", ]
tab_to_mt_g = table(to_mt_g_filtrado$CODMUNRES)
df_to_mt_g = as.data.frame(tab_to_mt_g)
names(df_to_mt_g) = c("CODMUNRES","TO_MT_G")
base_sim = merge(base_sim, df_to_mt_g, by = "CODMUNRES", all.x = TRUE)

#TONT_B, PT, A, PD, I:

tab_tont = table(to_nt_filtrado$CODMUNRES, to_nt_filtrado$RACACOR)
df_tont = as.data.frame.matrix(tab_tont)
names(df_tont) = c("TONT_B","TONT_PT","TONT_A","TONT_PD","TONT_I")
df_tont$CODMUNRES = rownames(df_tont)
base_sim = merge(base_sim, df_tont, by="CODMUNRES", all.x = T)

#TO_MT:
to_mt_filtrado = dados_sim_2[!(is.na(dados_sim_2$TPMORTEOCO)), ]
tab_to_mt = table(to_mt_filtrado$CODMUNRES)
df_to_mt = as.data.frame(tab_to_mt)
names(df_to_mt) = c("CODMUNRES", "TO_MT")
base_sim = merge(base_sim, df_to_mt, by = "CODMUNRES", all.x = T)

#TO_MT_DG,PT,AB,42,43
to_mt_dg_filtrado = to_mt_filtrado
to_mt_dg_filtrado <- to_mt_filtrado
to_mt_dg_filtrado$TPMORTEOCO = droplevels(to_mt_dg_filtrado$TPMORTEOCO)
to_mt_dg_filtrado$TPMORTEOCO[to_mt_dg_filtrado$TPMORTEOCO == "Não ocorreu nestes períodos"] = NA
tab_to_mt_dg = table(to_mt_dg_filtrado$CODMUNRES, to_mt_dg_filtrado$TPMORTEOCO)
df_to_mt_dg = as.data.frame.matrix(tab_to_mt_dg)
names(df_to_mt_dg) = c("TO_MT_DG","TO_MT_PT","TO_MT_AB","TO_MT_42","TO_MT_43")
df_to_mt_dg$CODMUNRES = rownames(df_to_mt_dg)
base_sim = merge(base_sim, df_to_mt_dg, by = "CODMUNRES", all.x = TRUE)

#TO_MT_P:
to_mt_filtrado2 = to_mt_dg_filtrado
to_mt_filtrado2$TPMORTEOCO[to_mt_dg_filtrado$TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação"] = NA
to_mt_filtrado2$TPMORTEOCO = droplevels(to_mt_dg_filtrado$TPMORTEOCO)
tab_to_mt_filtrado2 = table(to_mt_filtrado2$CODMUNRES)
df_to_mt_p = as.data.frame(tab_to_mt_filtrado2)
names(df_to_mt_p) = c("CODMUNRES","TO_MT_P")
base_sim = merge(base_sim, df_to_mt_p, by= "CODMUNRES", all.x = TRUE)

#TO_MT_P_I:
to_mt_p_i = to_mt_filtrado2[to_mt_filtrado2$IDADE >= 415 & to_mt_filtrado2$IDADE <= 449, ]
tab_to_mt_p_i = table(to_mt_p_i$CODMUNRES)
df_to_mt_p_i = as.data.frame(tab_to_mt_p_i)
names(df_to_mt_p_i) = c("CODMUNRES", "TO_MT_P_I")
base_sim = merge(base_sim, df_to_mt_p_i, by = "CODMUNRES", all.x = TRUE)

#TO_MT_P_ES,EFI,EFII,EM,ESI,ESC

tab_to_mt_p_es = table(to_mt_filtrado2$CODMUNRES, to_mt_filtrado2$ESC2010)
df_to_mt_p_es = as.data.frame.matrix(tab_to_mt_p_es)
names(df_to_mt_p_es) = c("TO_MT_P_ES","TO_MT_P_EFI","TO_MT_P_EFII","TO_MT_P_EM","TO_MT_P_ESI","TO_MT_P_ESC")
df_to_mt_p_es$CODMUNRES = rownames(df_to_mt_p_es)
base_sim = merge(base_sim, df_to_mt_p_es, by="CODMUNRES", all.x = TRUE)

#Agora adicionando o Nivel.

cols_contagem2 = setdiff(names(base_sim), c("CODMUNRES","ANO","TO","TORC","TORCR","TO_NN","TO_N","TO_CB_I","TO_CB_N","TO_CB_C","TO_CB_R","TO_CB_O","TO_M","TO_F","TO_F_IF","TO_FT","TO_NT","TO_NT_P","TO_NT_T","TO_PNT","TO_MT_G","TONT_B","TONT_PT","TONT_A","TONT_PD","TONT_I","TO_MT", "TO_MT_DG","TO_MT_PT","TO_MT_AB","TO_MT_42","TO_MT_43","TO_MT_P","TO_MT_P_I","TO_MT_P_ES","TO_MT_P_EFI","TO_MT_P_EFII","TO_MT_P_EM","TO_MT_P_ESI","TO_MT_P_ESC"))
base_sim[cols_contagem2][is.na(base_sim[cols_contagem2])] = 0

linha_estado2 = base_sim[1,]
linha_estado2[,] = NA
cols_contagem2 = setdiff(names(base_sim),c("CODMUNRES","ANO"))
linha_estado2[cols_contagem2] = colSums(base_sim[cols_contagem2], na.rm = TRUE)

linha_estado2$CODMUNRES  = 16 
SIM_AC = rbind(linha_estado2, base_sim)
SIM_AC$NIVEL = c("UF", rep("MUNICIPIO",nrow(SIM_AC)-1))
SIM_AC$ANO = 2015
SIM_AC = SIM_AC[,c("ANO","NIVEL","CODMUNRES", names(SIM_AC)[!names(SIM_AC) %in% c("ANO","NIVEL","CODMUNRES")])]

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
write.csv(SIM_AC, "SIM_AP.csv")
# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"
SIDRA = read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", header=T, sep=";")
SIDRA2 = read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", header=T, sep=",")
SIDRA3 = read.csv("população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv", header=T, sep=";")
SIDRA4 = read.csv("população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv", header=T, sep=";")

UFSIDRA = substr(as.character(SIDRA$CODMUNRES),1,2)
dados_sidra = SIDRA[UFSIDRA == "16",]
UFSIDRA2 = substr(as.character(SIDRA2$CODMUNRES),1,2)
dados_sidra2 = SIDRA2[UFSIDRA2 == "16",]
UFSIDRA3 = substr(as.character(SIDRA3$CODMUNRES),1,2)
dados_sidra3 <- SIDRA3[UFSIDRA3 == "16" & !is.na(UFSIDRA3), ] # Por algum motivo, havia uma linha inteira de NAs a mais, que não existe no dataframe original.

UFSIDRA4 = substr(as.character(SIDRA4$CODMUNRES),1,2)
dados_sidra4 = SIDRA4[UFSIDRA4 == "16",]

lapply(dados_sidra, table, useNA = "always")
lapply(dados_sidra2, table, useNA = "always")
lapply(dados_sidra3, table, useNA = "always") 
lapply(dados_sidra4, table, useNA = "always")

base_sidra = data.frame(CODMUNRES=sort(unique(dados_sidra$CODMUNRES)))
base_sidra = cbind(ANO = 2015, base_sidra)

#POPRE_T:
popre_t = dados_sidra[, c(1,3)]
base_sidra = merge(base_sidra, popre_t, by="CODMUNRES", all.x = T)

#POPRC:
poprc = dados_sidra2[, c(1,3,4,5)]
base_sidra = merge(base_sidra, poprc, by = "CODMUNRES", all.x = TRUE)

#POPRC_15, 15_49, 50:
poprc_15 = dados_sidra4[dados_sidra4$F_IDADE %in% c("0 a 4 anos","5 a 9 anos","10 a 14 anos") ,]
poprc_49 = dados_sidra4[dados_sidra4$F_IDADE %in% c("15 a 19 anos","20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos", "45 a 49 anos"),]
poprc_50 = dados_sidra4[dados_sidra4$F_IDADE %in% c("50 a 54 anos","55 a 59 anos","60 a 64 anos","65 a 69 anos", "70 a 74 anos","75 a 79 anos", "80 a 89 anos", "90 a 99 anos","100 anos ou mais"),]

df_poprc_15 = aggregate(POP ~ CODMUNRES, data = dados_sidra4, sum)
names(df_poprc_15) = c("CODMUNRES","POPRC_15")

df_poprc_49 = aggregate(POP ~ CODMUNRES, data = dados_sidra4, sum)
names(df_poprc_49) = c("CODMUNRES","POPRC_15_49")

df_poprc_50 = aggregate(POP ~ CODMUNRES, data = dados_sidra4, sum)
names(df_poprc_50) = c("CODMUNRES","POPRC_50")
base_sidra = merge(base_sidra, df_poprc_15, by="CODMUNRES", all.x = T)
base_sidra = merge(base_sidra, df_poprc_49, by="CODMUNRES", all.x = T)
base_sidra = merge(base_sidra, df_poprc_50, by="CODMUNRES", all.x = T)

#Adicionando a UF


uf_poprc_15 = dados_sidra3[dados_sidra3$F_IDADE %in% c("0 a 4 anos","5 a 9 anos","10 a 14 anos") ,]
df_uf_pop15 = aggregate(POP ~ CODMUNRES, data = uf_poprc_15, sum)
names(df_uf_pop15) = c("CODMUNRES","POPRC_15")
uf_poprc_49 = dados_sidra3[dados_sidra3$F_IDADE %in% c("15 a 19 anos","20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos", "45 a 49 anos") ,]
df_uf_pop49 = aggregate(POP ~ CODMUNRES, data = uf_poprc_49, sum)
names(df_uf_pop49) = c("CODMUNRES","POPRC_15_49")
uf_poprc_50 = dados_sidra3[dados_sidra3$F_IDADE %in% c("50 a 54 anos","55 a 59 anos","60 a 64 anos","65 a 69 anos", "70 a 74 anos","75 a 79 anos", "80 a 89 anos", "90 a 99 anos","100 anos ou mais"),]
df_uf_pop50 = aggregate(POP ~ CODMUNRES, data = uf_poprc_50, sum)
names(df_uf_pop50) = c("CODMUNRES","POPRC_50")

base_sidra$POPRC_15[base_sidra$CODMUNRES == 16] = df_uf_pop15$POPRC_15[df_uf_pop15$CODMUNRES == 16]

base_sidra$POPRC_15_49[base_sidra$CODMUNRES == 16] = df_uf_pop49$POPRC_15_49[df_uf_pop49$CODMUNRES == 16]

base_sidra$POPRC_50[base_sidra$CODMUNRES == 16] = df_uf_pop50$POPRC_50[df_uf_pop50$CODMUNRES == 16]

#POPRC_F:

poprc_f_15 = dados_sidra4[dados_sidra4$F_IDADE %in% c("0 a 4 anos","5 a 9 anos","10 a 14 anos"),]
df_popf_15 = aggregate(POPF ~ CODMUNRES, data = poprc_f_15, sum)
names(df_popf_15) = c("CODMUNRES","POPRC_F_15")

poprc_f_49 = dados_sidra4[dados_sidra4$F_IDADE %in% c("15 a 19 anos","20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos", "45 a 49 anos") ,]
df_popf_49 = aggregate(POPF ~ CODMUNRES, data = poprc_f_49, sum)
names(df_popf_49) = c("CODMUNRES","POPRC_F_15_49")

poprc_f_50 = dados_sidra4[dados_sidra4$F_IDADE %in% c("50 a 54 anos","55 a 59 anos","60 a 64 anos","65 a 69 anos", "70 a 74 anos","75 a 79 anos", "80 a 89 anos", "90 a 99 anos","100 anos ou mais"),]
df_popf_50 = aggregate(POPF ~ CODMUNRES, data = poprc_f_49, sum)
names(df_popf_50) = c("CODMUNRES","POPRC_F_50")

base_sidra = merge(base_sidra, df_popf_15, by="CODMUNRES", all.x = T)
base_sidra = merge(base_sidra, df_popf_49, by="CODMUNRES", all.x = T)
base_sidra = merge(base_sidra, df_popf_50, by="CODMUNRES", all.x = T)

#Adicionando a UF


uf_poprc_15_f = dados_sidra3[dados_sidra3$F_IDADE %in% c("0 a 4 anos","5 a 9 anos","10 a 14 anos") ,]
df_uf_pop15_f = aggregate(POPF ~ CODMUNRES, data = uf_poprc_15_f, sum)
names(df_uf_pop15_f) = c("CODMUNRES","POPRC_F_15")
uf_poprc_49_f = dados_sidra3[dados_sidra3$F_IDADE %in% c("15 a 19 anos","20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos","40 a 44 anos", "45 a 49 anos") ,]
df_uf_pop49_f = aggregate(POPF ~ CODMUNRES, data = uf_poprc_49_f, sum)
names(df_uf_pop49_f) = c("CODMUNRES","POPRC_F_15_49")
uf_poprc_50_f = dados_sidra3[dados_sidra3$F_IDADE %in% c("50 a 54 anos","55 a 59 anos","60 a 64 anos","65 a 69 anos", "70 a 74 anos","75 a 79 anos", "80 a 89 anos", "90 a 99 anos","100 anos ou mais"),]
df_uf_pop50_f = aggregate(POPF ~ CODMUNRES, data = uf_poprc_50_f, sum)
names(df_uf_pop50_f) = c("CODMUNRES","POPRC_F_50")

base_sidra$POPRC_F_15[base_sidra$CODMUNRES == 16] = df_uf_pop15_f$POPRC_F_15[df_uf_pop15_f$CODMUNRES == 16]
base_sidra$POPRC_F_15_49[base_sidra$CODMUNRES == 16] = df_uf_pop49_f$POPRC_F_15_49[df_uf_pop49_f$CODMUNRES == 16]
base_sidra$POPRC_F_50[base_sidra$CODMUNRES == 16] = df_uf_pop50_f$POPRC_F_50[df_uf_pop50_f$CODMUNRES == 16]

#adicionando UF.

base_sidra$NIVEL = ifelse(base_sidra$CODMUNRES == 16, "UF", "MUNICIPIO")
base_sidra <- base_sidra[, c(1, ncol(base_sidra), 2:(ncol(base_sidra)-1))]

write.csv(base_sidra, "SIDRA_AP")
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
