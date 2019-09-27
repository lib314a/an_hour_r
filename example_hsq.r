rm(list = ls()) # 清空工作空间（删除此前所有自定义变量、函数）

# Step 0: inspect CODEBOOK -----------------------------------------------------
CODEBOOK <- readLines("HSQ/codebook.txt") # 读取编码说明

tail(CODEBOOK) # 留意 CODEBOOK 的末尾，保存着问卷问题对应的幽默风格信息。其中 6-
               # 为反向计分项目。

# >> [1] "The four scale scores of the HSQ were calculated as such (php code):"
# >> [2] ""
# >> [3] "affiliative. round(((6-$_POST['Q1']) + $_POST['Q5'] + (6-$_POST['Q9']) + $_POST['Q13'] + (6-$_POST['Q17']) + $_POST['Q21'] + (6-$_POST['Q25']) + (6-$_POST['Q29']))/8, 1);"
# >> [4] "selfenhancing. round(($_POST['Q2'] + $_POST['Q6'] + $_POST['Q10'] + $_POST['Q14'] + $_POST['Q18'] + $_POST['Q22'] + $_POST['Q26'] + $_POST['Q30'])/8,1);"
# >> [5] "aggressive. round(($_POST['Q3']+ $_POST['Q7'] + $_POST['Q11'] + $_POST['Q15'] + $_POST['Q19'] + $_POST['Q23'] + $_POST['Q27'] + $_POST['Q31'])/8,1);"
# >> [6] "selfdefeating. round(($_POST['Q4'] + $_POST['Q8'] + $_POST['Q12'] + $_POST['Q16'] + $_POST['Q20'] + $_POST['Q24'] + $_POST['Q28'] + $_POST['Q32'])/8,1);"

# 借助正则表达式，可以迅速找出幽默风格对应列的列标
getQuestInd <- function(x)
{
  # 注意列标都被单引号引起来了
  string.splitted <- strsplit(x, split = "'")[[1]]
  # 找出符合 "Q+number" 这个模式的字符串
  quest.ind <- grep(string.splitted, pattern = "^Q[0-9]+$")
  # NOTE 最后一个被求值的变量或常量将作为结果**自动**返回
  questions <- string.splitted[quest.ind]
}

AFF_ITEMS <- getQuestInd(grep("affiliative", CODEBOOK, value = TRUE))
AGG_ITEMS <- getQuestInd(grep("aggressive", CODEBOOK, value = TRUE))
SEN_ITEMS <- getQuestInd(grep("selfenhancing", CODEBOOK, value = TRUE))
SDE_ITEMS <- getQuestInd(grep("selfdefeating", CODEBOOK, value = TRUE))

#c("Q3", "Q7", "Q11", "Q15", "Q19", "Q23", "Q27", "Q31")
#c("Q1", "Q5", "Q9", "Q13", "Q17", "Q21", "Q25", "Q29")
#c("Q4", "Q8", "Q12", "Q16", "Q20", "Q24", "Q28", "Q32")
#c("Q2", "Q6", "Q10", "Q14", "Q18", "Q22", "Q26", "Q30")

# 同理，找出反向计分项目的列标
getReverseItems <- function(x)
{
  string.splitted <- strsplit(x, split = "]")[[1]]
  quest.ind <- grep(string.splitted, pattern = "6\\-")
  # *apply 家族函数对向量逐一求值
  sapply(string.splitted[quest.ind], getQuestInd)
}

REV_ITEMS <- getReverseItems(paste(tail(CODEBOOK), collapse = " "))
# ------------------------------------------------------------------------------

# Step 1: check the data set ---------------------------------------------------
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(GGally)) install.packages("GGally")

IN <- read.csv("HSQ/data.csv", header = TRUE) # 读取原始数据集
OUT <- list() # 建立 OUTPUT list，以便将结果数据存放在里面

dim(IN) # 查看表格大小
head(IN) # 预览表格头部
# 查看年龄分布
table(IN$age)
qplot(IN$age)
# 查看性别分布
table(IN$gender)
qplot(IN$gender)

# Step 2: clear data -----------------------------------------------------------
# 几类要删除的被试：
SUBJ_OMIT <- with(IN,
  # 对几乎所有问题都作相同回答的被试
  apply(IN[paste("Q", 1:32, sep = "")], 1, function(x) all(abs(x-mean(x))<1)) |
  # 年龄不介于 10 至 90 之间的被试
  (age < 10) | (age > 90) |
  # 性别未填男性或女性的被试
  !(gender %in% 1:2)
)
message(sum(SUBJ_OMIT), " subjects omitted!") # 在终端里报告删除被试的数量
IN <- IN[!SUBJ_OMIT,] # 删除相应的被试

# 由于年龄是分类变量，我们将其类（class）改成 factor，以便后续处理。其余变量为数
# 值型变量（integer 或 numeric）
IN$gender <- factor(IN$gender, levels = 1:2, labels = c("Male", "Female"))

# Step 3: calculate scores for humor styles per subject -------------------------
# 先处理反向计分项：用 6 减反向计分值，从而统一到正向计分上来
IN[REV_ITEMS] <- (6 - IN[REV_ITEMS])

# data.frame 类的表格可以用 $ 符号方便地添加新的列/变量
IN$aggressive <- apply(IN[AGG_ITEMS], 1, mean)
IN$affiliative <- apply(IN[AFF_ITEMS], 1, mean)
IN$selfdefeating <- apply(IN[SDE_ITEMS], 1, mean)
IN$selfenhancing <- apply(IN[SEN_ITEMS], 1, mean)

# 至此，我们得到了所有被试在各个幽默风格上的得分情况
head(IN)

if (!require(tidyr)) install.packages("tidyr")
OUT$raw <- IN # 记录原始数据
OUT$data <- IN[!(names(IN) %in% paste("Q", 1:32, sep = ""))] # 一个没有原始数据
                                                             # 的表格
OUT$data.tall <- gather(OUT$data, "style", "score", c(4:7)) # 没有原始数据，并且
                                                            # 四个幽默风格聚合成
                                                            # 一列变量

# 目前 OUT list 中保存了三个表格：raw, data 和 data.tall。他们在后面的分析中会被分别用到。
# OUT -+-- raw: Q1 Q2 Q3 Q4 Q5 ... Q31 Q32 age gender accuracy
#      |
#      +-- data: age gender accuracy aggressive affiliative selfdefeating selfenhancing
#      |
#      +-- data.tall: age gender accuracy style score

# Step 4: Cronbach's alpha -----------------------------------------------------
cronbach.alpha <- function(x)
{
  # The average variance of each component
  calc.vbar <- function(x) mean(apply(x, 2, var))

  # The average covariances between the components, excluding the variances of
  # each component
  calc.cbar <- function(x)
  {
    r <- cov(x)
    mean(r[upper.tri(r)])
  }

  # The formula is from wikipedia: cronbach's alpha
  calc.alpha <- function(K, cbar, vbar) (K*cbar)/(vbar+(K-1)*cbar)

  calc.alpha(ncol(x), calc.cbar(x), calc.vbar(x))
}

OUT$cronbach.alpha$affiliative <- cronbach.alpha(OUT$raw[AFF_ITEMS])
OUT$cronbach.alpha$aggressive <- cronbach.alpha(OUT$raw[AGG_ITEMS])
OUT$cronbach.alpha$selfenhancing <- cronbach.alpha(OUT$raw[SEN_ITEMS])
OUT$cronbach.alpha$selfdefeating <- cronbach.alpha(OUT$raw[SDE_ITEMS])

# Step 5: visualization --------------------------------------------------------
OUT$correlation.matrix <-
  ggpairs(OUT$data, columns = 4:7, title = "Correlations between humor styles")

source("summarySE.r")
OUT$summary.se <-
  summarySE(OUT$data.tall, measurevar = "score", groupvars = c("gender", "style"))

OUT$gender.effect <-
  ggplot(data = OUT$data.tall, mapping = aes(x = gender, y = score)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    colour = "black",
    size = 1.2
  ) +
  geom_errorbar(
    mapping = aes(ymax = score+ci, ymin = score-ci),
    size = 1.2 , width = .3 , position = position_dodge(.9)
  ) +
  facet_wrap(~style, nrow = 1) +
  coord_cartesian(ylim = c(2.5, 4.3)) +
  scale_fill_manual(values = c('white')) +
  theme_bw(base_size = 18) +
  labs(x = 'Gender' , y = 'Score' , title = 'Gender effect on humor styles')

###############
# SAVE OUTPUT #
###############
saveRDS(OUT, file = "OUT.rds", ascii = TRUE)
