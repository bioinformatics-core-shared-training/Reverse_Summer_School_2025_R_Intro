library(tidyverse)

metabric <- read_csv("data/metabric_clinical_and_expression_data.csv")

class(metabric)
is.data.frame(metabric)
is_tibble(metabric)

metabric

# first plot using ggplot
ggplot( data=metabric) +
  geom_point(mapping = aes(x=GATA3, y= ESR1) )


ggplot(data=metabric) +
  geom_point(mapping = aes(x=GATA3, y=ESR1, colour = Nottingham_prognostic_index))


ggplot(data=metabric) +
  geom_point(mapping = aes(x=GATA3, y=ESR1, size=ER_status) )

ggplot(data=metabric) +
  geom_point(mapping = aes( x = GATA3, y=ESR1, colour = Nottingham_prognostic_index) )

ggplot(data=metabric) +
  geom_point(mapping = aes( x = GATA3, y=ESR1, colour = PAM50, shape = `3-gene_classifier`) )


ggplot(data=metabric) +
  geom_point(mapping = aes( x = GATA3, 
                            y=ESR1, 
                            colour = PAM50, 
                            shape = `3-gene_classifier`), size=5, alpha = 0.3 )



ggplot(data=metabric) +
  geom_point(mapping = aes( x = GATA3, y=ESR1, size = ER_status) )

# bar plot

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster))

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill=Integrative_cluster))


ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill=`3-gene_classifier`))



ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill="blue"))

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill=ER_status))

metabric$fill <- "blue"

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill="blue"))

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster), fill="blue")



# statistical transformations


ggplot(data=metabric) +
  geom_point(mapping=aes(x=GATA3, y=ESR1)) +
  geom_smooth(mapping=aes(x=GATA3, y=ESR1))


ggplot(data=metabric, mapping = aes(x=GATA3, y=ESR1) ) +
  geom_point(size=0.5, alpha=0.3, color="red") +
  geom_smooth() 



ggplot(data=metabric, mapping = aes(x=GATA3, y=ESR1, colour=ER_status) ) +
  geom_point(shape=5) +
  geom_smooth(mapping = aes(x=GATA3, y=ESR1), inherit.aes = FALSE ) 

# box plots

ggplot(data=metabric, mapping = aes(x=ER_status, y=ESR1)) +
  geom_boxplot() +
  geom_point()

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ggplot(data=metabric, mapping = aes(x=ER_status, y=ESR1)) +
  geom_boxplot() +
  geom_jitter(size=0.5, alpha=0.2, width = 0.2)+
  stat_summary(fun=median, color="red") +
  stat_summary(fun=mean, col="blue") +
  stat_summary(fun=getmode, color="orange")



ggplot(data=metabric, mapping = aes(x = ER_status, y=ESR1, color=HER2_status)) +
  geom_boxplot()


# Time series plots
library(readxl)
transport_use <- read_xlsx("data/2020-04-26_COVID-19_Press_Conference_Data.xlsx",
                           
                           sheet = "Transport use",
                           col_names = c("date", "transport_type", "percentage"),
                           col_types = c("date", "text", "numeric"),
                           skip = 4,
                           n_max = 186
                           )
transport_use$percentage <- transport_use$percentage * 100

table(transport_use$transport_type)


ggplot(data=transport_use) +
  geom_line(mapping = aes(x=date, y=percentage, linetype=transport_type))

# histogram
ggplot(data=metabric, mapping = aes(x=Age_at_diagnosis)) +
  geom_histogram(binwidth = 5)

# factors in plots

ggplot(data=metabric) +
  geom_point(mapping = aes(x=GATA3, y=ESR1, colour = Tumour_stage))

table(metabric$Tumour_stage)

metabric$Tumour_stage <- as.factor(metabric$Tumour_stage)
metabric$Tumour_stage

ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill=Integrative_cluster))

table(metabric$Integrative_cluster)
metabric$Integrative_cluster <- factor(metabric$Integrative_cluster,
                                       levels = c("1","2","3", "4ER-", "4ER+", "5", "6", "7", "8", "9", "10")
                                       )



p <- ggplot(data=metabric) +
  geom_bar(mapping = aes(x=Integrative_cluster, fill=Integrative_cluster))
p
class(p)

is.list(p)
p$scales

