library(flacco)
library(data.table)
library(ggplot2)
library(doParallel)
library(xtable)

setwd('~/Downloads/Bas')
registerDoParallel(cores = 4)

t <- theme_grey() +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(2, "line"))
theme_set(t)

data_files <- c('cifar-results.csv', 'mnist-results.csv', 'fashion-results.csv')
dt_features <- list()

for (data_file in data_files) {
  dt <- fread(data_file)
  y <- dt$perf
  
  if (length(grep('narrows', data_file)) == 0) {  
    par <- list(
      filters_ = c(10, 600),
      k_ = c(1, 8),
      s_= c(1, 5),
      dense_size_ = c(0, 2000),
      dropout_ = c(1e-5, 9e-1),
      lr = c(1e-5, 1e-2),
      l2 = c(1e-5, 1e-2)
    )
  } else { # for the refined search space
    par <- list(
      filters_ = c(250, 400),
      k_ = c(3, 7),
      s_ = c(2, 5),
      dense_size_ = c(500, 1500),
      dropout_ = c(1e-1, 4e-1),
      lr = c(4e-3, 9e-3),
      l2 = c(5e-4, 3e-3)
    )
  }
  
  X <- dt[
    , -c(
      'stack_0', 'stack_1', 'stack_2', 
      'filters_1', 'filters_3', 'filters_5',
      'activation', 'activ_dense', 'max_pooling',
      'perf', 'time'
    )
  ]
  
  for (name in c('filters_', 's_', 'k_', 'dense_size_', 'dropout_', 'lr', 'l2')) {
    .normalize <- function(x) 10 * (x - par[[name]][1]) / (par[[name]][2] - par[[name]][1]) - 5
    cols <- names(X)[grep(paste0('^', name), names(X))]
    X[, (cols) := lapply(.SD, .normalize), .SDcols = cols]
  }
  
  bootstrap.size <- 800
  bootstrap.reps <- 30
  feat.sets <- c("ela_distr", "ela_meta", "disp", "nbc", "ic")
  N <- nrow(X)
  
  res <- foreach(k = 1:bootstrap.reps) %dopar% {
    idx <- sample(N, bootstrap.size)
    feat.object = createFeatureObject(X = X[idx, ], y = y[idx])
    features <- list()
    for (set in feat.sets) {
      features <- c(
        features, calculateFeatureSet(feat.object, set = set)
      )
    }
    feature_names <- names(features)
    features <- features[!grepl('costs_fun_evals|costs_runtime', feature_names) &
                           !grepl('mean_10|25', feature_names) &
                           !grepl('median|cond|max_by_min|coef\\.min|coef\\.max|peaks|quad_w_', feature_names)]
    
    data.table(
      rep = k,
      data.set = gsub('^(.*)-.*', '\\1', data_file),
      feature = names(features),
      value = unlist(features)
    )
  }
  dt_features <- c(res, dt_features)
}

dt_features <- rbindlist(dt_features)
dt_features[, feature := gsub('ela_(meta.)?', '', feature)]
dt_features[data.set == 'fashion', data.set := 'Fashion']
dt_features[data.set == 'mnist', data.set := 'MNIST']
dt_features[data.set == 'cifar', data.set := 'CIFAR-10']

p <- ggplot(dt_features, aes(x = data.set, y = value)) + 
  geom_violin(colour = 'coral') +
  geom_jitter(height = 0, width = 0.15, alpha = .5, shape = 19) +
  labs(x = '', y = '') + 
  scale_x_discrete(expand = c(0, 0)) + 
  facet_wrap(~feature, scales = 'free_y') + 
  guides(fill = guide_legend(title = NULL, nrow = 1)) + 
  theme(
    legend.position = 'bottom',
    strip.text.x = element_text(size = 19),
    axis.text.x = element_text(size = 17, angle = 0),
    axis.text.y = element_text(angle = 0, size = 16)
  )

ggsave(file.path('./', 'flacco_feature2.pdf'), plot = p, width = 500, height = 300, dpi = 100, units = 'mm')
fwrite(dt_features, 'data.csv')

dt_ <- dt_features[, 
              paste0(format(mean(value), digits = 3), '+', format(sd(value), digits = 3)), 
              by = .(data.set, feature)
            ] %>%
  dcast(feature ~ data.set, value.var = 'V1') %>% 
  setcolorder(c('feature', 'mnist', 'fashion', 'cifar')) 

xtable(dt_, align = c('l', 'l', 'r', 'r', 'r')) %>% 
  print(include.rownames = F)