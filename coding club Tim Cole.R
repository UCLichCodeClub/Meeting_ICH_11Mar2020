  # AIM to learn something about the tidyverse in R
  # plot growth charts using the pipe, map and ggplot

  # load and if necessary install libraries
  pkg <- c('gamlss', 'tidyverse', 'AGD')

  . <- sapply(pkg, function(x)
    if (!require(x, character.only = TRUE, quietly = TRUE)) {
      install.packages(x)
      require(x, character.only = TRUE, quietly = TRUE)})

  # use the pipe:  x %>% f

  # x %>% f(...)  is equivalent to
  # f(x, ...)

  # example
  squares <- data.frame(x = c(0, 1, 4, 9, 16), y = 25)
  sqrt(squares)
  squares %>%
    sqrt

  # get data from AGD package
  boys7482 %>%
    head

  # tidy data
  boys <- boys7482 %>%
    as_tibble() %>%
    select(age:hc) %>%
    mutate(bmi = wgt / (hgt/100)^2) %>%
    drop_na
  boys

  # variable names
  vars <- c("height", "weight", "bmi", "headcirc") %>%
    setNames(., .)
  names(boys) <- c('age', vars)
  boys

  # fit gamlss model for height
  # LMS method = family BCCG
  a1 <- gamlss(
    height ~ pb(sqrt(age)),   # Mu
    sigma.formula = ~ pb(age), # Sigma
    nu.formula = ~ pbz(age),   # Lambda (Nu)
    family=BCCG, data = boys)

  # define nine centiles to plot, two-thirds of a z-score apart
  (prob <- setNames(pnorm(4:-4 * 2/3),
                    c("99.6th","98th","91st","75th","50th","25th","9th","2nd","0.4th")))

  # draw centiles
  centiles(a1, cent = prob * 100, legend = FALSE)
  # add grid
  grid()

  # specific AIM
  # to draw centiles for all four measurements in a 2x2 array

  # age grid for plot
  age_grid <- tibble(age = seq(0, max(boys$age), by = 0.1))

  # reshape data from wide to long
  # put all four measurements in single column and copy age
  model_data <- boys %>%
    pivot_longer(- age, names_to = "y", values_to = "measurement") %>%
    mutate(y = fct_inorder(y))
  boys
  model_data

  # use map for loops

  # map(.x = list, .f = function)
  # returns a list the same length as .x
  # each element of .x has .f applied to it

  squares
  map(.x = squares$x, .f = sqrt)
  map_dbl(.x = squares$x, .f = sqrt)
  map_chr(.x = squares$x, .f = sqrt)
  map_dfc(.x = squares$x, .f = sqrt)
  map_dfr(.x = setNames(squares$x, squares$x), .f = sqrt)

  # loop to fit models - takes ~2 minutes
  models <- map(.x = vars, ~ gamlss(
    get(.x) ~ pb(sqrt(age)), # .x substitutes variable name
    sigma.formula = ~ pb(age),
    nu.formula = ~ pbz(age),
    family=BCCG, data = boys))

  # models is a list of four models, one for each measurement
  typeof(models)
  length(models)
  names(models)

  # loop to extract centiles per model
  quiet <- quietly(predictAll)
  model_centiles <- map_dfr(.x = models, ~ { # models
    LMS <- quiet(.x, age_grid)$result
    map_dfc(.x = as.list(prob), function(p) { # centiles
      with(LMS, qBCCG(p, mu = mu, sigma = sigma, nu = nu))
    }) %>%
      bind_cols(age_grid) %>%
      pivot_longer(-age, names_to = "centile", values_to = "measurement") %>%
      mutate(centile = fct_relabel(factor(centile), ~ names(prob)))
  }, .id = 'y') %>%
    mutate(y = fct_inorder(y))
  model_centiles

  # plot data and centiles - one layer at a time
  (p <- ggplot(data = model_centiles, mapping = aes(age, measurement)))
  (p <- p + geom_point(data = model_data, colour = 'gray', size = 0.5))
  (p <- p + geom_path(mapping = aes(colour = centile)))
  (p <- p + facet_wrap(~ y, scales = 'free_y'))
  # (p <- p + theme_bw()) # bug

  # whole plot together
  ggplot(mapping = aes(age, measurement)) +
    geom_point(data = model_data, colour = 'gray', size = 0.5) +
    geom_path(data = model_centiles, mapping = aes(colour = centile)) +
    facet_wrap(~ y, scales = 'free_y') +
    theme_bw() +
    NULL

  ggsave('coding club centiles.pdf', w=7, h=7)
  save(models, model_data, model_centiles, file = 'saved results')
