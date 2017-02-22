
############## stats descriptives
mean_sd_by_modal = function(data, var_y, var_x, scale_y = 0){
  ### fonction qui permet de representer pour une variable quali, la moyenne et l'ecart type par modalite, ainsi que la
  ### la distribution de var_x
  # data : data.frame ; contient var_y et var_x
  # var_y : string ; nom de la variable Y
  # var_x : string ; nom de la variable X
  # scale_y : numeric >= 0 (par defaut 0) ; permet d'elargir la fenetre des valeurs de Y
  stat = aggregate(x = data[,var_y],
                   by = list(var_x = data[,var_x]),
                   FUN = function(x){c(moyenne = mean(x),ecart_type = sd(x))})
  
  stat$moyenne = stat$x[,"moyenne"]
  stat$ecart_type = stat$x[,"ecart_type"] / 4
  stat$borne_inf = stat$moyenne - stat$ecart_type
  stat$borne_sup = stat$moyenne + stat$ecart_type
  stat$x2 = stat$var_x
  
  plot1 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) ,]) + # & (data[,var_y] > 0)
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size =10)) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  plot2 = ggplot(data = stat, aes(x = factor(x2), y = moyenne, group = 1)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = borne_inf, ymax = borne_sup), fill = "blue", alpha = 0.3) +
    theme(text = element_text(size=17),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x = element_blank()) +
    ylab(paste0("Moyenne ", var_y)) +
    ggtitle(paste0("Moyenne par modalite +/- 1/4.ecart type : ",var_x)) +
    scale_y_continuous(limits = c(stat$borne_inf[which.min(stat$borne_inf)] - 
                                    (stat$moyenne[which.min(stat$borne_inf)] -
                                       stat$borne_inf[which.min(stat$borne_inf)]) * scale_y,
                                  stat$borne_sup[which.max(stat$borne_sup)] - 
                                    (stat$moyenne[which.max(stat$borne_sup)] -
                                       stat$borne_sup[which.max(stat$borne_sup)]) * scale_y ) )
  
  multiplot(plot2, plot1, cols = 1)
}

scatter_plot_int = function(var_y, var_x, data, y_lim_fenetre_haut, y_bin){
  
  if(!missing(y_bin)){
    data[,var_x] = data[,var_x] - (data[,var_x] %% y_bin)
  }
  a = data[,c(var_x, var_y)]
  colnames(a) = c("x","y")
  to_plot = a %>% 
    group_by(x) %>% 
    summarise(y = mean(y))
  
  if(missing(y_lim_fenetre_haut)){
    plot1 = ggplot(data = to_plot, aes(x = x, y = y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red", method = 'loess') +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  } else {
    plot1 = ggplot(data = to_plot, aes(x = x, y = y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red", method = 'loess') +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ylim(y_lim_fenetre_haut) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  
  plot2 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) & (data[,var_y] > 0),]) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  multiplot(plot1, plot2, cols = 1)
}


scatter_plot = function(var_y, var_x, data, y_lim_fenetre_haut){
  
  if(missing(y_lim_fenetre_haut))
  {
    plot2 = ggplot(data = data, aes_string(x = var_x, y = var_y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      geom_smooth(colour = "red") +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  else
  {
    plot2 = ggplot(data = data, aes_string(x = var_x, y = var_y)) +
      geom_point(alpha = 0.3, colour = "blue") +
      ylim(y_lim_fenetre_haut) +
      geom_smooth(colour = "red") +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      ggtitle(paste0("Influence de la variable ",var_x, " sur ",var_y))
  }
  
  plot1 = ggplot( aes_string(x = var_x) , 
                  data = data[!is.na(data[,var_y]) ,]) + # & (data[,var_y] > 0)
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size =10)) +
    #scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 17)) +
    ylab(paste0("Distribution ",var_x)) +
    xlab("")
  
  multiplot(plot2, plot1, cols = 1)
}

####################### GLM bootstrap

perf_glm = function(data , var_y, vars_x, family , n_train , n_test, n_repet, fonc_lien_inverse){
  ### fonction qui permet d'Ã©valuer le R2 et l'indice de Gini d'un modÃ¨le par bootstrap, en utilisant diffÃ©rentes taille
  ### d'Ã©chantillon train
  # data : data.frame ; contient les variables var_y et vars_x
  # var_y : string ; nom de la variable Y
  # vars_x : vecteur de string ; noms des variables X
  # family : objet de type "family" ou bien string ; famille de distribution Ã  utiliser dans le GLM
  # n_train : vecteur d'entiers ; diffÃ©rentes tailles d'Ã©chantillons train
  # n_test : entier ; taille de l'Ã©chantillon test (unique pour tte les taille de train)
  # n_repet : entier ; nb. de "bootstrap" effectuÃ©s pour une taille de train
  # fonc_lien_inverse : fonction ; inverse de la fonction lien utiliser dans le GLM (exemple lien log -> fonc_lien_inverse = exp )
  formula = paste0(var_y, " ~ ", paste0(vars_x, collapse = "+"))
  for (i in 1:length(n_train)){
    for (j in 1:n_repet){
      cat(i," * ",j,"\n")
      train_lines = sample(1:dim(data)[1], size = n_train[i], replace = T)
      test_lines = sample(setdiff(1:dim(data)[1],train_lines), n_test , replace = T)
      res_glm = glm( formula = formula ,
                     data = data[train_lines,],
                     family = family)
            
      R2_train = 1 - sum( (data[train_lines, var_y] - res_glm$fitted.values )^2) /
        (sd(data[train_lines, var_y])^2 * (length(train_lines)-1) )
      
      R2_test = 1 - sum( (data[test_lines, var_y] - fonc_lien_inverse(predict(res_glm, newdata = data[test_lines,])) )^2) /
        (sd(data[test_lines, var_y])^2 * (length(test_lines)-1) )
      
      Gini_train = NormalizedGini(solution = data[train_lines, var_y], 
                                  submission = res_glm$fitted.values)
      Gini_test = NormalizedGini(solution = data[test_lines, var_y],
                                 submission = fonc_lien_inverse(predict(res_glm, newdata = data[test_lines,])))
      if(i == 1 & j == 1){
        resultats = data.frame(n_train = n_train[i], 
                               R2_train = R2_train,
                               R2_test = R2_test, 
                               Gini_train = Gini_train, 
                               Gini_test = Gini_test)
      } else {
        resultats = rbind(resultats, c(n_train[i], R2_train, R2_test, Gini_train, Gini_test))
      }
    }
  }
  return(round(resultats,5))
}

coef_glm = function(data , var_y, vars_x, family , n_train, n_repet){
  ### fonction qui permet de fitter plusieurs GLM sur des Ã©chantillons "train" bootstrap de tailles n_train et de renvoyer
  ### un data.frame avec tous les coefficients Ã©valuÃ©s
  # voir fonction "perf_glm" pour le dÃ©tail des arguments
  formula = paste0(var_y, " ~ ", paste0(vars_x, collapse = "+"))
  for (i in 1:length(n_train)){
    for (j in 1:n_repet){
      cat(i," * ",j,"\n")
      train_lines = sample(1:dim(data)[1], size = n_train[i], replace = T)
      res_glm = glm( formula = formula ,
                     data = data[train_lines,],
                     family = family)
      
      if(i == 1 & j == 1){
        resultats = as.data.frame(res_glm$coefficients)
      } else {
        resultats = cbind(resultats, res_glm$coefficients)
      }
    }
  }
  return(round(resultats, 5))
}

sd_coef_bootstrap = function(data , var_y, vars_x, family , n_train, n_repet){
  ### fonction qui permet d'Ã©valuer l'Ã©cart type des coefficients d'un GLM par mÃ©thode bootstrap
  # voir fonction "perf_glm" pour le dÃ©tail des arguments
  formula = paste0(var_y, " ~ ", paste0(vars_x, collapse = "+"))
  for (i in 1:length(n_train)){
    for (j in 1:n_repet){
      cat(i," * ",j,"\n")
      train_lines = sample(1:dim(data)[1], size = n_train[i], replace = T)
      res_glm = glm( formula = formula ,
                     data = data[train_lines,],
                     family = family)
      
      if(i == 1 & j == 1){
        resultats = as.data.frame(res_glm$coefficients)
      } else {
        resultats = cbind(resultats, res_glm$coefficients)
      }
    }
  }
  return(apply(resultats, MARGIN = 1, FUN = sd))
}

# sd_manual = sd_coef_bootstrap(data = data[!is.na(data$cout_moyen_s_DTA) & (data$cout_moyen_s_DTA > 0),],
#                             var_y = "cout_moyen_s_DTA",
#                             vars_x = c("Marque","Alimentation","PTAC","Vitesse_maxi","Controle_dynamique_de_stabilite","Composite"),
#                             family = "poisson" ,
#                             n_train = sum(!is.na(data$cout_moyen_s_DTA) & (data$cout_moyen_s_DTA > 0)),
#                             n_repet = 50)


####################### GLM : visualisation sorties

plot_coef_glm_quali = function(res_glm, var_x,data, is_int_conf, 
                               sd_manual, order_modal){
  ### fonction qui permet de visualiser pour une variable quali les coefficients du GLM associÃ©s Ã  chaque modalitÃ© ainsi que
  ### l'intervalle de confiance associÃ© Ã  chaque coefficient
  ### Si sd_manual n'est pas renseignÃ© la fonction utilisera par dÃ©faut l'intervalle de confiance fourni dans l'objet GLM
  ### Les Ã©carts types donnÃ©s par le GLM sont parfois faux (dans le GLM poisson par exemple) et alors sd_manual 
  ### permet de spÃ©cifier un vecteur d'Ã©cart type pour les coefficients
  # res_glm : objet GLM ; output d'un GLM
  # var_x : string ; variable dont on souhaite faire le graphique
  # data : data.frame ; contient la variable "var_x"
  # is_int_conf : bool ; reprÃ©sente t-on les intervalle de confiance
  # sd_manual : vecteur d'Ã©carts types pour les coefs
  # order_modal : vecteur de string ; ordre (de gauche Ã  droite) par lequel on souhaite ordonner les modalitÃ©s
  # sur le graphique
  
  if (!missing(sd_manual)){
    sd = sd_manual
  } else {
    sd = summary(res_glm)$coefficients[,"Std. Error"]
  }
  to_plot = data.frame(coef = res_glm$coefficients,
                       inf_5_pourcent = res_glm$coefficients - 1.96 * sd,
                       sup_5_pourcent = res_glm$coefficients + 1.96 * sd)
  to_plot = to_plot[grep(pattern = var_x, row.names(to_plot)),]
  new_names = levels(data[,var_x])
  to_plot = rbind(c(0,0,0),to_plot)
  to_plot$legende = factor(new_names, levels = new_names)
  if (!missing(order_modal)){
    to_plot = join(x = data.frame(legende = order_modal),
                    y = to_plot,
                    by = "legende",
                    type = "inner")
  } else {
  to_plot = to_plot[order(as.character(to_plot$legende)),]
  }
  to_plot$legende = as.character(to_plot$legend)
  to_plot$legende = factor(to_plot$legende, levels = to_plot$legende)
  if (is_int_conf){
    ggplot(data = to_plot, aes(x = legende , y = coef)) +
      #geom_line(size = 1.5) +
      geom_pointrange(aes(ymin = inf_5_pourcent, ymax = sup_5_pourcent),
                      size = 1.2, colour = "blue",
                      fill = "white", shape = 22) +
      #guides(colour = guide_legend(override.aes = list(size = 10), title = "LÃ©gende : ") ) +
      theme(legend.position="bottom",text = element_text(size=20)) +
      ggtitle(paste0("Coefficients + int. conf. 95% : ",var_x)) +
      ylab("Coefficients") +
      xlab("ModalitÃ©s")
  } else {
    ggplot(data = to_plot, aes(x = legende , y = coef, group = 1)) +
      geom_line(size = 1.2, colour = "blue") +
      #guides(colour = guide_legend(override.aes = list(size = 10), title = "LÃ©gende : ") ) +
      theme(legend.position="bottom",text = element_text(size=20)) +
      ggtitle(paste0("Coefficients : ",var_x)) +
      ylab("Coefficients") +
      xlab("ModalitÃ©s")
  }
}


###################### CART bootstrap

perf_CART = function(data , var_y, vars_x, method, n_train , n_test, n_repet, vect_cp){
  formula = paste0(var_y, " ~ ", paste0(vars_x, collapse = "+"))
  for (i in 1:length(vect_cp)){
    for (j in 1:n_repet){
      cat(i," * ",j,"\n")
      train_lines = sample(1:dim(data)[1], size = n_train, replace = T)
      test_lines = sample(setdiff(1:dim(data)[1],train_lines), n_test , replace = T)
      res_CART = rpart( formula = formula ,
                        data = data[train_lines,],
                        method = method,
                        control = rpart.control(cp = vect_cp[i], xval = 2))
      
      R2_train = 1 - sum( (data[train_lines, var_y] - predict(res_CART, newdata = data[train_lines,] ) )^2) /
        (sd(data[train_lines, var_y])^2 * (length(train_lines)-1) )
      
      R2_test = 1 - sum( (data[test_lines, var_y] - predict(res_CART, newdata = data[test_lines,]) )^2) /
        (sd(data[test_lines, var_y])^2 * (length(test_lines)-1) )
      
      Gini_train = NormalizedGini(solution = data[train_lines, var_y], 
                                  submission = predict(res_CART, newdata = data[train_lines,] ))
      Gini_test = NormalizedGini(solution = data[test_lines, var_y],
                                 submission = predict(res_CART, newdata = data[test_lines,]))
      if(i == 1 & j == 1){
        resultats = data.frame(cp = vect_cp[i], 
                               R2_train = R2_train,
                               R2_test = R2_test, 
                               Gini_train = Gini_train, 
                               Gini_test = Gini_test)
      } else {
        resultats = rbind(resultats, c(vect_cp[i], R2_train, R2_test, Gini_train, Gini_test))
      }
    }
  }
  return(round(resultats,5))
}

##################### RF : visualisation sorties

plot_rfSRC_importance = function(rfSRC, nb_variable){
  ## cette foonction trace le graphique des variables les plus importantes dans la random forest
  ## rfSRC : objet output rfsrc
  ## nb_variable : nb de variable Ã  faire apparaitre sur le graphique
  to_plot = data.frame(variable = names(rfSRC$importance), importance = rfSRC$importance)
  to_plot = to_plot[order(to_plot$importance, decreasing = T),]
  nb_var = min(nb_variable,dim(to_plot)[1])
  to_plot = to_plot[1:nb_var,]
  ggplot(data = to_plot, aes(x = factor(variable,levels=rev(unique(variable))) , weight = importance)) +
    geom_bar() +
    theme(text = element_text(size = 20)) +
    coord_flip() +
    ggtitle(paste0(nb_var," variables les plus importantes")) +
    xlab("Importance des variables") +
    ylab("")
}

plot_influence_variable_quanti_rfSRC = function(rfSRC, var_x){
  to_plot = data.frame(
    x = rfSRC$xvar[,var_x],
    y = rfSRC$predicted)
  ggplot(data = to_plot, aes(x = x, y = y))+
    geom_point(alpha = 0.3) +
    geom_smooth() +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle(paste0("PrÃ©diction du modÃ¨le en fonction de ", var_x)) +
    ylab("PrÃ©diction") +
    xlab(var_x)
}

plot_influence_variable_quali_rfSRC = function(rfSRC, var_x){
  to_plot = data.frame(x = rfSRC$xvar[,var_x], y = rfSRC$predicted)
  ggplot(data = to_plot, aes(x = x, y = y)) +
    geom_boxplot() +
    theme(text = element_text(size = 20)) +
    ggtitle(paste0("PrÃ©diction de ",rfSRC$yvar.names," en fonction de ", var_x)) +
    xlab("ModalitÃ©s") +
    ylab("PrÃ©dictions")
}

plot_error_rfSRC = function(rfSRC){
  Variance_y = sd(rfSRC$yvar)^2
  to_plot = data.frame(x = 1:rfSRC$ntree , y =  rfSRC$err.rate / Variance_y)
  ggplot(data = to_plot, aes(x=x, y =y)) + 
    geom_line(size = 2, colour = "blue") +
    theme(text = element_text(size = 20)) +
    ggtitle("Evolution de 1-R2 OOB") +
    xlab("Nb. arbres") +
    ylab("% erreur OOB")
}

##################### RF : calibrage

calibre_rf = function(data,var_y, vars_x, vect_nodedepth, vect_nodesize, vect_ntree, vect_nsplit){
  
  grid_param = expand.grid(nodedepth = vect_nodedepth,
                           nodesize = vect_nodesize,
                           ntree = vect_ntree,
                           nsplit = vect_nsplit)
  
  formula = paste0(var_y, " ~ ", paste0(vars_x, collapse = "+"))
  formula = as.formula(formula)
  for (i in 1:dim(grid_param)[1]){
    cat(i, " sur ", dim(grid_param)[1],"\n")
    rf = rfsrc(formula = formula,
               data = data,
               ntree = grid_param$ntree[i],
               nodedepth = grid_param$nodedepth[i],
               nodesize = grid_param$nodesize[i],
               nsplit = grid_param$nsplit[i],
               importance = "none")
    
    is_OOB = !is.na(rf$predicted.oob)
    
    R2_train = 1 - sum( (data[, var_y] - rf$predicted )^2) /
      (sd(data[, var_y])^2 * (length(data[, var_y])-1) )
    
    R2_OOB = 1 - sum( (data[is_OOB, var_y] - rf$predicted.oob[is_OOB] )^2) /
      (sd(data[is_OOB, var_y])^2 * (length(data[is_OOB, var_y])-1) )
    
    Gini_train = NormalizedGini(solution = data[, var_y], 
                                submission = rf$predicted)
    Gini_OOB = NormalizedGini(solution = data[is_OOB, var_y], 
                              submission = rf$predicted.oob[is_OOB])
    
    if(i == 1){
      resultats = data.frame(R2_train = R2_train,
                             R2_OOB = R2_OOB, 
                             Gini_train = Gini_train, 
                             Gini_OOB = Gini_OOB)
    } else {
      resultats = rbind(resultats, c(R2_train, R2_OOB, Gini_train, Gini_OOB))
    }
  }
  return(cbind(grid_param,resultats))
}


############# Indice de Gini

SumModelGini <- function(solution, submission) {
  ## fonction appel?e par NormalizedGini
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}

NormalizedGini <- function(solution, submission) {
  # function qui calcul l'indicateur de performance de Gini d'un mod?le
  # solution : vecteur des vraies valeurs
  # submission : vecteur des valeurs pr?dites
  # (c'est mieux si solution et submission sont pris sur un ?chantillon test et pas le train)
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}


############# pour graphique multiple sur une meme image


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  ## fonction qui permet de faire des images avec plusieurs graphiques avec ggplot2
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



