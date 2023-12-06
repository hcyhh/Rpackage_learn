# -------------------------------------------------------------------------#
# simulation2                                                              #          
# Y 连续 M W二分类                                                        #
#                                                           by Congyuan He #                                                                      
#                                                           by 2023.11.20  #                                                                     
# -------------------------------------------------------------------------#
rm(list=ls())
# 1.Path ####
path_data <-
  c("/Users/hecongyuan/Documents/Study/R/R_github/Graduation/Graduation Project/Data/")
path_function <-
  c("/Users/hecongyuan/Documents/Study/R/R_github/Graduation/Graduation Project/Function/")
#path_data <- c("/home/20143248/perl5/hcy/data/")
# 2.Package ####
#library(tidyverse)
library(purrr)
library(MASS)


# 3.data generate####
set.seed(1729)

expit<-function(x) exp(x)/(1+exp(x))

n<-100000



C <- rnorm(n)
A<-rbinom(n,1,prob=0.5)
L <- rnorm(n,mean=(-1.89+0.37*C+1.3*A),sd=0.5)
M<-rbinom(n,size=1,expit(x=-1.26+0.73*L+0.95*A+0.26*C))
W<-rbinom(n,size=1,expit(x=.19+0.67*C+0.95*L+0.95*A+0.55*M))
Y<-rnorm(n,mean=(0.122+0.95*A+0.65*L+0.5*C+0.75*W+0.85*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)
data <-as.data.frame(cbind(A=A,Y=Y,C=C,M=M,W=W,L=L))

#抽象函数
C <- rnorm(n)
A<-rbinom(n,1,prob=0.5)
L.fun <- function(C,A)rnorm(n,mean=(-1.89+0.37*C+1.3*A),sd=0.5)
M.fun <- function(A,L,C)rbinom(n,size=1,expit(x=-1.26+0.73*L+0.95*A+0.26*C))
W.fun <- function(A,L,C,M)rbinom(n,size=1,expit(x=.19+0.67*C+0.95*L+0.95*A+0.55*M))
Y.fun <- function(A,L,C,M,W)rnorm(n,mean=(0.122+0.95*A+0.65*L+0.5*C+0.75*W+0.85*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)


# 4.decomposition  ####
#混杂暴露指定
pre_cov <- c("C")
treatment <- c("A")

#模型指定
m_form <- M ~ C * A
w_form <- W ~ C * A + M
y_form <- Y ~ C*A+M+W+A*W+A*M+L+M*W+A*M*W

#链接函数
m1.family <- binomial(link = "logit")
m2.family <-binomial(link = "logit")
l.family <- gaussian(link = "identity")
y.family <- gaussian(link = "identity")

#L的模型
m1 <- glm(L~C*A,data=data,family = l.family)
zmodels <- list(m1)


decompz <- function(treatment, pre_cov, zmodels, y_form, m_form,
                    m1.family = m1.family,
                    m2.family = m2.family,
                    l.family = l.family,
                    y.family = y.family,
                    bootstrap = TRUE, iterations = 1000,
                    data){
  
  #获取中介变量、结果变量、L
  mediator1 <- all.vars(m_form)[1L]
  mediator2 <- all.vars(w_form)[1L]
  outcome <- all.vars(y_form)[1L]
  post_cov <- vapply(zmodels, function(x) names(x[["model"]])[1], character(1L))
  
  
  a <- data[[treatment]]
  m <- data[[mediator1]]
  w <- data[[mediator2]]
  y <- data[[outcome]]
  x <- data[, pre_cov, drop = FALSE]
  z <- data[, post_cov, drop = FALSE]
  
  #复制数据
  data_ed <- data
  
  #获取C和L的残差
  for(i in seq_along(x)) data_ed[[names(x)[i]]] <- x[[i]]-mean(x[[i]])
  for(i in seq_along(z)) data_ed[[names(z)[i]]] <- z[[i]] - zmodels[[i]][["fitted.values"]]
  
  #重新拟合模型
  m_model1 <- glm(formula = m_form, family = m1.family, data = data_ed)
  m_model2 <- glm(formula = w_form, family = m2.family, data = data_ed)
  y_model <- glm(formula = y_form,family = y.family, data = data_ed)
  
  #整理
  object <- list(y_model = y_model, m_model1 = m_model1, m_model2 = m_model2 ,zmodels = zmodels,
                 var_names = list(treatment = treatment,
                                  mediator1 = mediator1,
                                  mediator2 = mediator2,
                                  outcome = outcome,
                                  pre_cov = pre_cov,
                                  post_cov = post_cov),
                 data = data, data_ed = data_ed)
  
  
  #object <- out
  decomp <- function(object){
    
    #定义a1,a0,m*
    a0 = 0
    a1 = 1
    m = 0
    
    #选择系数函数
    `%||%` <- function(a, b) if (!is.na(a)) a else b
    
    var_names <- object$var_names
    
    coefs_y <- coef(object$y_model)
    
    beta2 <- coefs_y[var_names$treatment]
    beta4 <- coefs_y[var_names$mediator1]
    beta5 <- coefs_y[var_names$mediator2]
    
    beta6 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator1)] %||%
                coefs_y[paste0(var_names$mediator1,":",var_names$treatment)]) %||% 0
    beta7 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2)] %||%
                coefs_y[paste0(var_names$mediator2,":",var_names$treatment)]) %||% 0
    beta8 <- (coefs_y[paste0(var_names$mediator2,":",var_names$mediator1)] %||%
                coefs_y[paste0(var_names$mediator1,":",var_names$mediator2)]) %||% 0
    beta9 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2,":",var_names$mediator1)] %||%
                coefs_y[paste0(var_names$treatment,":",var_names$mediator1,":",var_names$mediator2)]) %||% 0
    
    # pred1 and pred0 from the mediator model
    if (object$m_model1$family$link != "identity"){
      
      newdata0 <- newdata1 <- object$data_ed
      newdata0[[var_names$treatment]] <- a0
      newdata1[[var_names$treatment]] <- a1
      
      pred0 <- mean(predict.glm(object$m_model1, newdata0, type = "response"))
      pred1 <- mean(predict.glm(object$m_model1, newdata1, type = "response"))
      
      w00 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=0), type = "response"))
      w0m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred0), type = "response"))
      w0m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred1), type = "response"))
      
      w10 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=0), type = "response"))
      w1m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred0), type = "response"))
      w1m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred1), type = "response"))
      
    } else {
      
      coefs_m1 <- coef(object$m_model1)
      coefs_m2 <- coef(object$m_model2)
      
      theta0 <- coefs_m1["(Intercept)"]
      theta2 <- coefs_m1[var_names$treatment]
      
      epsilon0 <- coefs_m2["(Intercept)"]
      epsilon2 <- coefs_m2[var_names$treatment]
      epsilon3 <- coefs_m2[var_names$mediator1]
      
      pred0 <- theta0 + theta2 * a0
      pred1 <- theta0 + theta2 * a1
      
      w00 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * 0
      w0m0 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred0
      w0m1 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred1
      
      w10 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * 0
      w1m0 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred0
      w1m1 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred1
    }
    
    #效应参数估计
    NDE <-  (((a1-a0)*(beta2+beta6*(pred0)+beta7*w0m0+beta9*pred0*w0m0)))
    
    CDE <- ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
    
    INT.re.am <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
    INT.re.aw <- ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
    INT.re.amw <- (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))-
      ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-
      ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))+
      ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
    INT.ref <- INT.re.am+INT.re.aw+INT.re.amw
    #CDE+INT.ref
    
    
    NIE <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*(pred1-pred0)*a1+beta7*a1*(w1m1-w0m0)+
              beta8*(pred1*w1m1-pred0*w0m0)+beta9*a1*(pred1*w1m1-pred0*w0m0))
    
    INT.med.am <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
    
    INT.med.aw <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
    
    INT.med.amw <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w1m1+beta9*pred1*w1m1))-
      ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-
      ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))+
      (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
    
    INT.med <- INT.med.am+INT.med.aw+INT.med.amw
    
    
    PIE.m <- (pred1-pred0)*(beta4+beta6*(a0)+beta8*w0m1+beta9*w0m1*a0)
    PIE.w <- (w1m0-w0m0)*(beta5+beta7*(a0)+beta8*pred0+beta9*pred0*a0)
    PIE.mw <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*a0*(pred1-pred0)+beta7*a0*(w1m1-w0m0)+
                 beta8*(pred1*w1m1-pred0*w0m0)+beta9*a0*(pred1*w1m1-pred0*w0m0))-PIE.m-PIE.w
    
    
    PIE <- PIE.m+PIE.w+PIE.mw
    
    PIE+INT.med
    
    
    
    #计算真值
    ##  Y(a,m*,w*) Y(a*,m*,w*)
    Y100 <- Y.fun(1,L.fun(C,1),C,0,0)
    Y000 <- Y.fun(0,L.fun(C,0),C,0,0)
    ##  Y(a,M(a*),w*) Y(a*,M(a*),w*)
    Y10.0 <- Y.fun(1,L.fun(C,1),C,pred0,0)
    Y00.0 <- Y.fun(0,L.fun(C,0),C,pred0,0)
    ##  Y(a,m*,W(a*)) Y(a*,m*,W(a*))
    Y100. <- Y.fun(1,L.fun(C,1),C,0,w00)
    Y000. <- Y.fun(0,L.fun(C,0),C,0,w00)
    ##  Y(a,M(a*),W(a*)) Y(a*,M(a*),W(a*))
    Y10.0. <- Y.fun(1,L.fun(C,1),C,pred0,w0m0)
    Y00.0. <- Y.fun(0,L.fun(C,0),C,pred0,w0m0)
    ##  Y(a,M(a),W(a*))  Y(a*,M(a),W(a*))
    Y11.0. <- Y.fun(1,L.fun(C,1),C,pred1,w0m1)
    Y01.0. <- Y.fun(0,L.fun(C,0),C,pred1,w0m1)
    ##  Y(a,M(a*),W(a))  Y(a*,M(a*),W(a))
    Y10.1. <- Y.fun(1,L.fun(C,1),C,pred0,w1m0)
    Y00.1. <- Y.fun(0,L.fun(C,0),C,pred0,w1m0)
    ##  Y(a,M(a),W(a))   Y(a*,M(a),W(a))
    Y11.1. <- Y.fun(1,L.fun(C,1),C,pred1,w1m1)
    Y01.1. <- Y.fun(0,L.fun(C,0),C,pred1,w1m1)
    
    CDE.real <-mean(Y100-Y000)
    INT.re.am.real <- mean((Y10.0 - Y00.0)) - mean((Y100 - Y000))
    INT.re.aw.real <- mean((Y100. - Y000.)) -mean((Y100 - Y000))
    INT.re.amw.real <- mean(Y10.0. - Y00.0.)-mean(Y10.0 - Y00.0)-mean(Y100. - Y000.)+mean(Y100 - Y000)
    
    INT.ref.real <- mean(Y10.0.-Y00.0.)-mean(Y100-Y000)
    NDE.real <-mean(Y10.0.-Y00.0.)
    #INT.ref.real <- INT.re.am.real+INT.re.aw.real+INT.re.amw.real
    #NDE.real <- CDE.real+INT.re.am.real+INT.re.aw.real+INT.re.amw.real
    
    
    INT.med.am.real <- mean((Y11.0. - Y01.0.)-(Y10.0. - Y00.0.))
    INT.med.aw.real <- mean((Y10.1. - Y00.1.)-(Y10.0. - Y00.0.))
    INT.med.amw.real <- mean((Y11.1. - Y01.1.)-(Y11.0. - Y01.0.)-(Y10.1. - Y00.1.)+(Y10.0. - Y00.0.))
    
    INT.med.real <-mean(Y11.1.-Y01.1.)-mean(Y10.0.-Y00.0.)
    
    #INT.med.real <- INT.med.am.real+INT.med.aw.real+INT.med.amw.real
    
    
    PIE.m.real <- mean(Y01.0. - Y00.0.)
    PIE.w.real <- mean(Y00.1. - Y00.0.)
    PIE.mw.real <- mean(Y01.1. - Y01.0. - Y00.1. + Y00.0.)
    PIE.real <-mean(Y01.1.-Y00.0.)
    #PIE.real <- PIE.m.real +PIE.w.real+PIE.mw.real
    NIE.real <- mean(Y11.1.-Y10.0.)
    #NIE.real <- PIE.real+INT.med.real
    # real.value <- data.frame(. = c("CDE","INTrefAM","INTrefAW","INTrefAMW","INTmedM","INTmedW","INTmedMW","PIEM","PIEW","PIEMW"),
    #                          Real = c(CDE.real,INTrefAM.real,INTrefAW.real,INTrefAWM.real,INTmedM.real,INTmedW.real,INTmedMW.real,PIEM.real,PIEW.real,PIEMW.real))
    
    
    
    #数据展示整理
    real <- est <- se <- lower <- upper <-absolute.bias <- relative.bias <- RMSE <- relative.RMSE <-
      setNames(rep(NA, 15), c("NDE","NIE","CDE","INT.ref","INT.re.am",
                              "INT.re.aw","INT.re.amw","INT.med","INT.med.am","INT.med.aw",
                              "INT.med.amw","PIE","PIE.m","PIE.w","PIE.mw"))
    est[] <- c(NDE,NIE,CDE,INT.ref,INT.re.am,
               INT.re.aw,INT.re.amw,INT.med,INT.med.am,INT.med.aw,
               INT.med.amw,PIE,PIE.m,PIE.w,PIE.mw)
    real[] <- c(NDE.real,NIE.real,CDE.real,INT.ref.real,INT.re.am.real,
                INT.re.aw.real,INT.re.amw.real,INT.med.real,INT.med.am.real,INT.med.aw.real,
                INT.med.amw.real,PIE.real,PIE.m.real,PIE.w.real,PIE.mw.real)
    
    # boostrap standard errors
    if (bootstrap == TRUE){
      
      holder <- `colnames<-`(matrix(NA, iterations, 15), c("NDE","NIE","CDE","INT.ref","INT.re.am",
                                                           "INT.re.aw","INT.re.amw","INT.med","INT.med.am","INT.med.aw",
                                                           "INT.med.amw","PIE","PIE.m","PIE.w","PIE.mw"))
      
      
      
      y_form <- formula(object$y_model)
      m_form1 <- formula(object$m_model1)
      m_form2 <- formula(object$m_model2)
      
      ##中介连接函数
      m_family <- object$m_model1$family
      
      z_forms <- lapply(object$zmodels, formula)
      z_families <- lapply(object$zmodels, family)
      
      nz <- length(object$zmodels)
      
      for (k in seq(1, iterations))
      {
        #if(k %% (iterations/10) == 0) cat(".")
        
        progress <- k / iterations * 100  # 计算进度百分比
        cat("\r[", paste(rep("=", progress / 2), collapse = ""), ">",
            paste(rep(" ", 50 - progress / 2), collapse = ""), "]",
            sprintf("%.1f%%", progress))  # 打印进度条和百分比
        flush.console()  # 刷新控制台输出
        Sys.sleep(0.1)
        
        
        
        # resample from the original data
        indices <- sample.int(nrow(object$data), replace = TRUE)
        data <- object$data[indices, , drop = FALSE]
        
        # refit the post-treatment confounder models
        glm_partial <- partial(glm, data = data)
        zmodels <- Map(glm_partial, z_forms, z_families)
        
        # take pre- and post-treatment confounders
        x <- data[, var_names$pre_cov, drop = FALSE]
        z <- data[, var_names$post_cov, drop = FALSE]
        
        # copy data to date_ed
        data_ed <- data
        
        # demean x and residualize z
        for(i in seq_along(x)) data_ed[[names(x)[i]]] <- x[[i]]-mean(x[[i]])
        
        for(i in seq_along(z)) data_ed[[names(z)[i]]] <- z[[i]] - zmodels[[i]][["fitted.values"]]
        
        # mediator and outcome models
        m_model1 <- glm(formula = m_form, family = m1.family , data = data_ed)
        m_model2 <- glm(formula = w_form, family = m1.family , data = data_ed)
        
        
        y_model <- lm(formula = y_form, data = data_ed)
        # outcome model coefs
        coefs_y <- coef(y_model)
        
        beta2 <- coefs_y[var_names$treatment]
        beta4 <- coefs_y[var_names$mediator1]
        beta5 <- coefs_y[var_names$mediator2]
        
        beta6 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator1)] %||%
                    coefs_y[paste0(var_names$mediator1,":",var_names$treatment)]) %||% 0
        beta7 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2)] %||%
                    coefs_y[paste0(var_names$mediator2,":",var_names$treatment)]) %||% 0
        beta8 <- (coefs_y[paste0(var_names$mediator2,":",var_names$mediator1)] %||%
                    coefs_y[paste0(var_names$mediator1,":",var_names$mediator2)]) %||% 0
        beta9 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2,":",var_names$mediator1)] %||%
                    coefs_y[paste0(var_names$treatment,":",var_names$mediator1,":",var_names$mediator2)]) %||% 0
        
        # pred1 and pred0 from the mediator model
        if (object$m_model1$family$link != "identity"){
          
          newdata0 <- newdata1 <- object$data_ed
          newdata0[[var_names$treatment]] <- a0
          newdata1[[var_names$treatment]] <- a1
          
          pred0 <- mean(predict.glm(object$m_model1, newdata0, type = "response"))
          pred1 <- mean(predict.glm(object$m_model1, newdata1, type = "response"))
          
          w00 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=0), type = "response"))
          w0m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred0), type = "response"))
          w0m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred1), type = "response"))
          
          w10 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=0), type = "response"))
          w1m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred0), type = "response"))
          w1m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred1), type = "response"))
          
        } else {
          
          coefs_m1 <- coef(object$m_model1)
          coefs_m2 <- coef(object$m_model2)
          
          theta0 <- coefs_m1["(Intercept)"]
          theta2 <- coefs_m1[var_names$treatment]
          
          epsilon0 <- coefs_m2["(Intercept)"]
          epsilon2 <- coefs_m2[var_names$treatment]
          epsilon3 <- coefs_m2[var_names$mediator1]
          
          pred0 <- theta0 + theta2 * a0
          pred1 <- theta0 + theta2 * a1
          
          w00 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * 0
          w0m0 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred0
          w0m1 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred1
          
          w10 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * 0
          w1m0 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred0
          w1m1 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred1
        }
        
        
        # effect decomposition
        holder[k,"NDE"] <-  (((a1-a0)*(beta2+beta6*(pred0)+beta7*w0m0+beta9*pred0*w0m0)))
        
        holder[k,"CDE"] <- ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
        
        holder[k,"INT.re.am"] <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
        holder[k,"INT.re.aw"] <- ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
        holder[k,"INT.re.amw"] <- (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))-
          ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-
          ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))+
          ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
        holder[k,"INT.ref"] <- holder[k,"INT.re.am"]+holder[k,"INT.re.aw"]+holder[k,"INT.re.amw"]
        #CDE+INT.re.am+INT.re.aw+INT.re.mw
        
        
        holder[k,"NIE"] <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*(pred1-pred0)*a1+beta7*a1*(w1m1-w0m0)+
                              beta8*(pred1*w1m1-pred0*w0m0)+beta9*a1*(pred1*w1m1-pred0*w0m0))
        
        holder[k,"INT.med.am"] <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
        holder[k,"INT.med.aw"] <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
        holder[k,"INT.med.amw"] <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w1m1+beta9*pred1*w1m1))-
          ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-
          ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))+
          (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
        
        holder[k,"INT.med"] <-  holder[k,"INT.med.am"]+holder[k,"INT.med.aw"]+holder[k,"INT.med.amw"]
        
        
        holder[k,"PIE.m"] <- (pred1-pred0)*(beta4+beta6*(a0)+beta8*w0m1+beta9*w0m1*a0)
        holder[k,"PIE.w"] <- (w1m0-w0m0)*(beta5+beta7*(a0)+beta8*pred0+beta9*pred0*a0)
        holder[k,"PIE.mw"] <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*a0*(pred1-pred0)+beta7*a0*(w1m1-w0m0)+
                                 beta8*(pred1*w1m1-pred0*w0m0)+beta9*a0*(pred1*w1m1-pred0*w0m0))-holder[k,"PIE.m"]-holder[k,"PIE.w"]
        
        holder[k,"PIE"] <- holder[k,"PIE.m"]+holder[k,"PIE.w"]+holder[k,"PIE.mw"]
      }
      
      
      
      se <- apply(holder, 2, sd)
      lower <- apply(holder, 2, quantile, 0.025)
      upper <- apply(holder, 2, quantile, 0.975)
      
      absolute.bias <- sapply(1:ncol(holder), function(i) {
        col_mean <- mean(abs(holder[, i] - real[i]), na.rm = TRUE)
        return(col_mean)
      })
      
      
      relative.bias <- absolute.bias/real
      
      RMSE <- sapply(1:ncol(holder), function(i) {
        col_rmse <- sqrt(mean((holder[, i] - real[i])^2, na.rm = TRUE))
        return(col_rmse)
        
      })
      
      
      relative.RMSE <- RMSE/RMSE
    }
    
    fullmat <- cbind(real,est, se, lower, upper,absolute.bias,relative.bias,RMSE,relative.RMSE)
    colnames(fullmat) <- c("real","Estimate", "SE", "2.5% Perc", "97.5% Perc",
                           "absolute.bias" ,"relative.bias","RMSE","relative.RMSE")
    
    fullmat <- as.data.frame(fullmat)
    fullmat$effect.name <- rownames(fullmat)
    fullmat <- cbind(fullmat[, "effect.name", drop = FALSE], fullmat[, -ncol(fullmat)])
  }
  
  decomp(object)
  
  
  
}

sim2 <- decompz(treatment, pre_cov, zmodels, y_form, m_form,
                m1.family = m1.family,
                m2.family = m2.family,
                l.family =  l.family ,
                y.family =  y.family ,
                bootstrap = TRUE, iterations = 50,
                data=data)
print(sim2)

write.table(sim2,paste0(path_data,"sim2.csv"),sep = ",",col.names = TRUE,row.names = FALSE)


# over  ####
print("正确模型")




















# 5 模型指定及不同参数设置结果####
# *5.1 alpha####
#生成数据
set.seed(1729) 

expit<-function(x) exp(x)/(1+exp(x)) 

# n<-10000

#alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
#gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
#lambda s a parameter that controls the degree to which C modifies the effect of L on Y
#eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic

alpha1 <- c(seq(0, 0.5, by = 0.05))
gamma <- 0
lambda <- 0
eta <- 0


alpha_outcome <- list()
for (g in 1:length(alpha1)) {
  #alpha <- c(seq(0, 0.5, by = 0.05))
  
  alpha <- alpha1[g]
  
  gamma <- 0
  lambda <- 0
  eta <- 0
  
  
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L<-rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M<-rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W<-rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y<-rnorm(n,mean=(0.122+(0.95+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)
  data <-as.data.frame(cbind(A=A,Y=Y,C=C,M=M,W=W,L=L))
  
  #抽象函数
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L.fun <- function(C,A)rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M.fun <- function(A,L,C)rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W.fun <- function(A,L,C,M)rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y.fun <- function(A,L,C,M,W)rnorm(n,mean=(0.122+(0.55+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.1*A*M*W),sd=0.5)
  
  #alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
  #gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
  #lambda s a parameter that controls the degree to which C modifies the effect of L on Y
  #eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic
  #混杂暴露指定
  pre_cov <- c("C") 
  treatment <- c("A")
  
  #模型指定
  m_form <- M ~ C * A
  w_form <- W ~ C * A + M
  y_form <- Y ~ C*A+M+W+A*W+A*M+L+M*W+A*M*W
  
  
  #L的模型
  m1 <- glm(L~C*A,data=data,family = l.family)
  zmodels <- list(m1)
  
  
  decompz1 <- function(treatment, pre_cov, zmodels, y_form, m_form,
                       m1.family = m1.family,
                       m2.family = m2.family,
                       l.family =  l.family,
                       y.family =  y.family,
                       bootstrap = TRUE, iterations = 1000,
                       data){
    
    #获取中介变量、结果变量、L
    mediator1 <- all.vars(m_form)[1L]
    mediator2 <- all.vars(w_form)[1L]
    outcome <- all.vars(y_form)[1L]
    post_cov <- vapply(zmodels, function(x) names(x[["model"]])[1], character(1L))
    
    
    a <- data[[treatment]]
    m <- data[[mediator1]]
    w <- data[[mediator2]]
    y <- data[[outcome]]
    x <- data[, pre_cov, drop = FALSE]
    z <- data[, post_cov, drop = FALSE]
    
    #复制数据
    data_ed <- data
    
    #获取C和L的残差
    for(i in seq_along(x)) data_ed[[names(x)[i]]] <- x[[i]]-mean(x[[i]])
    for(i in seq_along(z)) data_ed[[names(z)[i]]] <- z[[i]] - zmodels[[i]][["fitted.values"]]
    
    #重新拟合模型
    m_model1 <- glm(formula = m_form, family = m1.family, data = data_ed)
    m_model2 <- glm(formula = w_form, family = m2.family, data = data_ed)
    y_model <- glm(formula = y_form,family = gaussian(link = "identity"), data = data_ed)
    
    #整理
    object <- list(y_model = y_model, m_model1 = m_model1, m_model2 = m_model2 ,zmodels = zmodels,
                   var_names = list(treatment = treatment,
                                    mediator1 = mediator1,
                                    mediator2 = mediator2,
                                    outcome = outcome,
                                    pre_cov = pre_cov,
                                    post_cov = post_cov),
                   data = data, data_ed = data_ed)
    
    
    #object <- out
    decomp <- function(object){
      
      #定义a1,a0,m*
      a0 = 0
      a1 = 1
      m = 0
      
      #选择系数函数
      `%||%` <- function(a, b) if (!is.na(a)) a else b
      
      var_names <- object$var_names
      
      coefs_y <- coef(object$y_model)
      
      beta2 <- coefs_y[var_names$treatment]
      beta4 <- coefs_y[var_names$mediator1]
      beta5 <- coefs_y[var_names$mediator2]
      
      beta6 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator1)] %||%
                  coefs_y[paste0(var_names$mediator1,":",var_names$treatment)]) %||% 0
      beta7 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2)] %||%
                  coefs_y[paste0(var_names$mediator2,":",var_names$treatment)]) %||% 0
      beta8 <- (coefs_y[paste0(var_names$mediator2,":",var_names$mediator1)] %||%
                  coefs_y[paste0(var_names$mediator1,":",var_names$mediator2)]) %||% 0
      beta9 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2,":",var_names$mediator1)] %||%
                  coefs_y[paste0(var_names$treatment,":",var_names$mediator1,":",var_names$mediator2)]) %||% 0
      
      # pred1 and pred0 from the mediator model
      if (object$m_model1$family$link != "identity"){
        
        newdata0 <- newdata1 <- object$data_ed
        newdata0[[var_names$treatment]] <- a0
        newdata1[[var_names$treatment]] <- a1
        
        pred0 <- mean(predict.glm(object$m_model1, newdata0, type = "response"))
        pred1 <- mean(predict.glm(object$m_model1, newdata1, type = "response"))
        
        w00 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=0), type = "response"))
        w0m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred0), type = "response"))
        w0m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred1), type = "response"))
        
        w10 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=0), type = "response"))
        w1m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred0), type = "response"))
        w1m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred1), type = "response"))
        
      } else {
        
        coefs_m1 <- coef(object$m_model1)
        coefs_m2 <- coef(object$m_model2)
        
        theta0 <- coefs_m1["(Intercept)"]
        theta2 <- coefs_m1[var_names$treatment]
        
        epsilon0 <- coefs_m2["(Intercept)"]
        epsilon2 <- coefs_m2[var_names$treatment]
        epsilon3 <- coefs_m2[var_names$mediator1]
        
        pred0 <- theta0 + theta2 * a0
        pred1 <- theta0 + theta2 * a1
        
        w00 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * 0
        w0m0 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred0
        w0m1 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred1
        
        w10 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * 0
        w1m0 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred0
        w1m1 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred1
      }
      
      #效应参数估计
      NDE <-  (((a1-a0)*(beta2+beta6*(pred0)+beta7*w0m0+beta9*pred0*w0m0)))
      
      CDE <- ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
      
      INT.re.am <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
      INT.re.aw <- ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
      INT.re.amw <- (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))-
        ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-
        ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))+
        ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
      INT.ref <- INT.re.am+INT.re.aw+INT.re.amw
      #CDE+INT.ref
      
      
      NIE <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*(pred1-pred0)*a1+beta7*a1*(w1m1-w0m0)+
                beta8*(pred1*w1m1-pred0*w0m0)+beta9*a1*(pred1*w1m1-pred0*w0m0))
      
      INT.med.am <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
      
      INT.med.aw <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
      
      INT.med.amw <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w1m1+beta9*pred1*w1m1))-
        ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-
        ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))+
        (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
      
      INT.med <- INT.med.am+INT.med.aw+INT.med.amw
      
      
      PIE.m <- (pred1-pred0)*(beta4+beta6*(a0)+beta8*w0m1+beta9*w0m1*a0)
      PIE.w <- (w1m0-w0m0)*(beta5+beta7*(a0)+beta8*pred0+beta9*pred0*a0)
      PIE.mw <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*a0*(pred1-pred0)+beta7*a0*(w1m1-w0m0)+
                   beta8*(pred1*w1m1-pred0*w0m0)+beta9*a0*(pred1*w1m1-pred0*w0m0))-PIE.m-PIE.w
      
      
      PIE <- PIE.m+PIE.w+PIE.mw
      
      PIE+INT.med
      
      
      
      #计算真值
      ##  Y(a,m*,w*) Y(a*,m*,w*) 
      Y100 <- Y.fun(1,L.fun(C,1),C,0,0)
      Y000 <- Y.fun(0,L.fun(C,0),C,0,0)
      ##  Y(a,M(a*),w*) Y(a*,M(a*),w*)
      Y10.0 <- Y.fun(1,L.fun(C,1),C,pred0,0)
      Y00.0 <- Y.fun(0,L.fun(C,0),C,pred0,0)
      ##  Y(a,m*,W(a*)) Y(a*,m*,W(a*))
      Y100. <- Y.fun(1,L.fun(C,1),C,0,w00)
      Y000. <- Y.fun(0,L.fun(C,0),C,0,w00)
      ##  Y(a,M(a*),W(a*)) Y(a*,M(a*),W(a*))
      Y10.0. <- Y.fun(1,L.fun(C,1),C,pred0,w0m0)
      Y00.0. <- Y.fun(0,L.fun(C,0),C,pred0,w0m0)
      ##  Y(a,M(a),W(a*))  Y(a*,M(a),W(a*))
      Y11.0. <- Y.fun(1,L.fun(C,1),C,pred1,w0m1)
      Y01.0. <- Y.fun(0,L.fun(C,0),C,pred1,w0m1)
      ##  Y(a,M(a*),W(a))  Y(a*,M(a*),W(a))
      Y10.1. <- Y.fun(1,L.fun(C,1),C,pred0,w1m0)
      Y00.1. <- Y.fun(0,L.fun(C,0),C,pred0,w1m0)
      ##  Y(a,M(a),W(a))   Y(a*,M(a),W(a))
      Y11.1. <- Y.fun(1,L.fun(C,1),C,pred1,w1m1)
      Y01.1. <- Y.fun(0,L.fun(C,0),C,pred1,w1m1)
      
      CDE.real <-mean(Y100-Y000)
      INT.re.am.real <- mean((Y10.0 - Y00.0)) - mean((Y100 - Y000))
      INT.re.aw.real <- mean((Y100. - Y000.)) -mean((Y100 - Y000))
      INT.re.amw.real <- mean(Y10.0. - Y00.0.)-mean(Y10.0 - Y00.0)-mean(Y100. - Y000.)+mean(Y100 - Y000)
      
      INT.ref.real <- mean(Y10.0.-Y00.0.)-mean(Y100-Y000)
      NDE.real <-mean(Y10.0.-Y00.0.)
      #INT.ref.real <- INT.re.am.real+INT.re.aw.real+INT.re.amw.real
      #NDE.real <- CDE.real+INT.re.am.real+INT.re.aw.real+INT.re.amw.real
      
      
      INT.med.am.real <- mean((Y11.0. - Y01.0.)-(Y10.0. - Y00.0.))
      INT.med.aw.real <- mean((Y10.1. - Y00.1.)-(Y10.0. - Y00.0.))
      INT.med.amw.real <- mean((Y11.1. - Y01.1.)-(Y11.0. - Y01.0.)-(Y10.1. - Y00.1.)+(Y10.0. - Y00.0.))
      
      INT.med.real <-mean(Y11.1.-Y01.1.)-mean(Y10.0.-Y00.0.)
      
      #INT.med.real <- INT.med.am.real+INT.med.aw.real+INT.med.amw.real
      
      
      PIE.m.real <- mean(Y01.0. - Y00.0.)
      PIE.w.real <- mean(Y00.1. - Y00.0.)
      PIE.mw.real <- mean(Y01.1. - Y01.0. - Y00.1. + Y00.0.)
      PIE.real <-mean(Y01.1.-Y00.0.)
      #PIE.real <- PIE.m.real +PIE.w.real+PIE.mw.real
      NIE.real <- mean(Y11.1.-Y10.0.)
      #NIE.real <- PIE.real+INT.med.real
      # real.value <- data.frame(. = c("CDE","INTrefAM","INTrefAW","INTrefAMW","INTmedM","INTmedW","INTmedMW","PIEM","PIEW","PIEMW"),
      #                          Real = c(CDE.real,INTrefAM.real,INTrefAW.real,INTrefAWM.real,INTmedM.real,INTmedW.real,INTmedMW.real,PIEM.real,PIEW.real,PIEMW.real))
      
      
      
      #数据展示整理
      real <- est <- se <- lower <- upper <-absolute.bias <- relative.bias <- RMSE <- relative.RMSE <- 
        setNames(rep(NA, 15), c("NDE","NIE","CDE","INT.ref","INT.re.am",
                                "INT.re.aw","INT.re.amw","INT.med","INT.med.am","INT.med.aw",
                                "INT.med.amw","PIE","PIE.m","PIE.w","PIE.mw"))
      est[] <- c(NDE,NIE,CDE,INT.ref,INT.re.am,
                 INT.re.aw,INT.re.amw,INT.med,INT.med.am,INT.med.aw,
                 INT.med.amw,PIE,PIE.m,PIE.w,PIE.mw)
      # real[] <- c(NDE.real,NIE.real,CDE.real,INT.ref.real,INT.re.am.real,
      #             INT.re.aw.real,INT.re.amw.real,INT.med.real,INT.med.am.real,INT.med.aw.real,
      #             INT.med.amw.real,PIE.real,PIE.m.real,PIE.w.real,PIE.mw.real)
      real[] <- c(sim2$real)
      
      # boostrap standard errors
      if (bootstrap == TRUE){
        
        holder <- `colnames<-`(matrix(NA, iterations, 15), c("NDE","NIE","CDE","INT.ref","INT.re.am",
                                                             "INT.re.aw","INT.re.amw","INT.med","INT.med.am","INT.med.aw",
                                                             "INT.med.amw","PIE","PIE.m","PIE.w","PIE.mw"))
        
        
        
        y_form <- formula(object$y_model)
        m_form1 <- formula(object$m_model1)
        m_form2 <- formula(object$m_model2)
        
        ##中介连接函数
        m_family <- object$m_model1$family
        
        z_forms <- lapply(object$zmodels, formula)
        z_families <- lapply(object$zmodels, family)
        
        nz <- length(object$zmodels)
        
        for (k in seq(1, iterations))
        {
          #if(k %% (iterations/10) == 0) cat(".")
          
          progress <- k / iterations * 100  # 计算进度百分比
          cat("\r[", paste(rep("=", progress / 2), collapse = ""), ">",
              paste(rep(" ", 50 - progress / 2), collapse = ""), "]",
              sprintf("%.1f%%", progress))  # 打印进度条和百分比
          flush.console()  # 刷新控制台输出
          Sys.sleep(0.1) 
          
          
          
          # resample from the original data
          indices <- sample.int(nrow(object$data), replace = TRUE)
          data <- object$data[indices, , drop = FALSE]
          
          # refit the post-treatment confounder models
          glm_partial <- partial(glm, data = data)
          zmodels <- Map(glm_partial, z_forms, z_families)
          
          # take pre- and post-treatment confounders
          x <- data[, var_names$pre_cov, drop = FALSE]
          z <- data[, var_names$post_cov, drop = FALSE]
          
          # copy data to date_ed
          data_ed <- data
          
          # demean x and residualize z
          for(i in seq_along(x)) data_ed[[names(x)[i]]] <- x[[i]]-mean(x[[i]])
          
          for(i in seq_along(z)) data_ed[[names(z)[i]]] <- z[[i]] - zmodels[[i]][["fitted.values"]]
          
          # mediator and outcome models
          m_model1 <- glm(formula = m_form, family = m1.family , data = data_ed)
          m_model2 <- glm(formula = w_form, family = m1.family , data = data_ed)
          
          
          y_model <- lm(formula = y_form, data = data_ed)
          # outcome model coefs
          coefs_y <- coef(y_model)
          
          beta2 <- coefs_y[var_names$treatment]
          beta4 <- coefs_y[var_names$mediator1]
          beta5 <- coefs_y[var_names$mediator2]
          
          beta6 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator1)] %||%
                      coefs_y[paste0(var_names$mediator1,":",var_names$treatment)]) %||% 0
          beta7 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2)] %||%
                      coefs_y[paste0(var_names$mediator2,":",var_names$treatment)]) %||% 0
          beta8 <- (coefs_y[paste0(var_names$mediator2,":",var_names$mediator1)] %||%
                      coefs_y[paste0(var_names$mediator1,":",var_names$mediator2)]) %||% 0
          beta9 <- (coefs_y[paste0(var_names$treatment,":",var_names$mediator2,":",var_names$mediator1)] %||%
                      coefs_y[paste0(var_names$treatment,":",var_names$mediator1,":",var_names$mediator2)]) %||% 0
          
          # pred1 and pred0 from the mediator model
          if (object$m_model1$family$link != "identity"){
            
            newdata0 <- newdata1 <- object$data_ed
            newdata0[[var_names$treatment]] <- a0
            newdata1[[var_names$treatment]] <- a1
            
            pred0 <- mean(predict.glm(object$m_model1, newdata0, type = "response"))
            pred1 <- mean(predict.glm(object$m_model1, newdata1, type = "response"))
            
            w00 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=0), type = "response"))
            w0m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred0), type = "response"))
            w0m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata0[["C"]],A=newdata0[["A"]],M=pred1), type = "response"))
            
            w10 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=0), type = "response"))
            w1m0 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred0), type = "response"))
            w1m1 <- mean(predict.glm(object$m_model2, data.frame(C=newdata1[["C"]],A=newdata1[["A"]],M=pred1), type = "response"))
            
          } else {
            
            coefs_m1 <- coef(object$m_model1)
            coefs_m2 <- coef(object$m_model2)
            
            theta0 <- coefs_m1["(Intercept)"]
            theta2 <- coefs_m1[var_names$treatment]
            
            epsilon0 <- coefs_m2["(Intercept)"]
            epsilon2 <- coefs_m2[var_names$treatment]
            epsilon3 <- coefs_m2[var_names$mediator1]
            
            pred0 <- theta0 + theta2 * a0
            pred1 <- theta0 + theta2 * a1
            
            w00 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * 0
            w0m0 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred0
            w0m1 <- epsilon0 +  epsilon2 * a0 +  epsilon3 * pred1
            
            w10 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * 0
            w1m0 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred0
            w1m1 <- epsilon0 +  epsilon2 * a1 +  epsilon3 * pred1
          }
          
          
          # effect decomposition
          holder[k,"NDE"] <-  (((a1-a0)*(beta2+beta6*(pred0)+beta7*w0m0+beta9*pred0*w0m0)))
          
          holder[k,"CDE"] <- ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
          
          holder[k,"INT.re.am"] <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
          holder[k,"INT.re.aw"] <- ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))-((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
          holder[k,"INT.re.amw"] <- (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))-
            ((a1-a0)*(beta2+beta6*(pred0)+beta7*0+beta9*pred0*0))-
            ((a1-a0)*(beta2+beta6*0+beta7*w00+beta9*0*w00))+
            ((a1-a0)*(beta2+beta6*0+beta7*0+beta9*0*0))
          holder[k,"INT.ref"] <- holder[k,"INT.re.am"]+holder[k,"INT.re.aw"]+holder[k,"INT.re.amw"]
          #CDE+INT.re.am+INT.re.aw+INT.re.mw
          
          
          holder[k,"NIE"] <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*(pred1-pred0)*a1+beta7*a1*(w1m1-w0m0)+
                                beta8*(pred1*w1m1-pred0*w0m0)+beta9*a1*(pred1*w1m1-pred0*w0m0))
          
          holder[k,"INT.med.am"] <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
          holder[k,"INT.med.aw"] <- ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))-(((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
          holder[k,"INT.med.amw"] <- ((a1-a0)*(beta2+beta6*(pred1)+beta7*w1m1+beta9*pred1*w1m1))-
            ((a1-a0)*(beta2+beta6*(pred1)+beta7*w0m1+beta9*pred1*w0m1))-
            ((a1-a0)*(beta2+beta6*(pred0)+beta7*w1m0+beta9*pred0*w1m0))+
            (((a1-a0)*(beta2+beta6*(pred0)+beta7*(w0m0)+beta9*pred0*w0m0)))
          
          holder[k,"INT.med"] <-  holder[k,"INT.med.am"]+holder[k,"INT.med.aw"]+holder[k,"INT.med.amw"]
          
          
          holder[k,"PIE.m"] <- (pred1-pred0)*(beta4+beta6*(a0)+beta8*w0m1+beta9*w0m1*a0)
          holder[k,"PIE.w"] <- (w1m0-w0m0)*(beta5+beta7*(a0)+beta8*pred0+beta9*pred0*a0)
          holder[k,"PIE.mw"] <- (beta4*(pred1-pred0)+beta5*(w1m1-w0m0)+beta6*a0*(pred1-pred0)+beta7*a0*(w1m1-w0m0)+
                                   beta8*(pred1*w1m1-pred0*w0m0)+beta9*a0*(pred1*w1m1-pred0*w0m0))-holder[k,"PIE.m"]-holder[k,"PIE.w"]
          
          holder[k,"PIE"] <- holder[k,"PIE.m"]+holder[k,"PIE.w"]+holder[k,"PIE.mw"]
        }
        
        
        
        se <- apply(holder, 2, sd)
        lower <- apply(holder, 2, quantile, 0.025)
        upper <- apply(holder, 2, quantile, 0.975)
        
        absolute.bias <- sapply(1:ncol(holder), function(i) {
          col_mean <- mean(abs(holder[, i] - real[i]), na.rm = TRUE)
          return(col_mean)
        })
        
        
        relative.bias <- absolute.bias/real
        
        RMSE <- sapply(1:ncol(holder), function(i) {
          col_rmse <- sqrt(mean((holder[, i] - real[i])^2, na.rm = TRUE))
          return(col_rmse)
        })
        
        
        relative.RMSE <- RMSE/sim2$RMSE
      }
      
      fullmat <- cbind(real,est, se, lower, upper,absolute.bias,relative.bias,RMSE,relative.RMSE)
      colnames(fullmat) <- c("real","Estimate", "SE", "2.5% Perc", "97.5% Perc",
                             "absolute.bias" ,"relative.bias","RMSE","relative.RMSE")
      
      fullmat <- as.data.frame(fullmat)
      fullmat$effect.name <- rownames(fullmat)
      fullmat <- cbind(fullmat[, "effect.name", drop = FALSE], fullmat[, -ncol(fullmat)])
    }
    
    decomp(object)
    
    
    
  }
  
  alpha_outcome[[g]] <- decompz1(treatment, pre_cov, zmodels, y_form, m_form,
                                 m1.family = m1.family,
                                 m2.family = m2.family,
                                 l.family =  l.family,
                                 y.family =  y.family,
                                 bootstrap = TRUE, iterations = 1000,
                                 data=data)
  write.table(alpha_outcome[[g]],paste0(path_data,"alpha_outcome2_",alpha1[g],".csv"),sep = ",",col.names = TRUE,row.names = FALSE)
  
}

#over####

print("alpha")


















# *5.2 gamma####
#生成数据
set.seed(1729) 

expit<-function(x) exp(x)/(1+exp(x)) 

#n<-1000

#alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
#gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
#lambda s a parameter that controls the degree to which C modifies the effect of L on Y
#eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic

alpha <- 0
gamma1 <-c(seq(0, 0.5, by = 0.05))
lambda <- 0
eta <- 0


gamma_outcome <- list()
for (g in 1:length(gamma1)) {
  #alpha <- c(seq(0, 0.5, by = 0.05))
  
  alpha <- 0
  gamma <- gamma1[g]
  lambda <- 0
  eta <- 0
  
  
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L<-rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M<-rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W<-rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y<-rnorm(n,mean=(0.122+(0.95+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)
  data <-as.data.frame(cbind(A=A,Y=Y,C=C,M=M,W=W,L=L))
  
  #抽象函数
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L.fun <- function(C,A)rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M.fun <- function(A,L,C)rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W.fun <- function(A,L,C,M)rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y.fun <- function(A,L,C,M,W)rnorm(n,mean=(0.122+(0.55+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.1*A*M*W),sd=0.5)
  
  #alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
  #gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
  #lambda s a parameter that controls the degree to which C modifies the effect of L on Y
  #eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic
  
  #混杂暴露指定
  pre_cov <- c("C") 
  treatment <- c("A")
  
  #模型指定
  m_form <- M ~ C * A
  w_form <- W ~ C * A + M
  y_form <- Y ~ C*A+M+W+A*W+A*M+L+M*W+A*M*W
  
  
  
  #L的模型
  m1 <- glm(L~C*A,data=data,family = l.family)
  zmodels <- list(m1)
  
  gamma_outcome[[g]] <- decompz1(treatment, pre_cov, zmodels, y_form, m_form,
                                 m1.family = m1.family,
                                 m2.family = m2.family,
                                 l.family = l.family,
                                 y.family = y.family,
                                 bootstrap = TRUE, iterations = 1000,
                                 data=data)
  write.table(gamma_outcome[[g]],paste0(path_data,"gamma_outcome2_",gamma1[g],".csv"),sep = ",",col.names = TRUE,row.names = FALSE)
  
}
#over####

print("gamma")

# *5.3 lambda####
#生成数据
set.seed(1729) 

expit<-function(x) exp(x)/(1+exp(x)) 

# n<-100000

#alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
#gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
#lambda s a parameter that controls the degree to which C modifies the effect of L on Y
#eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic

alpha <- 0
gamma <-0
lambda1 <- c(seq(0, 0.5, by = 0.05))
eta <- 0


lambda_outcome <- list()
for (g in 1:length(lambda1)) {
  #alpha <- c(seq(0, 0.5, by = 0.05))
  
  alpha <- 0
  gamma <- 0
  lambda <- lambda1[g]
  eta <- 0
  
  
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L<-rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M<-rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W<-rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y<-rnorm(n,mean=(0.122+(0.95+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)
  data <-as.data.frame(cbind(A=A,Y=Y,C=C,M=M,W=W,L=L))
  
  #抽象函数
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L.fun <- function(C,A)rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M.fun <- function(A,L,C)rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W.fun <- function(A,L,C,M)rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y.fun <- function(A,L,C,M,W)rnorm(n,mean=(0.122+(0.55+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.1*A*M*W),sd=0.5)
  
  #alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
  #gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
  #lambda s a parameter that controls the degree to which C modifies the effect of L on Y
  #eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic
  
  #混杂暴露指定
  pre_cov <- c("C") 
  treatment <- c("A")
  
  #模型指定
  m_form <- M ~ C * A
  w_form <- W ~ C * A + M
  y_form <- Y ~ C*A+M+W+A*W+A*M+L+M*W+A*M*W
  
  
  
  #L的模型
  m1 <- glm(L~C*A,data=data,family = l.family)
  zmodels <- list(m1)
  
  
  lambda_outcome[[g]] <- decompz1(treatment, pre_cov, zmodels, y_form, m_form,
                                  m1.family = m1.family,
                                  m2.family = m2.family,
                                  l.family = l.family,
                                  y.family = y.family,
                                  bootstrap = TRUE, iterations = 1000,
                                  data=data)
  write.table(lambda_outcome[[g]],paste0(path_data,"lambda_outcome2_",lambda1[g],".csv"),sep = ",",col.names = TRUE,row.names = FALSE)
  
}
#over####
print("lambda")

# *5.4 eta####
#生成数据
set.seed(1729) 

expit<-function(x) exp(x)/(1+exp(x)) 

# n<-1000

#alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
#gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
#lambda s a parameter that controls the degree to which C modifies the effect of L on Y
#eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic

alpha <- 0
gamma <-0
lambda1 <- 0
eta1 <- c(seq(0, 0.5, by = 0.05))


eta_outcome <- list()
for (g in 1:length(eta1)) {
  #alpha <- c(seq(0, 0.5, by = 0.05))
  
  alpha <- 0
  gamma <- 0
  lambda <- 0
  eta <- eta1[g]
  
  
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L<-rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M<-rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W<-rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y<-rnorm(n,mean=(0.122+(0.95+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.5*A*M*W),sd=0.5)
  data <-as.data.frame(cbind(A=A,Y=Y,C=C,M=M,W=W,L=L))
  
  #抽象函数
  C <- rnorm(n) 
  A<-rbinom(n,1,prob=0.5)
  L.fun <- function(C,A)rnorm(n,mean=(-1.89+(0.37-eta*C)*C+(alpha*C+1.3)*A),sd=0.5) 
  M.fun <- function(A,L,C)rbinom(n,size=1,expit(x=-1.26+0.73*L+(0.95+alpha*C)*A+(0.26-eta*C)*C))
  W.fun <- function(A,L,C,M)rbinom(n,size=1,expit(x=0.19+(0.67-eta*C)*C+0.95*L+(0.95+alpha*C)*A+0.55*M)) 
  Y.fun <- function(A,L,C,M,W)rnorm(n,mean=(0.122+(0.55+alpha*C)*A+(0.65+lambda*C)*L+(0.5-eta*C)*C+(0.75+gamma*C)*W+(0.85+gamma*C)*M+0.2*A*M+0.3*A*W+0.1*M*W+0.1*A*M*W),sd=0.5)
  
  #alpha is a parameter that controls the degree to which C modifies the effects of A on L, M,W and Y
  #gamma is a parameter that controls the degree to which C modifies the effect of M W on Y
  #lambda s a parameter that controls the degree to which C modifies the effect of L on Y
  #eta is a parameter that controls whether the effects of C on L, M,W and Y are linear versus parabolic
  
  #混杂暴露指定
  pre_cov <- c("C") 
  treatment <- c("A")
  
  #模型指定
  m_form <- M ~ C * A
  w_form <- W ~ C * A + M
  y_form <- Y ~ C*A+M+W+A*W+A*M+L+M*W+A*M*W
  
  
  
  #L的模型
  m1 <- glm(L~C*A,data=data,family = l.family)
  zmodels <- list(m1)
  
  
  
  eta_outcome[[g]] <- decompz1(treatment, pre_cov, zmodels, y_form, m_form,
                               m1.family = m1.family,
                               m2.family = m2.family,
                               l.family = l.family,
                               y.family = y.family,
                               bootstrap = TRUE, iterations = 1000,
                               data=data)
  write.table(eta_outcome[[g]],paste0(path_data,"eta_outcome2_",eta1[g],".csv"),sep = ",",col.names = TRUE,row.names = FALSE)
  
}
#over####

print("eta")  

# 6结果整理####
zhishi <-  c(seq(0, 0.5, by = 0.05))
for (j in 2:11) {
  names(alpha_outcome[[j]])[7:10] <- c(paste("alpha",zhishi[j],"_", c("absolute.bias","relative.bias","RMSE","relative.RMSE"), sep = ""))
}

for (j in 2:11) {
  names(gamma_outcome[[j]])[7:10] <- paste("gamma",zhishi[j],"_", c("absolute.bias","relative.bias","RMSE","relative.RMSE"), sep = "")
}

for (j in 2:11) {
  names(lambda_outcome[[j]])[7:10] <- paste("lambda",zhishi[j],"_", c("absolute.bias","relative.bias","RMSE","relative.RMSE"), sep = "")
}

for (j in 2:11) {
  names(eta_outcome[[j]])[7:10] <- paste("eta1",zhishi[j],"_", c("absolute.bias","relative.bias","RMSE","relative.RMSE"), sep = "")
}




alpha <- cbind(alpha_outcome[[2]][,7:10],
               alpha_outcome[[3]][,7:10],
               alpha_outcome[[4]][,7:10],
               alpha_outcome[[5]][,7:10],
               alpha_outcome[[6]][,7:10],
               alpha_outcome[[7]][,7:10],
               alpha_outcome[[8]][,7:10],
               alpha_outcome[[9]][,7:10],
               alpha_outcome[[10]][,7:10],
               alpha_outcome[[11]][,7:10])


gamma <- cbind(gamma_outcome[[2]][,7:10],
               gamma_outcome[[3]][,7:10],
               gamma_outcome[[4]][,7:10],
               gamma_outcome[[5]][,7:10],
               gamma_outcome[[6]][,7:10],
               gamma_outcome[[7]][,7:10],
               gamma_outcome[[8]][,7:10],
               gamma_outcome[[9]][,7:10],
               gamma_outcome[[10]][,7:10],
               gamma_outcome[[11]][,7:10])

lambda <- cbind(lambda_outcome[[2]][,7:10],
                lambda_outcome[[3]][,7:10],
                lambda_outcome[[4]][,7:10],
                lambda_outcome[[5]][,7:10],
                lambda_outcome[[6]][,7:10],
                lambda_outcome[[7]][,7:10],
                lambda_outcome[[8]][,7:10],
                lambda_outcome[[9]][,7:10],
                lambda_outcome[[10]][,7:10],
                lambda_outcome[[11]][,7:10])

eta <- cbind(eta_outcome[[2]][,7:10],
             eta_outcome[[3]][,7:10],
             eta_outcome[[4]][,7:10],
             eta_outcome[[5]][,7:10],
             eta_outcome[[6]][,7:10],
             eta_outcome[[7]][,7:10],
             eta_outcome[[8]][,7:10],
             eta_outcome[[9]][,7:10],
             eta_outcome[[10]][,7:10],
             eta_outcome[[11]][,7:10])


final <- cbind(sim2,alpha,gamma,lambda,eta)

write.table(final,paste0(path_data,"final2.csv"),sep = ",",col.names = TRUE,row.names = FALSE)









