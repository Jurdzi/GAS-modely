# Inverzia semidefinetnej matice
# Lubovolna mocnina matice 
# Efektivne a rychle pocitanie pretoze mozeme pouzit QAQ^t rozklad

ginv_2 <- function (X, tol = sqrt(.Machine$double.eps), h = -1/2) {
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- eigen(X, symmetric = TRUE)
    Positive <- Xsvd$values > max(tol * Xsvd$values[1L], 0)
    if (all(Positive)) 
        Xsvd$vectors %*% (Xsvd$values^(h) * t(Xsvd$vectors))
    else Xsvd$vectors[, Positive, drop = FALSE] %*% (Xsvd$values[Positive]^(h) *
        t(Xsvd$vectors[, Positive, drop = FALSE]))
}

