mMeta.aMeta <-
function(est, std.err, tau0=FALSE, n.perm=5000, seed=NULL) {
	if(!identical(as.numeric(is.na(est)),as.numeric(is.na(std.err)))) stop('The locations of NAs should be matched between est and std.err')
	if(nrow(est) < 10) stop('The number of studies should be greater than or equal to 10')
	ind.all.na <- which(apply(est, 2, function(x) sum(is.na(x))) == nrow(est))
	est[,-ind.all.na] <- est
	std.err[,-ind.all.na] <- std.err
	if (!is.null(seed)) {
		set.seed(seed)
	}
	num.stu <- nrow(est)
	num.mar <- ncol(est)
	est.list <- list()
	var.list <- list()
	for (j in 1:num.mar) {
		est.list[[j]] <- est[,j]
		var.list[[j]] <- std.err[,j]^2
	}
	if (num.stu < 15) {
		signs <- permutations(2, num.stu, c(-1,1), repeats = TRUE)
		n.perm <- 2^num.stu
	} else {
		signs <- matrix(NA, n.perm, num.stu)
		for (j in 1:n.perm) {
			signs[j,] <- sample(c(-1,1), num.stu, replace = TRUE)
		}
	}
	if (is.null(n.perm)) {
		n.perm <- n.perm
	}
	if (tau0) {
		fit <- function(x, y, num.stu, signs, n.perm) {
			x <- x[!is.na(x)]
			y <- y[!is.na(y)]
			ind.num.stu <- length(x)
			tau20 <- sum((x-mean(x))^2)/ind.num.stu
			W <- diag(tau20/(y+tau20))
			X <- rep(1, ind.num.stu)
			P <- W-W%*%X%*%ginv(t(X)%*%W%*%X)%*%t(X)%*%W
			tau2 <- as.numeric((t(x)%*%P%*%x)/(ind.num.stu-1))
			wi <- 1/(y+tau2)
			mu.est <- sum(wi*x)/sum(wi)
			mu.std.err <- sqrt(1/sum(wi))
			x.null <- matrix(NA, n.perm, ind.num.stu)
			if (is.null(signs)) {
				for (j in 1:n.perm) {
					x.null[j,] <- sample(c(-1,1), ind.num.stu, replace = TRUE)*abs(x)
				}
			} else {
				for (j in 1:n.perm) {
					x.null[j,] <- signs[j,which(!is.na(x))]*abs(x)
				}
			}
			mu.est.tau2.null <- apply(x.null, 1, function(z) {
				tau20.null <- sum((z-mean(z))^2)/ind.num.stu
				W.null <- diag(tau20.null/(y+tau20.null))
				P.null <- W.null-W.null%*%X%*%ginv(t(X)%*%W.null%*%X)%*%t(X)%*%W.null
				tau2.null <- as.numeric((t(z)%*%P.null%*%z)/(ind.num.stu-1))
				wi.null <- 1/(y+tau2.null)
				return(c(sum(wi.null*z)/sum(wi.null), sqrt(1/sum(wi.null))))}
			)
			pv <- (length(which(abs(mu.est.tau2.null[1,]) > abs(mu.est)))+1)/(n.perm+1)
			mu.est.null.list <- as.list(mu.est.tau2.null[1,])
			pv.null <- sapply(mu.est.null.list, function(z) (length(which(abs(mu.est.tau2.null[1,]) > abs(z)))+1)/n.perm)
			return(c(mu.est, mu.est.tau2.null[1,], mu.std.err, mu.est.tau2.null[2,], pv, pv.null))
		}
		out.fit <- mapply(function(x,y) fit(x,y,signs=signs,n.perm=n.perm), est.list, var.list)
		mu.out <- out.fit[1:(n.perm+1),]
		pv.out <- out.fit[((n.perm+1)*2+1):((n.perm+1)*3),]
		ind.pvs <- pv.out[1,]
		ind.mus <- mu.out[1,]
		ind.cis <- t(apply(mu.out[2:(n.perm+1),],2,function(x)quantile(x,prob=c(0.025,0.975))))+ind.mus
		mu.s <- mu.out[1,]
		cov.mat <- matrix(NA, num.mar, num.mar)
		for (j in 1:num.mar) { 
			for (k in 1:num.mar) { 
				cov.mat[j,k] <- cov(mu.out[2:(n.perm+1),j],mu.out[2:(n.perm+1),k])
			}
		}
		wi.mat <- 1/cov.mat
		mmeta.mu <- ave.mu <- sum(wi.mat%*%mu.s)/sum(wi.mat)
		mu.s.null.list <- apply(mu.out[2:(n.perm+1),],1,list)
		ave.mu.null <- sapply(mu.s.null.list, function(x) sum(wi.mat%*%unlist(x))/sum(wi.mat))
		mmeta.pv <- (length(which(abs(ave.mu.null) > abs(ave.mu)))+1)/(n.perm+1)
		mmeta.ci <- quantile(unlist(ave.mu.null),prob=c(0.025,0.975))+ave.mu
		out.fit <- mapply(function(x,y) fit(x,y,signs=NULL,n.perm=n.perm), est.list, var.list)
		pv.out <- out.fit[((n.perm+1)*2+1):((n.perm+1)*3),]
		min.pvs <- apply(pv.out,1,min)
		ameta.pv <- (length(which(min.pvs[-1] < min.pvs[1]))+1)/(n.perm+1)
		out <- rbind(cbind(ind.mus,ind.cis,ind.pvs),
		c(mmeta.mu,mmeta.ci,mmeta.pv),
		c(NA,NA,NA,ameta.pv))
		colnames(out) <- c("Est","Lower(2.5%)","Upper(97.5%)","P-value")
		rownames(out) <- c(colnames(est), "mMeta", "aMeta")	
		return(out)
	}
	if (!tau0) {
		fit <- function(x, y, num.stu, signs, n.perm) {
			x <- x[!is.na(x)]
			y <- y[!is.na(y)]
			ind.num.stu <- length(x)
			tau20 <- sum((x-mean(x))^2)/ind.num.stu
			W <- diag(tau20/(y+tau20))
			X <- rep(1, ind.num.stu)
			P <- W-W%*%X%*%ginv(t(X)%*%W%*%X)%*%t(X)%*%W
			tau2 <- as.numeric((t(x)%*%P%*%x)/(ind.num.stu-1))
			wi <- 1/(y+tau2)
			mu.est <- sum(wi*x)/sum(wi)
			mu.std.err <- sqrt(1/sum(wi))
			x.null <- matrix(NA, n.perm, ind.num.stu)
			if (is.null(signs)) {
				for (j in 1:n.perm) {
					x.null[j,] <- sample(c(-1,1), ind.num.stu, replace = TRUE)*abs(x)
				}
			} else {
				for (j in 1:n.perm) {
					x.null[j,] <- signs[j,which(!is.na(x))]*abs(x)
				}
			}
			mu.est.tau2.null <- apply(x.null, 1, function(z) {
				tau2.null <- 0
				wi.null <- 1/(y+tau2.null)
				return(c(sum(wi.null*z)/sum(wi.null), sqrt(1/sum(wi.null))))}
			)
			pv <- (length(which(abs(mu.est.tau2.null[1,]) > abs(mu.est)))+1)/(n.perm+1)
			mu.est.null.list <- as.list(mu.est.tau2.null[1,])
			pv.null <- sapply(mu.est.null.list, function(z) (length(which(abs(mu.est.tau2.null[1,]) > abs(z)))+1)/n.perm)
			return(c(mu.est, mu.est.tau2.null[1,], mu.std.err, mu.est.tau2.null[2,], pv, pv.null))
		}
		out.fit <- mapply(function(x,y) fit(x,y,signs=signs,n.perm=n.perm), est.list, var.list)
		mu.out <- out.fit[1:(n.perm+1),]
		pv.out <- out.fit[((n.perm+1)*2+1):((n.perm+1)*3),]
		ind.pvs <- pv.out[1,]
		ind.mus <- mu.out[1,]
		ind.cis <- t(apply(mu.out[2:(n.perm+1),],2,function(x)quantile(x,prob=c(0.025,0.975))))+ind.mus
		mu.s <- mu.out[1,]
		cov.mat <- matrix(NA, num.mar, num.mar)
		for (j in 1:num.mar) { 
			for (k in 1:num.mar) { 
				cov.mat[j,k] <- cov(mu.out[2:(n.perm+1),j],mu.out[2:(n.perm+1),k])
			}
		}
		wi.mat <- 1/cov.mat
		mmeta.mu <- ave.mu <- sum(wi.mat%*%mu.s)/sum(wi.mat)
		mu.s.null.list <- apply(mu.out[2:(n.perm+1),],1,list)
		ave.mu.null <- sapply(mu.s.null.list, function(x) sum(wi.mat%*%unlist(x))/sum(wi.mat))
		mmeta.pv <- (length(which(abs(ave.mu.null) > abs(ave.mu)))+1)/(n.perm+1)
		mmeta.ci <- quantile(unlist(ave.mu.null),prob=c(0.025,0.975))+ave.mu
		out.fit <- mapply(function(x,y) fit(x,y,signs=NULL,n.perm=n.perm), est.list, var.list)
		pv.out <- out.fit[((n.perm+1)*2+1):((n.perm+1)*3),]
		min.pvs <- apply(pv.out,1,min)
		ameta.pv <- (length(which(min.pvs[-1] < min.pvs[1]))+1)/(n.perm+1)
		out <- rbind(cbind(ind.mus,ind.cis,ind.pvs),
		c(mmeta.mu,mmeta.ci,mmeta.pv),
		c(NA,NA,NA,ameta.pv))
		colnames(out) <- c("Est","Lower(2.5%)","Upper(97.5%)","P-value")
		rownames(out) <- c(colnames(est), "mMeta", "aMeta")	
		return(out)
	}
}
