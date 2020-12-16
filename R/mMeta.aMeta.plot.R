mMeta.aMeta.plot <-
function(mMeta.aMeta.out){ 
	pvs <- round(mMeta.aMeta.out[,4], 3)
	ind.0 <- which(pvs == 0) 
	ind.1 <- which(pvs == 1)
	pvs[ind.0] <- "<.001"
	pvs[ind.1] <- ">.999"

	text.tab <- cbind(c(NA, rownames(mMeta.aMeta.out)), c("p-value", pvs))
	ci.tab <- as.matrix(rbind(c(NA, NA, NA), mMeta.aMeta.out[,c(1,2,3)]))

	forestplot(labeltext=text.tab, mean=ci.tab[,1], lower=ci.tab[,2], upper=ci.tab[,3], 
	hrzl_lines=TRUE, new_page=TRUE, boxsize=0.25, line.margin=0.5, grid=TRUE,
	col=fpColors(box="black",line="black", summary="red3"),
	is.summary=c(TRUE,rep(FALSE,(nrow(mMeta.aMeta.out)-2)),TRUE,TRUE), xlab="95% Confidence Interval",
	txt_gp=fpTxtGp(label=list(gpar(fontfamily="", cex=0.8),gpar(fontfamily="", cex=0.8)),
	ticks=gpar(fontfamily="", cex=0.8),
	xlab=gpar(fontfamily="", cex=0.8)))
}
