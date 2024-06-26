volcano_plot_text_P05 <- function(x, title) {
    
    #x <- dplyr::filter(x, PValue <0.3)
    
    test <- ifelse(x$PValue, "Non significant")
    test <- ifelse(x$logFC > 0 & x$PValue <0.05, "PValue < 0.05 & logFC > 0", test)
    test <- ifelse(x$logFC < 0 & x$PValue <0.05, "PValue < 0.05 & logFC < 0", test)
    test <- ifelse(x$logFC > 0 & x$FDR <0.1, "FDR < 0.1 & logFC > 0", test)
    test <- ifelse(x$logFC < 0 & x$FDR <0.1, "FDR < 0.1 & logFC < 0", test)
    
    plotDat <- data.frame(x, Group=test)
    
    plotDat$logFC <- ifelse(plotDat$logFC > 1, 1, plotDat$logFC)
    plotDat$logFC <- ifelse(plotDat$logFC < -1, -1, plotDat$logFC)
    
    
    p <- ggplot(plotDat, aes(x = logFC, y=-log10(PValue), fill=Group, col = Group, label=gene_name)) +
        geom_point(size=0.9, pch=21, alpha=0.7, stroke = 0.5)+
        theme_light()+
        geom_vline(xintercept = 1, linetype = 2)+
        geom_vline(xintercept = -1, linetype = 2)+
        geom_text_repel(data = filter(plotDat, PValue < 0.05))+
        #geom_text(data = filter(plotDat, FDR < 0.3), 
        #          aes(x = logFC, y=-log10(PValue), fill=Group, col = Group, label=gene_name))+
        scale_fill_manual(values=c("Non significant"="grey30", 
                                   "PValue < 0.05 & logFC > 0"="#eb5e60", 
                                   "PValue < 0.05 & logFC < 0"="#62a0ca", 
                                   "FDR < 0.1 & logFC > 0" = "#960304", 
                                   "FDR < 0.1 & logFC < 0" = "#01538a"))+
        scale_color_manual(values=c("Non significant"="grey30", 
                                    "PValue < 0.05 & logFC > 0"="#eb5e60", 
                                    "PValue < 0.05 & logFC < 0"="#62a0ca", 
                                    "FDR < 0.1 & logFC > 0" = "#960304", 
                                    "FDR < 0.1 & logFC < 0" = "#01538a"))+
        labs(title= title, y="-log10(P)", x="logFC")+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(legend.text=element_text(size=16))+
        theme(axis.text=element_text(size=14))+
        theme(legend.title=element_blank())+
        theme(axis.text=element_text(size=14))+
        theme(axis.title = element_text(size=14, face = "bold"))+
        theme(legend.position="none")+
        coord_cartesian(xlim = c(-1.1, 1.1))+
        geom_hline(yintercept = -log10(0.05), linetype=2)+
        scale_x_continuous(breaks=c(-1, -0.5, 0, 0.5, 1), labels=c("<-1","-0.5", "0", "0.5", ">1"))+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    p
    
}