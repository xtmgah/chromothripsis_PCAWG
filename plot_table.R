

# table now comes from cands_final

plot_table = function(table_now){

cols=c("histo",
          "position",
         "chrom",
         "type",
        "clusterSize",
        "clusterSize_all",
       "SVs",
       "SVs_sample",
          "imax",
          "i_i2_eval",
         "fragment_joints_FDR",
         "chr_breakpoint_enrichment_FDR",
        "inter_other_chroms_coords_all",
        "donor_unique_id",
"pur","plo")
table_now = table_now[,cols]

col_nams = c("Cancer type","Position", "chr",
              "Type",
              "Interleaved intrachr. SVs","Total SVs (intrachr. + transl.)",
              "SV types",
              "SVs in sample","Oscillating CN (2 and 3 states)",
               "CN segments", "FDR fragment joints","FDR chr. breakp. enrich.","Linked to chrs","donor_unique_id","Purity, ploidy")
names(table_now) = col_nams





mytheme <- gridExtra::ttheme_minimal(padding = unit(c(1.8,1.8),"mm"),
                                          core = list(fg_params=list(cex = .5)),
                                         colhead = list(fg_params=list(cex = .5)),#,fontface="italic")),
                                         rowhead = list(fg_params=list(cex = .5)))
nams = names(table_now)
dims = ncol(table_now)

table_now = t(table_now)
rownames(table_now) = nams
colnames(table_now) = don
s <- tableGrob(table_now, theme=mytheme) 

return(s)
}
