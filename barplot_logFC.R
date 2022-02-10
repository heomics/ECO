options(encoding = "UTF-8")
library(ggplot2)
library(grid)              
library(ggh4x)             
library(ggnewscale)       
library(ggplotify)         
library(ggtext)            
library(ggpubr)

####
load("data_logFC.RData")

####
input=list()
input$selectGeneSymbol <- "Sele"

data_end<-data.frame(Name_logFC,
                     CIL=data_logFC[data_logFC$Name %in% c(paste0(1:40,"_CI.L")),input$selectGeneSymbol],
                     CIR=data_logFC[data_logFC$Name %in% c(paste0(1:40,"_CI.R")),input$selectGeneSymbol],
                     LogFC=data_logFC[data_logFC$Name %in% c(paste0(1:40,"_logFC")),input$selectGeneSymbol],
                     pStar=symnum(data_logFC[data_logFC$Name %in% c(paste0(1:40,"_P.Value")),input$selectGeneSymbol],cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*',' '),na = F))
data_end$fill<-factor(data_end$fill,levels = c("Disease","Gene modification","Treatment","Basal condition"))


p<-ggplot(data = data_end, aes(x =Name, y=LogFC,label = Name, fill=Organ)) +
  geom_bar(stat = "identity", position="dodge",color="black",width = 0.9) +
  geom_errorbar(aes(ymin = CIL, ymax = CIR), width = .2) +
  rotate_x_text(angle = 90) +
  facet_nested(.~Organ+Disease, scales = "free_x", space = "free_x",switch = "x")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text.x = element_markdown(size = 6, angle=0),
        axis.text.x=element_text(size=8),
        strip.placement = "outside",
        plot.margin = unit(c(5, 10, 10, 20), "mm"),
        panel.background=element_rect(fill='transparent'),
        axis.line.y= element_line(color='black')) +
  geom_text(label=data_end$pStar,y=data_end$CIR+0.2,size = 5,na.rm = F)+
  labs(title = input$selectGeneSymbol, x = NULL, y = "logFC")+
  theme(plot.title = element_text(face = "italic"))+
  rotate_x_text(angle = 90)+
  scale_fill_manual(name = "Organ",values = unique(data_end$Organ_fill))+
  theme(legend.text = element_markdown())+
  labs(tag= "Conditions:     \n\n\nOrgans: ") +
  theme(plot.tag.position = c(0, 0.14),
        plot.tag = element_text(size = 12))+
  coord_cartesian(clip = "off")+
  #####
new_scale_fill()+geom_col(aes(y=0,fill=fill)) + 
  scale_fill_manual(values=pal,drop=FALSE,name="Condition",na.translate = FALSE)+  ## or use limits()
  theme(
    legend.key = element_rect(
      color = "black" # line color
    ))+
  theme(strip.text.x = element_markdown(margin = margin(b = 1,r=10,l=10)))
gt <- ggplotGrob(p)
gt <- assign_strip_colours(gt, 1:36,colours2)
grid.newpage(); grid.draw(gt)
as.ggplot(gt)

#####
sessionInfo()


R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
  [1] zh_CN.UTF-8/zh_CN.UTF-8/zh_CN.UTF-8/C/zh_CN.UTF-8/zh_CN.UTF-8

attached base packages:
  [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] kableExtra_1.3.4.9000 shinydashboard_0.7.1  shinymanager_1.0.300  pheatmap_1.0.12      
[5] ggtext_0.1.1          ggplotify_0.0.6       ggnewscale_0.4.5      ggh4x_0.1.2.1        
[9] shinyjs_2.0.0         shinyBS_0.61          DT_0.18               psych_2.1.3          
[13] shinythemes_1.2.0     ggpubr_0.4.0          gridExtra_2.3         ggplot2_3.3.3        
[17] tidyr_1.1.3           dplyr_1.0.6           openxlsx_4.2.3        shiny_1.6.0          

loaded via a namespace (and not attached):
  [1] readxl_1.3.1        backports_1.2.1     systemfonts_1.0.1   crosstalk_1.1.1    
[5] usethis_2.0.1       digest_0.6.27       htmltools_0.5.1.1   rsconnect_0.8.17   
[9] fansi_0.4.2         magrittr_2.0.1      memoise_2.0.0       billboarder_0.3.1  
[13] remotes_2.3.0       R.utils_2.10.1      svglite_2.0.0       askpass_1.1        
[17] prettyunits_1.1.1   colorspace_2.0-1    blob_1.2.1          rvest_1.0.0        
[21] haven_2.4.1         xfun_0.22           callr_3.7.0         crayon_1.4.1       
[25] jsonlite_1.7.2      glue_1.4.2          gtable_0.3.0        webshot_0.5.2      
[29] car_3.0-10          pkgbuild_1.2.0      abind_1.4-5         scales_1.1.1       
[33] DBI_1.1.1           rstatix_0.7.0       Rcpp_1.0.6          viridisLite_0.4.0  
[37] xtable_1.8-4        gridtext_0.1.4      tmvnsim_1.0-2       gridGraphics_0.5-1 
[41] foreign_0.8-81      bit_4.0.4           htmlwidgets_1.5.3   httr_1.4.2         
[45] sourcetools_0.1.7   RColorBrewer_1.1-2  ellipsis_0.3.2      pkgconfig_2.0.3    
[49] R.methodsS3_1.8.1   farver_2.1.0        sass_0.3.1          utf8_1.2.1         
[53] tidyselect_1.1.1    labeling_0.4.2      rlang_0.4.11        later_1.2.0        
[57] munsell_0.5.0       cellranger_1.1.0    tools_4.0.3         cachem_1.0.4       
[61] scrypt_0.1.3        cli_2.5.0           generics_0.1.0      RSQLite_2.2.7      
[65] devtools_2.4.1      broom_0.7.6         evaluate_0.14       stringr_1.4.0      
[69] fastmap_1.1.0       yaml_2.2.1          processx_3.5.2      knitr_1.33         
[73] bit64_4.0.5         fs_1.5.0            zip_2.1.1           purrr_0.3.4        
[77] nlme_3.1-152        mime_0.10           R.oo_1.24.0         xml2_1.3.2         
[81] compiler_4.0.3      rstudioapi_0.13     curl_4.3.1          testthat_3.0.2     
[85] ggsignif_0.6.1      tibble_3.1.1        bslib_0.2.4         stringi_1.5.3      
[89] ps_1.6.0            desc_1.3.0          forcats_0.5.1       lattice_0.20-44    
[93] markdown_1.1        vctrs_0.3.8         pillar_1.6.0        lifecycle_1.0.0    
[97] BiocManager_1.30.13 jquerylib_0.1.4     data.table_1.14.0   httpuv_1.6.1       
[101] R6_2.5.0            promises_1.2.0.1    rio_0.5.26          sessioninfo_1.1.1  
[105] assertthat_0.2.1    pkgload_1.2.1       openssl_1.4.4       rprojroot_2.0.2    
[109] withr_2.4.2         mnormt_2.0.2        parallel_4.0.3      hms_1.0.0          
[113] rmarkdown_2.8       rvcheck_0.1.8       carData_3.0-4       tinytex_0.31  