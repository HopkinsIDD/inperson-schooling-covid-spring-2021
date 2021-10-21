## Helper Functions/variables
sch_order <- c("maskMand_st","maskMand_tch" , "restEntry",
               "xdeskSpace" ,   "noShareSupp", "sameSt",
               "redClassSize",  "dailySymptScr", "sameTch",
               "noExtraCurr","closedCafe",   "deskShields" ,
               "closedPlay","outdoorInstr" )

sch_names <-c("student masking",
              "teacher masking",
              "restricted entry",
              "extra space",
              "no supply sharing",
              "same students",
              "reduced class size",
              "daily symptom screen",
              "same teacher",
              "no extracurricular",
              "closed cafe",
              "desk shields",
              "closed playground",
              "outdoor instruction")

names(sch_names)<-sch_order


##' Get a list of data used by fig 1. Convience function
##' 
fig_1_fig_dat<- function(df) {
   rc <- list()
   
   ##responses by county
   rc$resp_by_cnty <- df%>%
      group_by(fips)%>%
      summarize(n=n())%>%
      ungroup
   
   ##in person scholing by county
   rc$inper_school_by_county <- df %>%
      filter(Child_IP_any%in%c("Yes","No"))%>%
      group_by(fips)%>%
      summarize(n=n(),
                n_ip=sum(Child_IP_any=="Yes"))%>%
      ungroup()%>%
      mutate(pct_ip = n_ip/n*100)
   
   ##peent full time by county
   rc$pct_ft_by_cnty <- df%>% filter(Child_IP_any=="Yes")%>%
      group_by(fips) %>%
      summarize(n=n(),
                pct_ft = mean(Child_IP_full_bin, na.rm=T)*100)%>%
      ungroup()%>%
      filter(n>=10)
   
   
   ##number of mitigations by county
   rc$n_mit_by_county <- df%>% filter(Child_IP_any=="Yes")%>%
      mutate(n_interventions=#Child_IP_part_bin+
                sch_maskMand_st+
                sch_maskMand_tch+
                sch_sameTch+
                sch_sameSt+
                sch_outdoorInstr+
                sch_restEntry+
                sch_redClassSize+
                sch_closedCafe+
                sch_closedPlay+
                sch_deskShields+
                sch_xdeskSpace+
                sch_noExtraCurr+
                sch_noShareSupp+ 
                sch_dailySymptScr)%>%
      filter(n_interventions<=13)%>%
      group_by(fips)%>%
      summarize(n=n(),
                avg_int=mean(n_interventions, na.rm=T))%>%
      ungroup()%>%
      filter(n>=10)
   
   return(rc)
}

##' Figure 1: Both periods and delta between them.
##' 
fig_1 <- function(df) {
   fig_dat_p1 <- df%>%
      filter(period=="jan_mar")%>%
      fig_1_fig_dat
   
   
   fig_dat_p2 <- df%>%
      filter(period=="apr_jun")%>%
      fig_1_fig_dat
 
   
   ##Panel A: Respondents with children in HH
   resp_kids1 <- usmap::plot_usmap(regions="counties", 
                                  data=fig_dat_p1$resp_by_cnty,
                                  values="n", 
                                  color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_continuous(type="viridis", trans="log10",
                            limits=c(1,10000))+
      #ggtitle("Number of responsents reporting children in HH.")+
      labs(fill="N respondents")
   
   
   resp_kids2 <- usmap::plot_usmap(regions="counties", 
                                   data=fig_dat_p2$resp_by_cnty,
                                   values="n", 
                                   color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_continuous(type="viridis", trans="log10",
                            limits=c(1,10000))+
      #ggtitle("Number of responsents reporting children in HH.")+
      labs(fill="N respondents")
   
   
   resp_by_cnty_diff <- fig_dat_p2$resp_by_cnty%>%
      select(fips, n) %>%
      rename(n2=n)%>%
      inner_join(select(fig_dat_p1$resp_by_cnty, fips, n))%>%
      mutate(n_diff=n2/n)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   resp_kids_diff <- usmap::plot_usmap(regions="counties", 
                                   data=resp_by_cnty_diff,
                                   values="n_diff", 
                                   color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                   #limits=log10(c(1/12,12)), 
                                                   name="Relative Amount",
                                                   trans="log10") #+
      #scale_fill_continuous(type="viridis", trans="log10")
   
   
   
   ##Panel B: Proportion reporting in person schooling by county with
   ##>10 responondants
   
   ip_pct1 <- usmap::plot_usmap(regions="counties", 
                               data=filter(fig_dat_p1$inper_school_by_county, n>=10),
                               values="pct_ip",
                               color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent in person") +
      theme(legend.position = "bottom")
   
   
   ip_pct2 <- usmap::plot_usmap(regions="counties", 
                                data=filter(fig_dat_p2$inper_school_by_county, n>=10),
                                values="pct_ip",
                                color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent in person") +
      theme(legend.position = "bottom")
   
   
   
    inper_school_by_cnty_diff <- fig_dat_p2$inper_school_by_county%>%
      select(fips, pct_ip) %>%
      rename(pct_ip2=pct_ip)%>%
      inner_join(select(fig_dat_p1$inper_school_by_county, fips, pct_ip))%>%
      mutate(pct_ip_diff=pct_ip2/pct_ip)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   ip_diff <- usmap::plot_usmap(regions="counties", 
                                       data= inper_school_by_cnty_diff,
                                       values="pct_ip_diff", 
                                       color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Relative Amount",
                                                  trans="log10") #+
   #scale_fill_continuous(type="viridis", trans="log10")
   
   
      
   ##The pct reporting FT in counties where at least 10 people are reporting schooling
   
   ft_pct1 <- usmap::plot_usmap(regions="counties", 
                               data=fig_dat_p1$pct_ft_by_cnty,
                               values="pct_ft",
                               color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent full time") +
      theme(legend.position = "bottom")
   
   
   ft_pct2 <- usmap::plot_usmap(regions="counties", 
                                data=fig_dat_p2$pct_ft_by_cnty,
                                values="pct_ft",
                                color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent full time") +
      theme(legend.position = "bottom")
   
   
   
   pct_ft_by_cnty_diff <- fig_dat_p2$pct_ft_by_cnty%>%
      select(fips, pct_ft) %>%
      rename(pct_ft2=pct_ft)%>%
      inner_join(select(fig_dat_p1$pct_ft_by_cnty, fips, pct_ft))%>%
      mutate(pct_ft_diff=pct_ft2/pct_ft)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   ft_diff <- usmap::plot_usmap(regions="counties", 
                                data= pct_ft_by_cnty_diff,
                                values="pct_ft_diff", 
                                color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Relative Amount",
                                                  trans="log10") 
   
   ##Now the average number of interventions in those counties where at least 10 
   ##people ace in person schooling
   
   
   cnty_mit1 <- usmap::plot_usmap(regions="counties", 
                                 data=fig_dat_p1$n_mit_by_county,
                                 values="avg_int", 
                                 color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_distiller(palette="BrBG", direction=1, limits=c(0,11))+
      labs(fill="avg. # mitigations")
   
   
   cnty_mit2 <- usmap::plot_usmap(regions="counties", 
                                  data=fig_dat_p2$n_mit_by_county,
                                  values="avg_int", 
                                  color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_distiller(palette="BrBG", direction=1, limits=c(0,11))+
      labs(fill="avg. # mitigations")
   
     
   
   n_mit_diff <- fig_dat_p2$n_mit_by_county%>%
      select(fips,avg_int) %>%
      rename(avg_int2=avg_int)%>%
      inner_join(select(fig_dat_p1$n_mit_by_county, fips, avg_int))%>%
      mutate(avg_int_diff=avg_int2-avg_int)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   cnty_mit_diff <- usmap::plot_usmap(regions="counties", 
                                data= n_mit_diff,
                                values="avg_int_diff", 
                                color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Red-Green",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Difference") 
   
   
   ##Return the layed out figure
   rc <- cowplot::plot_grid(resp_kids1, resp_kids2, resp_kids_diff,
                            ip_pct1, ip_pct2, ip_diff,
                            ft_pct1, ft_pct2, ft_diff,
                            cnty_mit1, cnty_mit2, cnty_mit_diff,
                            labels=c("A","","","B","","","C","","","D","",""), ncol=3)
   
   return(rc)
}


##' Figure 2: National-level line plot.
##' 
make_natl_plot <- function(df_us2, cat, y_lab, col) {
   
   # if we're not plotting variants, just do a line plot
   if (cat != 'variant_fit') {
      # subset to group
      nat_plot <- 
         ggplot(df_us2%>%filter(variable==cat)) +
         # line plot
         geom_line(aes(x = Date, y = mean), 
                   color = col, size = 1) +
         theme_bw()
      
      # if we are plotting variants, plot Alpha and Delta separately in area plot
   } else {
      # subset to group
      nat_plot <- 
         ggplot(df_us2%>%
                   filter(variable%in%c('Alpha', 'Delta'))%>%
                   mutate(variable = factor(variable, 
                                            levels = c('Delta', 'Alpha')))) +
         # area plot
         geom_area(aes(x = Date, y = mean, 
                       group = variable, fill = variable),
                   color = 'black') +
         scale_fill_manual(values=alpha(c(col, col), c(0.9, 0.5)),
                           name = NULL) +
         theme_bw() + 
         theme(legend.position = c(0.15, 0.7),
               legend.background = element_blank())
      
   }
   
   # add all the other plot attributes
   nat_plot <- nat_plot + 
      labs(y = '   ') +
      scale_x_date(limits = as.Date(c("2021-01-08", "2021-06-18")),
                   breaks = as.Date(c("2021-02-01",
                                      "2021-03-01", "2021-04-01",
                                      "2021-05-01", "2021-06-01")),
                   labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun'),
                   minor_breaks = NULL) +
      ggtitle(y_lab) +
      ylim(c(0, ifelse(cat=='n_interventions', 13, 100)))+
      theme(axis.title.x = element_blank())
   
   # end function
   return(nat_plot)
}

##' Figure 2: County-level histogram
##' 
make_cnty_plot <- function(df_cnty, cat, x_lab, col) {
   # subset to group
   ggplot(df_cnty%>%filter(variable==cat)) +
      # plot
      geom_histogram(aes(value), 
                     fill = col, color = 'black', 
                     size = 0.4, alpha = 0.7, bins = 20) +
      coord_flip() +
      facet_wrap('month', nrow = 1) +
      labs(x = x_lab) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}


##' Figure S1: Dot plot comparing vaccination rates
##' 
compare_vax_plot <- function(vax_plot, x_col) {
   ggplot(vax_plot%>%filter(variable==x_col), 
          aes(x = value, y = k12_somevax)) +
      geom_point(alpha = 0.1) +
      geom_smooth() +
      geom_text(data=vals%>%filter(variable==x_col),
                aes(label=sprintf("cor=%1.2f\nslope=%1.1f (95%% CI %1.1f,%1.1f)",
                                  cor,estimate,conf.low, conf.high)),
                x=.0, y=1, color="blue",
                hjust=0, vjust=1,size=3.5) +
      geom_abline(slope = 1, intercept = 0, color = '#FF9900') +
      theme_bw() +
      xlab(paste0('Proportion vaccinated (', x_col, ')')) + 
      ylab('Proportion vaccinated (K-12 respondents)')
}


##' Figure S1: Map comparing vaccination rates
##' 
compare_vax_map <- function(vax_plot, var) {
   usmap::plot_usmap(regions="counties", 
                     data= vax_plot%>%filter(variable==var)%>%select(-variable),
                     values="avg_vx_diff", 
                     color=NA) +
      theme(legend.position = "bottom") +
      colorspace::scale_fill_continuous_diverging(palette = "Purple-Green",mid=0,
                                                  name="Average difference in \nproportion vaccinated") +
      ggtitle(paste0('Difference between K-12 respondents and ', var)) +
      theme(plot.title = element_text(hjust = 0.5))
}


##' Figure 4: Odds ratios with individual mitigation measures
##' 
mit_impact_plot <- function(mt_res, time_period) {
   
   # clean up and prep results
   mitt_effs <- mt_res%>%
      filter(period%in%time_period)%>%
      filter(str_detect(variable, "sch_")|variable=='Child_IP_part_bin')%>%
      filter(str_detect(adjustment,"adjusted"))%>%
      mutate(variable=factor(str_remove(variable,"sch_"),
                             levels=c(sch_order, 'Child_IP_part_bin')))%>%
      
      # basic plot
      ggplot(aes(x=variable, y=estimate, ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5)) +
      geom_hline(yintercept=1)+
      scale_color_brewer(name="",type="qual", palette = 'Dark2')+
      theme_bw()+
      ylab("Odds ratio")+
      xlab(NULL)+
      scale_x_discrete(labels=c(sch_names, 
                                Child_IP_part_bin="part time"))+
      theme(axis.text.x = element_text( angle = 45, hjust=1),
            legend.position='bottom')
   
   # return
   return(mitt_effs)
}


##' Figure 7: County-month-level correlations
##' 
##' @param data Data frame with county-month aggregated data
##' @param time_period Months to run analysis for
##' @param plot_tite Title for the plot
##' @param return_values Whether or not to return tables with corr and CI
##' 
run_corr_analysis <- function(data, time_period, plot_title,
                              return_values = FALSE) {
   
   # subset to period, if applicable
   df_cor <- data %>%
      filter(period %in% time_period) %>%
      select(-c('period', 'fips', 'month'))
   
   # correlation matrix
   cor_mat <- cor(df_cor, use = 'pairwise.complete.obs')
   
   # p values
   testRes <- cor.mtest(df_cor, conf.level = 0.95)
   
   # return values, if needed
   if (return_values) {
      print(plot_title)
      print(cor_mat)
      print(testRes)
   }

   # correlation plot
   ggcorrplot(cor_mat, lab = TRUE, ggtheme=ggplot2::theme_bw,
              colors = brewer.pal(n = 9, name = 'PRGn')[c(1,5,9)],
              tl.col = 'black', tl.srt = 45, insig = 'blank',
              p.mat = testRes$p, sig.level = 0.05,
              title = plot_title) -> p1
   
   # center title
   p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))
   
   # return plot
   return(p1)
}