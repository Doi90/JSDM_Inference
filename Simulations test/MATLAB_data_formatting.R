for(beta_id in c("nh", "hi")){
  
  for(corr_id in c("fc", "ac")){
    
    for(cov_id in c("mo", "mu")){
      
      for(sim in seq_len(10)){
        
        ## Create directory structure
        
        dir <- sprintf("Simulations test/MATLAB/examples/%s_%s_%s_%s",
                       beta_id,
                       corr_id,
                       cov_id,
                       sim)
        dir2 <- sprintf("%s/data",
                        dir)
        dir3 <- sprintf("%s/panels",
                        dir)
        dir4 <- sprintf("%s/posteriors",
                        dir)
        dir5 <- sprintf("%s/predictions",
                        dir)
        dir6 <- sprintf("%s/results",
                        dir)
        
        if(!dir.exists(dir)){
          dir.create(dir)
        } 
        
        if(!dir.exists(dir2)){
          dir.create(dir2)
        }
        
        if(!dir.exists(dir3)){
          dir.create(dir3)
        }
        
        if(!dir.exists(dir4)){
          dir.create(dir4)
        }
        
        if(!dir.exists(dir5)){
          dir.create(dir5)
        }
        
        if(!dir.exists(dir6)){
          dir.create(dir6)
        }
        
        ## Generate the necessary data files for HLR models
        
        ### PA
        
        filename_in <- sprintf("Simulations test/Datasets/Sim%s/pa_%s_%s_%s.csv",
                               sim,
                               beta_id,
                               corr_id,
                               cov_id)
        
        tmp <- read.csv(filename_in)
        
        filename_out <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/y_%1$s_%2$s_%3$s_%4$s.csv",
                                beta_id,
                                corr_id,
                                cov_id,
                                sim)
        
        write.table(tmp,
                    filename_out,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        filename_out <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/species_%1$s_%2$s_%3$s_%4$s.txt",
                                beta_id,
                                corr_id,
                                cov_id,
                                sim)
        
        file_tmp <- file(filename_out)  
        
        writeLines(colnames(tmp), file_tmp)    
        
        close(file_tmp)
        
        ### Covariates
        
        if(cov_id == "mo"){
          
          filename_in <- sprintf("Simulations test/Datasets/Sim%s/meas_cov.csv",
                                   sim)
          
        } else if(cov_id == "mu"){
          
          filename_in <- sprintf("Simulations test/Datasets/Sim%s/meas_unmeas_cov.csv",
                                   sim)
          
        }
        
        tmp <- read.csv(filename_in)
        
        filename_out <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/X_%1$s_%2$s_%3$s_%4$s.csv",
                                beta_id,
                                corr_id,
                                cov_id,
                                sim)
        
        write.table(tmp,
                    filename_out,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        filename_out <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/covariates_%1$s_%2$s_%3$s_%4$s.txt",
                                beta_id,
                                corr_id,
                                cov_id,
                                sim)
        
        file_tmp <- file(filename_out)  
        
        writeLines(c("Intercept", colnames(tmp)[-1]), file_tmp)    
        
        close(file_tmp)
        
        ### Misc other files
        
        #### LF_units
        
        filename <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/LF_units_%1$s_%2$s_%3$s_%4$s.csv",
                            beta_id,
                            corr_id,
                            cov_id,
                            sim)
        
        write.table(1:nrow(tmp),
                    filename,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        
        #### LF_alpha
        
        filename <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/LF_alpha_%1$s_%2$s_%3$s_%4$s_1.csv",
                            beta_id,
                            corr_id,
                            cov_id,
                            sim)
        
        alpha <- data.frame(a = seq(0, 1, 0.01),
                            b = c(0.5, rep(0.005, 100)))
        
        write.table(alpha,
                    filename,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        #### dist
        
        filename <- sprintf("Simulations test/MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/dist_%1$s_%2$s_%3$s_%4$s.csv",
                            beta_id,
                            corr_id,
                            cov_id,
                            sim)
        
        write.table(rep(2, 10),
                    filename,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
      }
    }
  }
}