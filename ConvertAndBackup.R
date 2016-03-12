temp <- concat(OUTPUT_DIR,'/Q1_norm.rda')
if (length(grep('Q1_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q1_norm <- ordinalToMetric(as.matrix(D$Q1), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q1_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q2_norm.rda')
if (length(grep('Q2_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q2_norm <- ordinalToMetric(as.matrix(D$Q2), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q2_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q3_norm.rda')
if (length(grep('Q3_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q3_norm <- ordinalToMetric(as.matrix(D$Q3), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q3_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q4_norm.rda')
if (length(grep('Q4_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q4_norm <- ordinalToMetric(as.matrix(D$Q4), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q4_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q5_norm.rda')
if (length(grep('Q5_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q5_norm <- ordinalToMetric(as.matrix(D$Q5), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q5_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q6_norm.rda')
if (length(grep('Q6_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q6_norm <- ordinalToMetric(as.matrix(D$Q6), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q6_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q7_norm.rda')
if (length(grep('Q7_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q7_norm <- ordinalToMetric(as.matrix(D$Q7), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q7_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q8_norm.rda')
if (length(grep('Q8_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q8_norm <- ordinalToMetric(as.matrix(D$Q8), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q8_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q9_norm.rda')
if (length(grep('Q9_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q9_norm <- ordinalToMetric(as.matrix(D$Q9), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q9_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q10_norm.rda')
if (length(grep('Q10_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q10_norm <- ordinalToMetric(as.matrix(D$Q10), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q10_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q11_norm.rda')
if (length(grep('Q11_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q11_norm <- ordinalToMetric(as.matrix(D$Q11), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q11_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q12_norm.rda')
if (length(grep('Q12_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q12_norm <- ordinalToMetric(as.matrix(D$Q12), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q12_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q13_norm.rda')
if (length(grep('Q13_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q13_norm <- ordinalToMetric(as.matrix(D$Q13), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q13_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q14_norm.rda')
if (length(grep('Q14_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q14_norm <- ordinalToMetric(as.matrix(D$Q14), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q14_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q15_norm.rda')
if (length(grep('Q15_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q15_norm <- ordinalToMetric(as.matrix(D$Q15), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q15_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q16_norm.rda')
if (length(grep('Q16_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q16_norm <- ordinalToMetric(as.matrix(D$Q16), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q16_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q17_norm.rda')
if (length(grep('Q17_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q17_norm <- ordinalToMetric(as.matrix(D$Q17), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q17_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q18_norm.rda')
if (length(grep('Q18_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q18_norm <- ordinalToMetric(as.matrix(D$Q18), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q18_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q19_norm.rda')
if (length(grep('Q19_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q19_norm <- ordinalToMetric(as.matrix(D$Q19), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q19_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q20_norm.rda')
if (length(grep('Q20_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q20_norm <- ordinalToMetric(as.matrix(D$Q20), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q20_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q21_norm.rda')
if (length(grep('Q21_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q21_norm <- ordinalToMetric(as.matrix(D$Q21), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q21_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q22_norm.rda')
if (length(grep('Q22_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q22_norm <- ordinalToMetric(as.matrix(D$Q22), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q22_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q23_norm.rda')
if (length(grep('Q23_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q23_norm <- ordinalToMetric(as.matrix(D$Q23), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q23_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q24_norm.rda')
if (length(grep('Q24_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q24_norm <- ordinalToMetric(as.matrix(D$Q24), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q24_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q25_norm.rda')
if (length(grep('Q25_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q25_norm <- ordinalToMetric(as.matrix(D$Q25), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q25_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q26_norm.rda')
if (length(grep('Q26_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q26_norm <- ordinalToMetric(as.matrix(D$Q26), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q26_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q27_norm.rda')
if (length(grep('Q27_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q27_norm <- ordinalToMetric(as.matrix(D$Q27), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q27_norm, file=temp)
  }
  load(temp)
}
temp <- concat(OUTPUT_DIR,'/Q28_norm.rda')
if (length(grep('Q28_norm', ls()))==0) {
  if (!file.exists(temp)) {
    Q28_norm <- ordinalToMetric(as.matrix(D$Q28), 
                               scaleType = "o", 
                               patternCoordinates = 5)
    save(Q28_norm, file=temp)
  }
  load(temp)
}


D_norm <- data.frame(D$instr,
                     D$class,
                     D$nb.repeat,
                     D$attendance,
                     D$difficulty,
                     Q1_norm,
                     Q2_norm,
                     Q3_norm,
                     Q4_norm,
                     Q5_norm,
                     Q6_norm,
                     Q7_norm,
                     Q8_norm,
                     Q9_norm,
                     Q10_norm,
                     Q11_norm,
                     Q12_norm,
                     Q13_norm,
                     Q14_norm,
                     Q15_norm,
                     Q16_norm,
                     Q17_norm,
                     Q18_norm,
                     Q19_norm,
                     Q20_norm,
                     Q21_norm,
                     Q22_norm,
                     Q23_norm,
                     Q24_norm,
                     Q25_norm,
                     Q26_norm,
                     Q27_norm,
                     Q28_norm)
temp <- concat(OUTPUT_DIR,'/D_norm.rda')
if (length(grep('D_norm', ls()))==0) {
  if (!file.exists(temp)) {
    save(D_norm, file=temp)
  }
  load(temp)
}
rm(temp)
rm(Q1_norm)
rm(Q2_norm)
rm(Q3_norm)
rm(Q4_norm)
rm(Q5_norm)
rm(Q6_norm)
rm(Q7_norm)
rm(Q8_norm)
rm(Q9_norm)
rm(Q10_norm)
rm(Q11_norm)
rm(Q12_norm)
rm(Q13_norm)
rm(Q14_norm)
rm(Q15_norm)
rm(Q16_norm)
rm(Q17_norm)
rm(Q18_norm)
rm(Q19_norm)
rm(Q20_norm)
rm(Q21_norm)
rm(Q22_norm)
rm(Q23_norm)
rm(Q24_norm)
rm(Q25_norm)
rm(Q26_norm)
rm(Q27_norm)
rm(Q28_norm)
