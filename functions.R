############ for table 1 and 2 ##############
# by: sort by
# for_n: to count syllabus (divided by 5), and calculate percentage
# num: the number with mean and SD (year)
get_data_features_by <- function(data, by, for_n, num){
# completed syllabus
n_syl <- sapply(levels(unlist(data[,by])),
                function(i){
                  sum(!(is.na(data[data[,by] == i, ][,for_n])))/5}) # divided by 5 because each syllabus should contain 5 spaces fitted
p_syl <- format(round((n_syl/8)*100,2), nsmall = 2) # 8 since 8 courses were selected for each university
# year (median and MAD)
m_y_syl <- sapply(levels(unlist(data[,by])),
                  function(i){
                    yrs <- unlist(data[data[,by] == i, ][,num])
                    median(yrs, na.rm = T)
                  })
sd_y_syl <- sapply(levels(unlist(data[,by])),
                   function(i){
                     yrs <- unlist(data[data[,by] == i, ][,num])
                     mad(yrs, na.rm = T)
                   })
y_syl <- paste(round(m_y_syl, 2), "±", round(sd_y_syl, 2))
# final data of syllabus
syl_data <- data.frame("n" = n_syl, "percentage" = p_syl, "num" = y_syl)
syl_data <- cbind("by" = rownames(syl_data), syl_data)
syl_data <- syl_data[order(syl_data$n, decreasing = T),]
rownames(syl_data) <- NULL
syl_data
}

########### for table 3 #################

wilcox_compare_by <- function(data, by, num1, num2, num0){
# first only set the median for each "by" (num1 and num2)
m_num0 <- sapply(levels(unlist(data[,by])),
                 function(i){
                   yrs <- unlist(data[data[,by] == i, ][,num0])
                   median(yrs, na.rm = T)
                 })
m_num1 <- sapply(levels(unlist(data[,by])),
         function(i){
           yrs <- unlist(data[data[,by] == i, ][,num1])
           median(yrs, na.rm = T)
         })
m_num2 <- sapply(levels(unlist(data[,by])),
         function(i){
           yrs <- unlist(data[data[,by] == i, ][,num2])
           mean(yrs, na.rm = T)
         })
# now, the wilcoxon test
CIs <- sapply(levels(unlist(data[,by])),
         function(i){
            n1 <- unlist(data[data[,by] == i, num1])
            n2 <- unlist(data[data[,by] == i, num2])
            wilcox.test(n2, n1, conf.int=T)$conf.int # wilcox function with first n2
            })
ps <- sapply(levels(unlist(data[,by])),
         function(i){
           n1 <- unlist(data[data[,by] == i, num1])
           n2 <- unlist(data[data[,by] == i, num2])
           wilcox.test(n2, n1, conf.int=T)$p.value # wilcox function with first n2
         })
book_data <- cbind(t(CIs),ps)
book_data <- cbind(m_num2, book_data)
book_data <- cbind(m_num1, book_data)
book_data <- cbind(m_num0, book_data)
book_data <- cbind(rownames(book_data), book_data)
rownames(book_data) <- NULL
book_data <- cbind(book_data[,1], round(as.data.frame(apply(book_data, 2, as.numeric))[,2:7], 2))
colnames(book_data) <- c("by", "n0", "n1", "n2", "CI.lower", "CI.upper", "p.value")
book_data
}

