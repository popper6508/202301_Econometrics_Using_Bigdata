### 개별기업 재무제표 읽어내기
finance_data <- function(firm_code) {
  firm_url <- paste0("https://comp.fnguide.com/SVO2/asp/SVD_Finance.asp?pGB=1&gicode=A",firm_code)
  firm_data <- GET(firm_url, user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
  firm_data <- firm_data %>%
    read_html() %>%
    html_table()

  ##각 재무제표 초본
  data_IS <- firm_data[[1]] %>%
    as.data.frame()
  data_IS[, 1] = gsub('계산에 참여한 계정 펼치기',
                      '', data_IS[, 1])
  data_IS = data_IS[!duplicated(data_IS[, 1]), ]
  rownames(data_IS) = NULL
  rownames(data_IS) = data_IS[, 1]
  data_IS[, 1] = NULL
  data_IS <- data_IS[,1:4]
  data_IS = sapply(data_IS, function(x) {
    str_replace_all(x, ',', '') %>%
      as.numeric()
  }) %>%
    data.frame(., row.names = rownames(data_IS))


  data_BS <- firm_data[[3]] %>%
    as.data.frame()
  data_BS[, 1] = gsub('계산에 참여한 계정 펼치기',
                      '', data_BS[, 1])
  data_BS = data_BS[!duplicated(data_BS[, 1]), ]
  rownames(data_BS) = NULL
  rownames(data_BS) = data_BS[, 1]
  data_BS[, 1] = NULL
  data_BS <- data_BS[,1:4]
  data_BS = sapply(data_BS, function(x) {
    str_replace_all(x, ',', '') %>%
      as.numeric()
  }) %>%
    data.frame(., row.names = rownames(data_BS))

  data_CF <- firm_data[[5]] %>%
    as.data.frame()
  data_CF[, 1] = gsub('계산에 참여한 계정 펼치기',
                      '', data_CF[, 1])
  data_CF = data_CF[!duplicated(data_CF[, 1]), ]
  rownames(data_CF) = NULL
  rownames(data_CF) = data_CF[, 1]
  data_CF[, 1] = NULL
  data_CF <- data_CF[,1:4]
  data_CF = sapply(data_CF, function(x) {
    str_replace_all(x, ',', '') %>%
      as.numeric()
  }) %>%
    data.frame(., row.names = rownames(data_CF))

  fi_all_tem <- rbind(data_CF, data_BS, data_IS)
  return(fi_all_tem)
}



finance_ratio <- function(firm_name) {
  firm_code = firmdata[firmdata['종목명']==firm_name, '종목코드']
   firm_url <- paste0("https://comp.fnguide.com/SVO2/ASP/SVD_FinanceRatio.asp?pGB=1&gicode=A",firm_code,"&cID=&MenuYn=Y&ReportGB=&NewMenuID=104&stkGb=701")
  firm_data <- GET(firm_url, user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
  firm_data <- firm_data %>%
    read_html() %>%
    html_table()
 
}

firm_url <- paste0("https://comp.fnguide.com/SVO2/ASP/SVD_FinanceRatio.asp?pGB=1&gicode=A005930&cID=&MenuYn=Y&ReportGB=&NewMenuID=104&stkGb=701")
firm_data <- GET(firm_url, user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
firm_data <- firm_data %>%
    read_html() %>%
    html_table()

firm_data[[1]]

firm_data[[1]][, 1] = gsub('계산에 참여한 계정 펼치기',
                      '', firm_data[[1]][, 1])
head(as.data.frame(firm_data[[1]]))

as.data.frame(firm_data[[1]])
