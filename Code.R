#1. THU THAP VA XU LY DU LIEU
#Lưu ý: Code có rất nhiều vòng lặp, sử dụng try để thử trong vòng lặp để bắt model bị lỗi
#khi chạy thì đợi vòng lặp chạy xong ở console hiện ">" thì mới chạy code tiếp để tránh bị ghi đè
library(quantmod)
library(readxl)
library(zoo)
setwd(getwd())
# 4/1/2022 - 16/10/2026
df <- read.csv.zoo("stocks_close_volume_2022_2026.csv", index.column = "time", format = "%m/%d/%Y")
colSums(is.na(df))

#Kiểm tra dữ liệu
colSums(is.na(df))
sum(which(diff(as.numeric(df[,1:5])) == 0 & diff(as.numeric(df[, 6:10])) == 0))
sum(df[, 6:10] == 0)
prices <- df[,1:5]
volume <- df[, 6:10]
names(prices)
names(volume)
nrow(df) #Tổng cộng 1191 phiên có giao dịch 
colSums(diff(log(prices)) == 0, na.rm = TRUE) #số ngày lợi suất=0 của từng mã, phải rất thấp
sum(rowSums(diff(prices) == 0, na.rm = TRUE) == 5) #số ngày cả 5 mã đứng yên

head(prices)
tail(prices)
df_diff <- diff(df)
df_diff
sum(is.na(df_diff))
# kiểm tra lại số cặp ngày trùng 
tickers <- c("ACB", "FPT", "HPG", "MSN", "VHM")
for (tk in tickers) {
  close_col <- paste0(tk, "_close")
  vol_col <- paste0(tk, "_volume")
  # Tìm các ngày mà cả chênh lệch giá và chênh lệch volume đều bằng 0
  trung_nhau <- df_diff[, close_col] == 0 & df_diff[, vol_col] == 0
  # Lọc ra các ngày thỏa mãn, index lúc này là ngày của ngày hôm sau
  cac_ngay <- index(df_diff)[which(trung_nhau)]
  if (length(cac_ngay) > 0) {
    cat("Mã", tk, "bị trùng giá & vol tại các ngày (ngày hôm sau):", as.character(cac_ngay), "\n")
  } else {
    cat("Mã", tk, "-> KHÔNG có ngày nào bị trùng.\n")
  }
}

#2. CHUẨN BỊ DỮ LIỆU ĐỂ XÂY MÔ HÌNH ARIMA
#cài các package để dự báo theo chuỗi thời gian
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")
library(forecast)
library(tseries)
#Với phương pháp Box-Jenkins (n>=50), ta cần kiểm tra dữ liệu xem có đủ 50 mẫu trở lên không
stopifnot(
  nrow(prices) >= 50,
  all(is.finite(as.numeric(prices))),
  all(as.numeric(prices) > 0)
)

#Kiểm tra cho thấy cổ phiếu danh mục đều đủ số quan sát để xây mô hình ARIMA
#Tính log giá 
log_prices <- log(prices)
log_prices
colnames(log_prices) <- c("ACB_log_price", "FPT_log_price","HPG_log_price",
                          "MSN_log_price","VHM_log_price")
str(log_prices)
summary(log_prices)
View(log_prices)
#phân rã dữ liệu để kiểm tra tính mùa vụ theo tuần
ACB_ts <- ts(diff(log_prices$ACB_log_price), frequency = 5)
plot(decompose(ACB_ts))
mtext("ACB", side=3, line=1, font=2, cex=1.3)

HPG_ts <- ts(diff(log_prices$HPG_log_price), frequency = 5)
plot(decompose(HPG_ts))
mtext("HPG", side=3, line=1, font=2, cex=1.3)

FPT_ts <- ts(diff(log_prices$FPT_log_price), frequency = 5)
plot(decompose(FPT_ts))
mtext("FPT", side=3, line=1, font=2, cex=1.3)

VHM_ts <- ts(diff(log_prices$VHM_log_price), frequency = 5)
plot(decompose(VHM_ts))
mtext("VHM", side=3, line=1, font=2, cex=1.3)

MSN_ts <- ts(diff(log_prices$MSN_log_price), frequency = 5)
plot(decompose(MSN_ts))
mtext("MSN", side=3, line=1, font=2, cex=1.3)
#Các đồ thị cho thấy không có mùa vụ theo tuần đáng kể ở cả 5 mã. 
#Xem độ mạnh mùa vụ và xu hướng nếu gần 0 = yếu, gần 1 = mạnh
fs_ACB <- max(0, 1 - var(decompose(ACB_ts)$random, na.rm=TRUE) / 
                var(decompose(ACB_ts)$seasonal + decompose(ACB_ts)$random, na.rm=TRUE))
fs_ACB
ft_ACB <- max(0, 1 - var(decompose(ACB_ts)$random, na.rm=TRUE) /
            var(decompose(ACB_ts)$trend + decompose(ACB_ts)$random, na.rm=TRUE))
ft_ACB
#Trend mạnh hơn seasonal 
nsdiffs(ACB_ts) #=0 nghĩa là không cần sai phân mùa vụ

#HPG
fs_HPG <- max(0, 1 - var(decompose(HPG_ts)$random, na.rm=TRUE) / 
                var(decompose(HPG_ts)$seasonal + decompose(HPG_ts)$random, na.rm=TRUE))
fs_HPG
ft_HPG <- max(0, 1 - var(decompose(HPG_ts)$random, na.rm=TRUE) /
                var(decompose(HPG_ts)$trend + decompose(HPG_ts)$random, na.rm=TRUE))
ft_HPG
#Trend mạnh hơn seasonal 
nsdiffs(HPG_ts) #=0 nghĩa là không cần sai phân mùa vụ

#FPT
fs_FPT <- max(0, 1 - var(decompose(FPT_ts)$random, na.rm=TRUE) / 
                var(decompose(FPT_ts)$seasonal + decompose(FPT_ts)$random, na.rm=TRUE))
fs_FPT
ft_FPT <- max(0, 1 - var(decompose(FPT_ts)$random, na.rm=TRUE) /
                var(decompose(FPT_ts)$trend + decompose(FPT_ts)$random, na.rm=TRUE))
ft_FPT
#Trend mạnh hơn seasonal
nsdiffs(FPT_ts) #=0 nghĩa là không cần sai phân mùa vụ

#VHM
fs_VHM <- max(0, 1 - var(decompose(VHM_ts)$random, na.rm=TRUE) / 
                var(decompose(VHM_ts)$seasonal + decompose(VHM_ts)$random, na.rm=TRUE))
fs_VHM
ft_VHM <- max(0, 1 - var(decompose(VHM_ts)$random, na.rm=TRUE) /
                var(decompose(VHM_ts)$trend + decompose(VHM_ts)$random, na.rm=TRUE))
ft_VHM
#Trend mạnh hơn seasonal 
nsdiffs(VHM_ts) #=0 nghĩa là không cần sai phân mùa vụ

#MSN
fs_MSN <- max(0, 1 - var(decompose(MSN_ts)$random, na.rm=TRUE) / 
                var(decompose(MSN_ts)$seasonal + decompose(MSN_ts)$random, na.rm=TRUE))
fs_MSN
ft_MSN <- max(0, 1 - var(decompose(MSN_ts)$random, na.rm=TRUE) /
                var(decompose(MSN_ts)$trend + decompose(MSN_ts)$random, na.rm=TRUE))
ft_MSN
#Trend mạnh hơn seasonal 
nsdiffs(MSN_ts) #=0 nghĩa là không cần sai phân mùa vụ

#3. XÂY DỰNG MÔ HÌNH ARIMA THEO PHƯƠNG PHÁP BOX-JENKINS
N <- nrow(log_prices)
N
df_train <- log_prices[1:1091, ]
df_test  <- log_prices[1092:N, ]
nrow(df_train) 
nrow(df_test)     # 1041 và 100
index(df_train)[c(1, nrow(df_train))] #ngày đầu và ngày cuối của train
index(df_test)[c(1, nrow(df_test))] #ngày đầu và ngày cuối của test
#B1: Định dạng mô hình
#Với giả thuyết H0: chuỗi không dừng và H1: chuỗi dừng 
adf.test(ts(df_train$ACB_log_price), alternative = "stationary")
adf.test(ts(df_train$HPG_log_price), alternative = "stationary")
adf.test(ts(df_train$FPT_log_price), alternative = "stationary")
adf.test(ts(df_train$VHM_log_price), alternative = "stationary")
adf.test(ts(df_train$MSN_log_price), alternative = "stationary")
#Sau khi kiểm tra thì p-value của cả 5 mã đều > 0.05 vì vậy ta sẽ không bác bỏ H0 
#Tiếp tục lấy sai phân dữ liệu bắt đầu từ bậc 1 cho đến kh chuyển sang chuỗi dừng
adf.test(ts(diff(df_train$ACB_log_price)), alternative = "stationary")
adf.test(ts(diff(df_train$HPG_log_price)), alternative = "stationary")
adf.test(ts(diff(df_train$FPT_log_price)), alternative = "stationary")
adf.test(ts(diff(df_train$VHM_log_price)), alternative = "stationary")
adf.test(ts(diff(df_train$MSN_log_price)), alternative = "stationary")
#Sau khi lấy sai phân bậc 1 ta thấy cả 5 mã đều chuyển sang chuỗi dừng
#Ta kết luận được d=1
d_ACB <- ts(diff(df_train$ACB_log_price))
d_HPG <- ts(diff(df_train$HPG_log_price))
d_FPT <- ts(diff(df_train$FPT_log_price))
d_VHM <- ts(diff(df_train$VHM_log_price))
d_MSN <- ts(diff(df_train$MSN_log_price))
#Tiếp theo ta đi xác định p và q (based on ACF/PACF hoặc tự động)
#PACF
pacf(d_ACB, lag.max = 20) #2, 12, 15
pacf(d_HPG, lag.max = 20) #1, 11, 12
pacf(d_FPT, lag.max = 20) #2, 8
pacf(d_VHM, lag.max = 20) #1, 6
pacf(d_MSN, lag.max = 20) #1, 3, 19
#Vay thu nang len p0-p8
#Cách lựa chọn p thì ta chọn những đường vượt ra khỏi vùng nét đứt báo hiệu PACF khác 0
#ACF
acf(d_ACB, lag.max = 20) #2, 12, 15
acf(d_HPG, lag.max = 20) #1, 11 
acf(d_FPT, lag.max = 20) #2, 8 
acf(d_VHM, lag.max = 20) #1, 6
acf(d_MSN, lag.max = 20) #1, 3
#vay lay q tu 0-8
#Sau khi nhìn vào đồ thị, ta thấy có nhiều điểm vượt ngưỡng sọc xanh (có ý nghĩa)
#Vì vậy nên nhóm sử dụng bổ trợ kiểm tay và auto.arima để ước lượng mô hình tìm điểm p,d,q tối ưu
#Đồng thời thực hiện thẩm định mô hình sau mỗi bước ước lượng cho từng mô hình của mỗi mã cổ phiếu
help(Arima) #xem cách dùng hàm Arima của gói forecast 
help(try)
View(df_train)
#Tự xây mô hình
str(df_train)
names(df_train)
result <- data.frame()
str(ts(as.numeric(df_train$ACB_log_price), frequency = 1))
#Lặp qua từng mã sau đó ghi kết quả vào data frame vừa tạo 
for (colname in names(df_train)) {
  y <- ts(as.numeric(df_train[, colname]), frequency = 1)
  for (p in 0:8) {
    for (q in 0:8) {
      cat(colname, "- p =", p, ", q =", q, "\n") #in ra dòng theo dõi tiến độ
      model <- try(Arima(y,order = c(p, 1, q)),silent=TRUE)
      #Nếu mô hình gặp lỗi thì bỏ qua và chạy tiếp
      if (class(model)[1] == "try-error") {
        next
      } else {
        result <- rbind(result, data.frame(
          ticker = colname,
          p = p,
          q = q,
          AIC = model$aic,
          AICc = model$aicc,
          BIC = model$bic
        ))
      }
    }
  }
}
result
str(result)
View(result)
result$p_value <- NA 
str(result)
View(result)
help(Box.test)
#Chạy mô hình qua từng dòng để xem p-value của phần dư
#Thống nhất quy tắc chọn lag - fitdf >= 10
nrow(result)
for (n in 1:nrow(result)) {
  cat("Dong",n,"/",nrow(result),"\n") #theo doi code chay
  dong <- ts(as.numeric(df_train[,result$ticker[n]])) #Lấy từng dòng theo từng tên mã cổ phiếu      
  model <- try(Arima(dong,order=c(result$p[n],1,result$q[n])), silent=TRUE)
  if (class(model)[1] == "try-error") {
    next
  } else {
    p_value <- Box.test(residuals(model),lag= result$p[n]+result$q[n]+10, 
                        type= c("Ljung-Box"), 
                        fitdf= result$p[n]+result$q[n])$p.value
    result$p_value[n] <- p_value
  }
}
summary(result)
colSums(is.na(result))
result
str(result)
str(result[result$p_value >= 0.1,]) #Có 140 model có p-value residuals >0.1
#Tách ra từng bảng cho 5 mã
result_ticker <- split(result, result$ticker)
str(result_ticker)
#Xem từng bảng, mỗi mã đều có chung 81 dòng 

#Bắt đầu chọn mô hình tốt nhất 
#dùng hàm for để lặp lại quy trình cho 5 mã 
best_model <- data.frame()
str(result_ticker) #list có 5 bảng với 5 tên mã cổ phiếu 
names(result_ticker)
for (colname in names(result_ticker)) {
  tb <- result_ticker[[colname]]
  tb <- tb[!(tb$p == 0 & tb$q==0),]
  if (nrow(tb) == 0) {
    cat(colname, ": khong co mo hinh nao qua Ljung-Box\n")
    next
  }
  tb$k <- tb$p + tb$q
  #theo BIC
  ds_d1 <- tb[order(tb$k, tb$BIC), ]
  d1 <- ds_d1[which.min(ds_d1$BIC), ]
  d1$tieu_chi <- "BIC"
  #theo AICc
  ds_d2 <- tb[order(tb$k, tb$AICc), ]
  d2 <- ds_d2[which.min(ds_d2$AICc), ]
  d2$tieu_chi <- "AICc"
  #theo AIC
  ds_d3 <- tb[order(tb$k, tb$AIC), ]
  d3 <- ds_d3[which.min(ds_d3$AIC), ]
  d3$tieu_chi <- "AIC"
  
  best_model <- rbind(best_model, d1, d2, d3)
}
best_model
#write.csv(best_model, "best_model.csv")
str(best_model)
#Lọc ra những model có chung bậc tham số trong 1 mã
library(dplyr)
best_model <- best_model %>% group_by(ticker, p, q) %>%
  summarise(
    AIC      = first(AIC),
    AICc     = first(AICc),
    BIC      = first(BIC),
    p_value  = first(p_value),
    k        = first(k),
    tieu_chi = paste(unique(tieu_chi), collapse = ", "),
    .groups  = "drop"
  )
str(best_model)
best_model
nrow(best_model)
#Lọc bỏ model có p-value LB <0.05
best_model <- best_model[!best_model$p_value < 0.05, ]
best_model
auto_model <- data.frame()
for (tk in unique(best_model$ticker)) {
  y <- ts(as.numeric(df_train[, tk]))
  fa <- auto.arima(y, d = 1, seasonal = FALSE, allowdrift = FALSE,
                   stepwise = FALSE, approximation = FALSE)
  ord <- arimaorder(fa)
  cat(tk, ": auto.arima chon", paste(arimaorder(fa), collapse = ","), "\n")
  auto_model <- rbind(auto_model, data.frame(
                      ticker = tk,
                      "p,d,q" = paste(ord, collapse = ","),
                      AIC = fa$aic,
                      AICc = fa$aicc,
                      BIC = fa$bic,
                      k = length(coef(fa)),
                      tieu_chi = "auto.arima")
  )
}
auto_model #auto cũng trả về model giống ở 2 mã so với model nhóm lọc
str(auto_model)
View(auto_model)
#Lấy mô hình gốc là random walk (0,1,0)
#chạy rolling forecast để so RSME, MAE
help("forecast.Arima")
help("Arima")
help("auto.arima")
help("arimaorder")
sai_so_tong_hop <- data.frame()
for (x in 1:nrow(best_model)) {
  ticker <- best_model$ticker[x]
  p_best <- best_model$p[x]
  q_best <- best_model$q[x]
  train <- ts(as.numeric(df_train[,ticker]))
  test <- ts(as.numeric(df_test[,ticker]))
  
  #3 model để so sánh là model chọn và model random walk và auto của arima 
  model_chon  <- Arima(train, order = c(p_best, 1, q_best))
  model_010 <- Arima(train, order = c(0, 1, 0))
  model_auto <- auto.arima(train, d = 1, seasonal = FALSE, allowdrift = FALSE,
                           stepwise = FALSE, approximation = FALSE)
  order_auto <- arimaorder(model_auto)   # (p, d, q) mà auto.arima chọn
  p_auto <- order_auto[1]
  d_auto <- order_auto[2]
  q_auto <- order_auto[3]
  
  # Chỉ lưu 1 mô hình ở lần gặp đầu tiên của mỗi mã
  them_model <- (x == match(ticker, best_model$ticker))
  # Auto trùng Random Walk ARIMA(0,1,0)
  trung_random_walk <- (p_auto == 0 && d_auto == 1 && q_auto == 0)
  # Auto trùng mô hình tự chọn
  #Các mô hình tự chọn đều có d = 1, đếm số điều kiện đạt >0 là mô hình auto khác 
  trung_tu_chon <- sum(best_model$ticker == ticker & best_model$p == p_auto &
                        best_model$q == q_auto) > 0
  # Chỉ lấy Auto nếu khác cả Random Walk và mọi mô hình tự chọn
  them_auto <- (them_model && !trung_random_walk && !trung_tu_chon)
  
  # p-value Ljung-Box của 3 mô hình (lag=fitdf+10)
  LB_model_chon <- Box.test(residuals(model_chon),
                            lag=p_best+q_best+10 , type = "Ljung-Box",
                            fitdf = p_best + q_best)$p.value
  LB_model_010  <- Box.test(residuals(model_010),
                            lag =10, type = "Ljung-Box",
                            fitdf = 0)$p.value
  LB_model_auto <- Box.test(residuals(model_auto), 
                            lag =order_auto[1] + order_auto[3] +10, type = "Ljung-Box",
                            fitdf = order_auto[1] + order_auto[3])$p.value
  #Tạo vector để chứa sai số khi dự báo
  chenh_lech_model_chon <- c()
  chenh_lech_model_010 <- c()
  chenh_lech_model_auto <- c()
  cat("Bat dau chay", ticker,"Model:", paste0(p_best, ",1,", q_best), "\n")
  #Lặp lại mỗi lần 10 phiên cho đến phiên 91 thì dự báo hết số phiên còn lạ và dừng(rolling forecast)
  for (i in 1:(length(test)-10+1)) {
    if (i>1) {
      ls <- c(train, test[1:(i-1)]) #lịch sử dự báo
    } else {
      ls <- train
    }
    #Lấy sai số trung bình của phiên tiếp theo để dự báo 50 lần
    forecast_model_chon <- as.numeric(forecast(Arima(ls, model=model_chon), h=10)$mean)
    forecast_model_010 <- as.numeric(forecast(Arima(ls,model=model_010), h=10)$mean)
    forecast_model_auto <- as.numeric(forecast(Arima(ls,model=model_auto), h=10)$mean)
    
    h_real <- test[i:(i+9)] #Xem 10 phiên dự báo trong thực tế từ phiên i đến i+9 
    fc_date <- as.Date(index(df_test)[i:(i+9)]) #Ngày dự báo 
  
    #Lưu kết quả theo từng phiên 
    sai_so_h <- data.frame(
      ticker = ticker,
      value = paste0(p_best, ",1,", q_best),
      model = "Tự chọn",
      phien = i,
      Ngay_du_bao = fc_date,
      So_phien = 1:10,
      thuc_te = h_real,
      du_bao = forecast_model_chon)
    #Lưu mô hình random walk 1 lân cho mỗi mã thôi
    if (them_model) {
      sai_so_rw <- data.frame(
        ticker = ticker,
        value = "0,1,0",
        model = "Random Walk",
        phien = i,
        Ngay_du_bao = fc_date,
        So_phien = 1:10,
        thuc_te = h_real,
        du_bao = forecast_model_010)
      sai_so_h <- rbind(
        sai_so_h,
        sai_so_rw
      )
    }
    #Lưu 1 mô hình auto nếu nó không trùng 
    if (them_auto) {
      sai_so_auto <- data.frame(
        ticker = ticker,
        value = paste(order_auto, collapse = ","),
        model = "Auto",
        phien = i,
        Ngay_du_bao = fc_date,
        So_phien = 1:10,
        thuc_te = h_real,
        du_bao = forecast_model_auto)
      sai_so_h <- rbind(
        sai_so_h,
        sai_so_auto
        )
    }
    #Tính sai số cho từng phiên dự báo 
    sai_so_h$error <- sai_so_h$thuc_te - sai_so_h$du_bao
    #Nối vào bảng lưu tổng
    sai_so_tong_hop <- rbind(sai_so_tong_hop, sai_so_h)
  }
  cat("Chay xong", ticker, paste0(p_best, ",1,", q_best),"\n" )
}
#Dự báo 91 lần với 10 phiên thì tổng quan sát phải là 910
table(sai_so_tong_hop$ticker, sai_so_tong_hop$model, sai_so_tong_hop$value)
sai_so_tong_hop <- unique(sai_so_tong_hop)
table(sai_so_tong_hop$ticker, sai_so_tong_hop$model, sai_so_tong_hop$value)
colSums(is.na(sai_so_tong_hop))
str(sai_so_tong_hop)
View(sai_so_tong_hop)
# write.csv(sai_so_tong_hop, "sai_so_tong_hop.csv")
#Xem sai số RMSE trung bình và MAE trung bình sẽ thế nào
ss <- sai_so_tong_hop
ss$se <- ss$error^2
ss$ae <- abs(ss$error)
kq <- aggregate(cbind(se, ae) ~ ticker + model + value, data = ss, FUN = mean)
kq$RMSE <- sqrt(kq$se)
kq$MAE  <- kq$ae
#Lấy random walk làm đối chiếu luôn bằng 1
rw <- kq[kq$model == "Random Walk", c("ticker", "RMSE", "MAE")]
names(rw)[2:3] <- c("RMSE_rw", "MAE_rw")
kq <- merge(kq, rw, by = "ticker")
kq$RMSE_tuong_doi <- kq$RMSE / kq$RMSE_rw  # < 1 nghĩa là tốt hơn random walk
kq$MAE_tuong_doi  <- kq$MAE  / kq$MAE_rw
kq[order(kq$ticker, kq$RMSE_tuong_doi),
   c("ticker", "model", "value", "RMSE", "MAE", "RMSE_tuong_doi", "MAE_tuong_doi")]

#Vì có model thắng được random walk, cũng như chênh lệch thua rất thấp
#nên ta cần kiểm định ý nghĩa thống kê
#Dùng kiểm định Diebold–Mariano để xem chênh lệch RMSE và MAE có ý nghĩa thống kê không
str(ss)
horizons <- c(1:10)
dm_kq <- data.frame()
for (tk in unique(ss$ticker)) {
  d_tk <- ss[ss$ticker == tk, ]
  rw_d <- d_tk[d_tk$model == "Random Walk", ]
  # danh sách các mô hình khác RW của mã này
  ds_mh <- unique(d_tk[d_tk$model != "Random Walk", c("model", "value")])
  
  for (i in 1:nrow(ds_mh)) {
    md <- d_tk[d_tk$model == ds_mh$model[i] & d_tk$value == ds_mh$value[i], ]
    for (k in horizons) {
      rw_k <- rw_d[rw_d$So_phien == k, ]
      md_k <- md[md$So_phien == k, ]
      #ghép cặp theo từng phiên, sắp xếp theo thứ tự thời gian
      chung <- sort(intersect(rw_k$phien, md_k$phien))
      e_rw <- rw_k$error[match(chung, rw_k$phien)]
      e_m <- md_k$error[match(chung, md_k$phien)]
      #mũ 2 cho RMSE, mũ 1 cho MAE
      #Ước lượng Bartlett dùng trọng số giảm dần theo lag nên phương sai luôn không âm
      d2 <- try(dm.test(e_rw, e_m, h=k, power=2, alternative="two.sided",
                        varestimator = "bartlett"), silent = TRUE)
      d1 <- try(dm.test(e_rw, e_m, h=k, power=1, alternative = "two.sided",
                        varestimator = "bartlett"), silent = TRUE)
      ok2 <- !inherits(d2, "try-error")
      ok1 <- !inherits(d1, "try-error")
      
      dm_kq <- rbind(dm_kq, data.frame(
        ticker = tk,
        model = ds_mh$model[i],
        value = ds_mh$value[i],
        buoc = k,
        n = length(chung),
        DM_RMSE = if (ok2) unname(d2$statistic) else NA,
        p_RMSE = if (ok2) d2$p.value else NA,
        DM_MAE = if (ok1) unname(d1$statistic) else NA,
        p_MAE = if (ok1) d1$p.value else NA
      ))
    }
  }
}

#hiệu chỉnh đa so sánh
dm_kq$p_RMSE_holm <- p.adjust(dm_kq$p_RMSE, method = "holm")
dm_kq$p_MAE_holm <- p.adjust(dm_kq$p_MAE,  method = "holm")
#kết luận
dm_kq$ket_luan_RMSE <- ifelse(dm_kq$p_RMSE < 0.05,
                              ifelse(dm_kq$DM_RMSE > 0, "Tốt hơn RW", "Kém hơn RW"),
                              "Không khác biệt")
dm_kq$ket_luan_MAE <- ifelse(dm_kq$p_MAE < 0.05,
                             ifelse(dm_kq$DM_MAE > 0, "Tốt hơn RW", "Kém hơn RW"),
                             "Không khác biệt")
colSums(is.na(dm_kq))
str(dm_kq)
View(dm_kq[, c(1, 3, 4, 6:9, 12:13 )])
library(readr)
write_excel_csv(dm_kq, "dm_kq.csv")
#write_excel_csv(kq, "kq.csv")
View(dm_kq[dm_kq$ket_luan_RMSE != "Không khác biệt" | dm_kq$ket_luan_MAE  != "Không khác biệt",])
#Với mục tiêu là dự báo, nhóm quyết định lựa chọn cho các mô hình sau làm ứng viên từ kq RMSE,MAE:
#ACB: (0,1,1); HPG: (0,1,1); FPT (2,1,3); VHM: (0,1,1); MSN(3,1,0)

#Tạo model ARIMA trên dữ liệu train xem ARCH
library(lmtest)
library(FinTS)
#model ACB (0,1,1)
plot(df_train$ACB_log_price, main = "ACB: Log Price")
plot(diff(df_train$ACB_log_price), main = "ACB: Log Return")
ts_ACB <- ts(as.numeric(df_train$ACB_log_price, frequency = 1))
model_ACB <- Arima(ts_ACB, order=c(0,1,1))
summary(model_ACB)
coeftest(model_ACB)
#Thực hiện thẩm định tự tương quan, phân phối chuẩn
checkresiduals(model_ACB, lag=11) # với Ljung-Box test p-value>0.05 ở lag 11 
Box.test(residuals(model_ACB)^2, lag = 11, fitdf = 1, type = "Ljung-Box")
ArchTest(residuals(model_ACB), lag=10) #Với mục tiêu dự báo cho 10 phiên thì chọn lag=10 
#Khi kiểm định ArchTest thì p-value < 0.05 cho thấy model vẫn còn hiệu ứng ARCH
#Ta phải chuyển đổi sang GARCH để loại bỏ hạn chế của ARCH

#model HPG (0,1,1)
plot(df_train$HPG_log_price, main = "HPG: Log Price")
plot(diff(df_train$HPG_log_price), main = "HPG: Log Return")
ts_HPG <- ts(as.numeric(df_train$HPG_log_price, frequency = 1))
model_HPG <- Arima(ts_HPG, order=c(0,1,1))
coeftest(model_HPG)
checkresiduals(model_HPG,lag=11) 
Box.test(residuals(model_HPG)^2, lag = 11, fitdf =1,  type = "Ljung-Box")
ArchTest(residuals(model_HPG), lag=10)
#Khi kiểm định ArchTest thì p-value < 0.05 cho thấy model vẫn còn hiệu ứng ARCH
#Ta phải chuyển đổi sang GARCH để loại bỏ hạn chế của ARCH

#model FPT (2,1,3)
plot(df_train$FPT_log_price, main = "FPT: Log Price")
plot(diff(df_train$FPT_log_price), main = "FPT: Log Return")
ts_FPT <- ts(as.numeric(df_train$FPT_log_price, frequency = 1))
model_FPT <- Arima(ts_FPT, order = c(2,1,3))
summary(model_FPT)
coeftest(model_FPT)
checkresiduals(model_FPT, lag=15)  
Box.test(residuals(model_FPT)^2, lag = 15, fitdf=5, type = "Ljung-Box")
ArchTest(residuals(model_FPT), lag=10)
#Khi kiểm định ArchTest thì p-value < 0.05 cho thấy model vẫn còn hiệu ứng ARCH
#Ta phải chuyển đổi sang GARCH để loại bỏ hạn chế của ARCH

#model VHM (0,1,1)
plot(df_train$VHM_log_price, main = "VHM: Log Price")
plot(diff(df_train$VHM_log_price), main = "VHM: Log Return")
ts_VHM <- ts(as.numeric(df_train$VHM_log_price, frequency = 1))
model_VHM <- Arima(ts_VHM, order=c(0,1,1))
summary(model_VHM)
coeftest(model_VHM)
checkresiduals(model_VHM, lag=11)
Box.test(residuals(model_VHM)^2, lag = 11, fitdf = 1, type = "Ljung-Box")
ArchTest(residuals(model_VHM), lag=10)
#Khi kiểm định ArchTest thì p-value < 0.05 cho thấy model vẫn còn hiệu ứng ARCH
#Ta phải chuyển đổi sang GARCH để loại bỏ hạn chế của ARCH

#model MSN (3,1,0)
plot(df_train$MSN_log_price, main = "MSN: Log Price")
plot(diff(df_train$MSN_log_price), main = "MSN: Log Return")
ts_MSN <- ts(as.numeric(df_train$MSN_log_price, frequency = 1))
model_MSN <- Arima(ts_MSN,order = c(3,1,0))
coeftest(model_MSN)
checkresiduals(model_MSN, lag=13)
Box.test(residuals(model_MSN)^2, lag = 13, fitdf=3, type = "Ljung-Box")
ArchTest(residuals(model_MSN), lag=10)
#Khi kiểm định ArchTest thì p-value < 0.05 cho thấy model vẫn còn hiệu ứng ARCH
#Ta phải chuyển đổi sang GARCH để loại bỏ hạn chế của ARCH

#Sau khi kiểm tra thì cả 5 mô hình đều cần phải chuyển sang GARCH để loại bỏ hạn chế của ARCH
#Nếu kiểm định các mean_model đã chọn thất bại, chuyển sang phương án quét các mô hình nhỏ
#Thống nhất giữ các mô hình nhỏ: (1,1), (1,2), (2,1), (2,2)
#TRƯỚC ĐÓ SẼ THỬ MÔ HÌNH (1,1) LÀM ĐIỂM SO SÁNH TRƯỚC 
if (!require(rugarch)) install.packages("rugarch")
library(rugarch)
help(ugarchspec)

#Kiểm định cho thấy trung bình log_return không khác 0
#ACB GARCH với phần dư chuẩn hóa (Standard residuals - sstd)
ret_ACB <- diff(ts_ACB)
str(ret_ACB)
t.test(ret_ACB) #kiểm tra sự khác biệt về trung bình
#Với ACB 
spec_ACB <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
#fit GARCH sẽ thử lần lượt 3 thuật toán tối ưu ("solnp", "hybrid", "nlminb") để tìm tham số 
fit_garch_ACB <- ugarchfit(data = ret_ACB ,spec = spec_ACB, solver = "hybrid")
fit_garch_ACB
# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_ACB@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_ACB) #Vì persistence gần 1 nên triển khai kiểm định thêm alpha
coef(fit_garch_ACB)["alpha1"]
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_ACB)),0) #Quá dài
#Xem sai so chuan
fit_garch_ACB@fit$robust.matcoef 
#Ở đây ta thấy rõ ma1 và alpha1, omega đều không có ý nghĩa thống kê 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_ACB)
#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_ACB <- residuals(fit_garch_ACB, standardize = TRUE)
#Thống nhất lag=20 cho 1 model
Box.test(z_ACB, lag = 20, type = "Ljung-Box", fitdf = 4)
Box.test(z_ACB^2, lag = 20, type = "Ljung-Box", fitdf = 2) #lấy tham số của GARCH
ArchTest(z_ACB, lags=10) #p-value đạt , ARCH không còn,
Acf(z_ACB) # khong co lag vượt ngưỡng  
#Vậy ta cần xem lại các model bắt đầu khác với ACB


#HPG GARCH tuong tu
ret_HPG <- diff(ts_HPG)
t.test(ret_HPG) 
#Voi HPG (0,1,1)
spec_HPG <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_HPG <- ugarchfit(data=ret_HPG, spec = spec_HPG, solver = "hybrid")
fit_garch_HPG

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_HPG@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_HPG) #Vì persistence gần 1 nên triển khai kiểm định thêm alpha
coef(fit_garch_HPG)["alpha1"]
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_HPG)),0) #Hợp lý với trung hạn-ngắn hạn
#Xem sai so chuan
fit_garch_HPG@fit$robust.matcoef 
#Ở đây ta thấy rõ ma1 và alpha1, omega đều không có ý nghĩa thống kê 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_HPG)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_HPG <- residuals(fit_garch_HPG, standardize = TRUE)
Box.test(z_HPG, lag = 20, type = "Ljung-Box", fitdf = 1) #p-value<0.1 nen van con kha yeu
Box.test(z_HPG^2, lag = 20, type = "Ljung-Box", fitdf = 2)  #lấy tham số của GARCH
ArchTest(z_HPG, lags=10) #p-value đạt, ARCH không còn,
Acf(z_HPG) #Chỉ có 3 lag vượt ngưỡng nhẹ do tính ngẫu nhiên
#Vậy ta cần xem lại các model bắt đầu khác với HPG   


#FPT GARCH tuong tu
ret_FPT <- diff(ts_FPT)
t.test(ret_FPT)
#Với bậc 2,1,3 thì kết quả vẫn còn tự tương quan, sau khi thử hạ xuống thì ta thấy (1,1,1) tốt nhất  
spec_FPT <- ugarchspec(mean.model = list(armaOrder = c(2,3), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_FPT <- ugarchfit(data=ret_FPT, spec = spec_FPT, solver = "hybrid")
fit_garch_FPT

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_FPT@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_FPT) #Vì persistence 0.999 nên triển khai kiểm định thêm alpha
coef(fit_garch_HPG)["alpha1"]
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_FPT)),0) #693 phiên, biến động rất chậm
#Xem sai so chuan
fit_garch_FPT@fit$robust.matcoef #các tham số hầu như không có ý nghĩa
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_FPT)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_FPT <- residuals(fit_garch_FPT, standardize = TRUE)
Box.test(z_FPT, lag = 20, type = "Ljung-Box", fitdf = 5) #p-value <0.1 nen con yeu
Box.test(z_FPT^2, lag = 20, type = "Ljung-Box", fitdf = 2)    # lấy tham số của GARCH
ArchTest(z_FPT, lags=10) #p-value đạt, ARCH không còn, 
Acf(z_FPT) #Chỉ có 1 lag 8 cận biên và lag 29 vuot do tính ngẫu nhiên
#Vậy ta cần xem lại các model bắt đầu khác với FPT

#VHM GARCH tuong tu
ret_VHM <- diff(ts_VHM)
t.test(ret_VHM)
#Với VHM(0,1,1)
spec_VHM <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_VHM <- ugarchfit(data=ret_VHM, spec = spec_VHM, solver = "hybrid")
fit_garch_VHM

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_VHM@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_VHM) #Vì persistence 0.999 nên triển khai kiểm định thêm alpha
coef(fit_garch_VHM)["alpha1"]
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_VHM)),0) #693 phiên, biến động rất chậm
#Xem sai so chuan
fit_garch_FPT@fit$robust.matcoef #các tham số hầu như không có ý nghĩa
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_FPT)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_VHM <- residuals(fit_garch_VHM, standardize = TRUE)
Box.test(z_VHM, lag = 20, type = "Ljung-Box", fitdf = 1)
Box.test(z_VHM^2, lag = 20, type = "Ljung-Box", fitdf = 2)    # lấy tham số của GARCH
ArchTest(z_VHM, lags = 10) #p-value đạt, ARCH không còn,
Acf(z_VHM) #Lag 1 không đạt, cần xem lại
#Vậy ta cần xem lại các model bắt đầu khác với VHM


#MSN GARCH tuong tu, thu (3,1,0)
ret_MSN <- diff(ts_MSN)
t.test(ret_MSN)
#MSN (3,1,0)
spec_MSN <- ugarchspec(mean.model = list(armaOrder = c(3,0), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_MSN <- ugarchfit(data=ret_MSN, spec = spec_MSN, solver = "hybrid")
fit_garch_MSN

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_MSN@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_MSN) #Vì persistence 0.9527 nên đây là một kết quả khá ổn
coef(fit_garch_MSN)["alpha1"]
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_MSN)),0) #14 phiên 
#Xem sai so chuan
fit_garch_MSN@fit$robust.matcoef #có ar3 có ý nghĩa, ar1 và ar2 thì không 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_FPT)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_MSN <- residuals(fit_garch_MSN, standardize = TRUE)
Box.test(z_MSN, lag = 20, type = "Ljung-Box", fitdf = 3)
Box.test(z_MSN^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_MSN, lags=10) # ARCH không còn, 
Acf(z_MSN) #chỉ có 1 lag 19 vượt ngưỡng và lag 29 hơi cận biên
#Vậy ta cần xem lại các model bắt đầu khác với VHM

#Tạo vòng lặp để xem các model bắt đầu khác thế nào
result_garch <- data.frame()
#Gọi tên 2 hàm kiểm tra p-value Ljung-Box test cho tự tương quan và ARCH để dùng trong vòng lặp
weighted_lb <- getFromNamespace(".weightedBoxTest", "rugarch")
weighted_arch <- getFromNamespace(".weightedarchlmtest", "rugarch")
names(df_train)
str(names(df_train))
#Vòng lặp quét qua p và q cho ARIMA-GARCH
for (colname in names(df_train)) {
  ts_ticker <- ts(as.numeric(df_train[,colname]), frequency = 1)
  
  if (any(!is.finite(ts_ticker))) {
    stop(colname, ": dữ liệu có NA/Inf")
  }
  
  ret_ticker <- diff(ts_ticker)
  cat("Đang quét", colname, "\n")
  
  for (p in 0:5) {
    for (q in 0:5) {
      spec <- ugarchspec(
        mean.model = list(
          armaOrder = c(p, q),
          include.mean = FALSE
        ),
        variance.model = list(
          model = "sGARCH",
          garchOrder = c(1, 1)
        ),
        distribution.model = "std"
      )
      
      model <- try(
        ugarchfit(
          data = ret_ticker,
          spec = spec,
          solver = "hybrid"
        ),
        silent = TRUE
      )
      
      # Tiêu chí thứ nhất: Loại model lỗi hoặc không hội tụ
      if (inherits(model, "try-error")) next
      if (model@fit$convergence != 0) next
      
      # Tránh dừng cả vòng quét nếu bước chẩn đoán gặp lỗi
      evaluation <- try({
        #Tiêu chí thứ 2 xem hệ số hợp lý & tính dai dẳng
        cf <- coef(model)
        alpha1 <- unname(cf["alpha1"])
        pers <- as.numeric(persistence(model))
        
        # Kiểm tra sai số chuẩn thông thường và robust
        se <- model@fit$matcoef[, 2]
        se_robust <- model@fit$robust.matcoef[, 2]
        
        se_ok <- (
          length(se) == length(cf) && length(se_robust) == length(cf) &&
            all(is.finite(se)) && all(se > 0) &&
            all(is.finite(se_robust)) && all(se_robust > 0)
        )
        #Gán nhãn xem sai số chuẩn robust
        robust_tab <- model@fit$robust.matcoef
        # Kiểm tra các hệ số AR/MA có ý nghĩa thống kê không
        mean_names <- c(
          if (p > 0) paste0("ar", seq_len(p)),
          if (q > 0) paste0("ma", seq_len(q))
        )
        p_mean <- robust_tab[mean_names, "Pr(>|t|)"]
        mean_sig <- (length(mean_names) == 0 || all(is.finite(p_mean) & p_mean < 0.05))
        #Hệ số hợp lý: áp dụng cho sGARCH(1,1) - std
        coef_ok <- (
          all(is.finite(cf)) &&  #finite là kiểm tra số đó có phải số hữu hạn không
            is.finite(pers) &&
            cf["omega"] > 0 &&
            cf["alpha1"] >= 0 &&
            cf["beta1"] >= 0 &&
            pers >= 0 &&
            pers < 1 &&
            cf["shape"] > 2
        )
        
        # Tiêu chí thứ ba: thông tin AIC|BIC
        ic <- infocriteria(model)
        
        # Tiêu chí thứ tư: Phần dư chuẩn hóa
        z <- as.numeric(residuals(model, standardize = TRUE))
        
        if (any(!is.finite(z))) {
          stop("Bị NA")
        }
        
        # Weighted Ljung-Box Test on Standardized Residuals trong model fit
        wlb_z <- weighted_lb(z, p = 1, df = p + q)
        p_wlb_z <- as.numeric(wlb_z[, "p-value"])
        
        # Weighted Ljung-Box Test on Standardized Squared Residuals
        # Xử lý bình phương p=2
        wlb_z2 <- weighted_lb(z, p = 2, df = 2)
        p_wlb_z2 <- as.numeric(wlb_z2[, "p-value"])
        
        # Weighted ARCH LM Tests cho sGARCH(1,1)
        eps <- as.numeric(residuals(model))
        sig <- as.numeric(sigma(model))
        #Thay lag 3,5,7 vào để check
        p_warch3 <- weighted_arch(
          eps, sig, lags = 3, fitdf = 2
        )$p.value
        
        p_warch5 <- weighted_arch(
          eps, sig, lags = 5, fitdf = 2
        )$p.value
        
        p_warch7 <- weighted_arch(
          eps, sig, lags = 7, fitdf = 2
        )$p.value
        #Ghép lại để xem
        p_warch <- c(p_warch3, p_warch5, p_warch7)
        
        weighted_ok <- (
          all(is.finite(p_wlb_z)) &&
            all(is.finite(p_wlb_z2)) &&
            all(is.finite(p_warch)) &&
            all(p_wlb_z > 0.05) &&
            all(p_wlb_z2 > 0.05) &&
            all(p_warch > 0.05))
        
        data.frame(
          ticker = colname,
          p = p,
          q = q,
          convergence = model@fit$convergence,  #Xem chỉ số hội tụ phải ==0 
          alpha1 = alpha1,
          beta1 = unname(cf["beta1"]),  #unname để chỉ hiện số numeric thôi
          persistence = pers,  #Xem độ dai dẳng 
          coef_ok = unname(coef_ok),
          se_ok = se_ok,
          mean_sig = mean_sig,
          AIC = ic["Akaike", 1],  #Xem AIC
          BIC = ic["Bayes", 1], #Xem BIC
          HQ = ic["Hannan-Quinn", 1], #Xem Hannan-Quinn (cai nay khong quan trong lam)
          p_WLB_z_min = min(p_wlb_z), #Xem p-value nhỏ nhất trong 3 lag của phần dư chuẩn hóa
          p_WLB_z2_min = min(p_wlb_z2), #Xem p-value nhỏ nhất trong 3 lag của phần dư chuẩn hóa bình phương
          p_WARCH_min = min(p_warch), #Xem p-value nhỏ nhất của ARCH test
          weighted_ok = weighted_ok  #Phần dư chuẩn hóa và ARCH đạt không
        )
      }, silent = TRUE)
      
      if (inherits(evaluation, "try-error")) {
        cat(colname, "p =", p, "q =", q, ":", evaluation, "\n")
        next
      }
      
      result_garch <- rbind(result_garch, evaluation)
    }
  }
}
rownames(result_garch) <- NULL
warnings() #Nếu có cảnh báo thì chỉ là do hàm xử lý phức tạp thôi không sao
str(result_garch)
View(result_garch)
#Lọc kiểm tra các model đạt điều kiện hệ số và các kiểm định phân dư
result_pass <- subset(
  result_garch,
  convergence == 0 &
    coef_ok &
    se_ok &
    mean_sig &
    weighted_ok &
    is.finite(AIC) &
    is.finite(BIC) &
    is.finite(HQ)
)
str(result_pass) #Lọc hết các model yếu
View(result_pass)
table(result_pass$ticker) #Xem mỗi mã còn bao nhiêu model đạt
result_ticker_garch <- split(result_pass, result_pass$ticker)
str(result_ticker_garch)
#Lọc từng model cho từng tiêu chí khác nhau
best_garch <- data.frame()
for (colname in names(result_ticker_garch)) {
  
  tb <- result_ticker_garch[[colname]]
  tb$k <- tb$p + tb$q
  
  for (criterion in c("AIC", "BIC", "HQ")) {
    
    ranked <- tb[
      order(tb[[criterion]], tb$k),
    ]
    
    selected <- ranked[1, , drop = FALSE]
    selected$tieu_chi <- criterion
    
    best_garch <- rbind(best_garch, selected)
  }
}

str(best_garch)
View(best_garch)
#Xem cac model duoc chon boi AIC
best_garch[best_garch$tieu_chi == "AIC",c("ticker", "p","q", "persistence", "alpha1")]
#Xem cac model duoc chon boi BIC
best_garch[best_garch$tieu_chi == "BIC",c("ticker", "p","q","persistence", "alpha1")]
#Xem cac model duoc chon boi Hanna-Quinn
best_garch[best_garch$tieu_chi == "HQ", c("ticker", "p","q","persistence", "alpha1")]

#Để chắc chắn hơn, nhóm quyết định sẽ bổ sung lọc thêm các model hiện tại bằng p-value Pearson
#Lọc p-value Pearson để xem các model này sẽ fit với kiểm định phân phối std tốt không
best_garch_pearson <- best_garch
#Tạo thêm các cột Group tương ứng 20,30,40,50 trong bảng kiểm định
best_garch_pearson$p_Pearson20 <- NA
best_garch_pearson$p_Pearson30 <- NA
best_garch_pearson$p_Pearson40 <- NA
best_garch_pearson$p_Pearson50 <- NA
best_garch_pearson$pearson_ok <- FALSE

for (i in seq_len(nrow(best_garch_pearson))) {
  ticker <- best_garch_pearson$ticker[i]
  p <- best_garch_pearson$p[i]
  q <- best_garch_pearson$q[i]
  ret <- diff(ts(as.numeric(df_train[,ticker])))
  
  spec <- ugarchspec(
    mean.model = list(
      armaOrder = c(p, q),
      include.mean = FALSE
    ),
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    distribution.model = "std" #Dùng phân phối kiểm định t-Student "std" để xem có phù hợp không
  )
  
  fit <- try(
    ugarchfit(
      data = ret,
      spec = spec,
      solver = "hybrid"
    ),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) next
  if (fit@fit$convergence != 0) next
  #Xem các group của Adjusted Pearson Goodness-of-Fit Test
  pearson_tab <- try(
    gof(fit, groups = c(20, 30, 40, 50)),
    silent = TRUE
  )
  if (inherits(pearson_tab, "try-error")) next
  pv <- as.numeric(pearson_tab[, 3])
  if (length(pv) != 4L) next
  #Trả về p-value của các group và xem model nào oke
  best_garch_pearson$p_Pearson20[i] <- pv[1]
  best_garch_pearson$p_Pearson30[i] <- pv[2]
  best_garch_pearson$p_Pearson40[i] <- pv[3]
  best_garch_pearson$p_Pearson50[i] <- pv[4]
  best_garch_pearson$pearson_ok[i] <- all(
    is.finite(pv) & pv > 0.05
  )
}
best_garch_pearson[
  c("ticker", "p", "q", "tieu_chi",
  "p_Pearson20", "p_Pearson30",
  "p_Pearson40", "p_Pearson50",
  "pearson_ok"
)]
# Chỉ giữ model qua Pearson ở cả 4 group
best_garch_pearson[best_garch_pearson$pearson_ok &
                     best_garch_pearson$weighted_ok &
                     best_garch_pearson$mean_sig &
                     best_garch_pearson$coef_ok &
                     best_garch_pearson$se_ok,
                   c("ticker", "p", "q", "tieu_chi", "weighted_ok", "mean_sig",
                     "coef_ok", "se_ok",
                     "p_Pearson20", "p_Pearson30",
                     "p_Pearson40", "p_Pearson50",
                     "pearson_ok")]
#Ở đây xui cái là VHM không có cái nào qua được Pearson
#Những model lọc được chọn với phân phối std là:
#====================================
#ACB(2,2) FPT(2,4) HPG(4,4) MSN(5,4)
#====================================
#Cần kiểm tra fit lại cho VHM với std mở rộng thêm lên tham số bậc 10 
result_VHM_std <- data.frame()
ts_VHM <- ts(as.numeric(df_train[, "VHM_log_price"]), frequency = 1)
ret_VHM <- diff(ts_VHM)
#Vòng lặp tương tự với std nhưng chỉ dành riêng cho VHM
for (p in 0:10) {
  for (q in 0:10) {
    cat("p =", p, "q =", q, "\n")
    spec <- ugarchspec(
      mean.model = list(
        armaOrder = c(p, q),
        include.mean = FALSE
      ),
      variance.model = list(
        model = "sGARCH",
        garchOrder = c(1, 1)
      ),
      distribution.model = "std" #Dùng kiểm định phân phối std
    )
    model <- try(
      ugarchfit(
        data = ret_VHM,
        spec = spec,
        solver = "hybrid"
      ),
      silent = TRUE
    )
    #Hội tụ
    if (inherits(model, "try-error")) {
      cat("Lỗi fit:", as.character(model), "\n")
      next
    }
    if (model@fit$convergence != 0) next
    evaluation <- try({
      #Hệ số và sai số chuẩn
      cf <- coef(model)
      pers <- as.numeric(persistence(model))
      
      robust_tab <- model@fit$robust.matcoef
      se <- model@fit$matcoef[, 2]
      se_robust <- robust_tab[, 2]
      se_ok <- (
        length(se) == length(cf) &&
          length(se_robust) == length(cf) &&
          all(is.finite(se) & se > 0) &&
          all(is.finite(se_robust) & se_robust > 0)
      )
      mean_names <- c(
        if (p > 0) paste0("ar", seq_len(p)),
        if (q > 0) paste0("ma", seq_len(q))
      )
      p_mean <- robust_tab[mean_names, "Pr(>|t|)"] #Lấy ra p-value của tham số trong robust
      mean_sig <- (
        length(mean_names) == 0 ||
          all(is.finite(p_mean) & p_mean < 0.05)
      )
      
      # shape phải >2 để biết psai hữu hạn và 0< persistence = alpha + beta <1 mới chọn
      coef_ok <- (
        all(is.finite(cf)) &&
          is.finite(pers) &&
          cf["omega"] > 0 &&
          cf["alpha1"] >= 0 &&
          cf["beta1"] >= 0 &&
          pers >= 0 && pers < 1 &&
          cf["shape"] > 2
      )
      #AIC, BIC, Hanna-Quinn
      ic <- infocriteria(model)
      
      # Phần dư chuẩn hóa, xem p-value giống std
      z <- as.numeric(residuals(model, standardize = TRUE))
      eps <- as.numeric(residuals(model))
      sig <- as.numeric(sigma(model))
      
      if (any(!is.finite(c(z, eps, sig))) || any(sig <= 0)) {
        stop("Phần dư hoặc sigma không hợp lệ")
      }
      #Ljung-Box test Standardized Residuals
      wlb_z <- weighted_lb(z, p = 1, df = p + q)
      p_wlb_z <- as.numeric(wlb_z[, "p-value"])
      #Ljung-Box Test on Standardized Squared Residuals
      wlb_z2 <- weighted_lb(z, p = 2, df = 2)
      p_wlb_z2 <- as.numeric(wlb_z2[, "p-value"])
      #ARCH lag 3,5,7
      p_warch3 <- weighted_arch(
        eps, sig, lags = 3, fitdf = 2
      )$p.value
      
      p_warch5 <- weighted_arch(
        eps, sig, lags = 5, fitdf = 2
      )$p.value
      
      p_warch7 <- weighted_arch(
        eps, sig, lags = 7, fitdf = 2
      )$p.value
      p_warch <- c(p_warch3, p_warch5, p_warch7)
      pv_weighted <- c(p_wlb_z, p_wlb_z2, p_warch)
      weighted_ok <- (
        length(pv_weighted) == 9 &&
          all(is.finite(pv_weighted)) &&
          all(pv_weighted > 0.05)
      )
      # Pearson kiểm tra phân phối std khi fit
      pearson_tab <- gof(model, groups = c(20, 30, 40, 50))
      p_pearson <- as.numeric(pearson_tab[, 3])
      pearson_ok <- all(
        is.finite(p_pearson) & p_pearson > 0.05
      )
      data.frame(
        ticker = "VHM_log_price",
        distribution = "std",
        p = p,
        q = q,
        convergence = model@fit$convergence,
        alpha1 = unname(cf["alpha1"]),
        beta1 = unname(cf["beta1"]),
        persistence = pers,
        shape = unname(cf["shape"]),
        coef_ok = unname(coef_ok),
        se_ok = se_ok,
        mean_sig = mean_sig,
        AIC = ic["Akaike", 1],
        BIC = ic["Bayes", 1],
        HQ = ic["Hannan-Quinn", 1],
        p_WLB_z_min = min(p_wlb_z),
        p_WLB_z2_min = min(p_wlb_z2),
        p_WARCH_min = min(p_warch),
        weighted_ok = weighted_ok,
        p_Pearson20 = p_pearson[1],
        p_Pearson30 = p_pearson[2],
        p_Pearson40 = p_pearson[3],
        p_Pearson50 = p_pearson[4],
        pearson_ok = pearson_ok
      )
    }, silent = TRUE)
    
    if (inherits(evaluation, "try-error")) {
      cat("Lỗi chẩn đoán:", as.character(evaluation), "\n")
      next
    }
    result_VHM_std <- rbind(result_VHM_std, evaluation)
  }
}
warnings() #cái này để xem cảnh báo thôi do lặp nhiều quá thì nó bị giới hạn
str(result_VHM_std)
result_VHM_std_pass <- result_VHM_std[
  result_VHM_std$coef_ok &
    result_VHM_std$se_ok &
    result_VHM_std$mean_sig &
    result_VHM_std$weighted_ok,
  c(
    "ticker", "p", "q", "convergence", "persistence", "AIC", "BIC", "HQ",
    "coef_ok", "se_ok", "mean_sig", "weighted_ok", "pearson_ok"
  )
]
#Bây giờ cái pearson thì VHM không có cái nào đạt rồi, đây là hạn chế của VHM
#Ta sẽ xem xét BIC,AIC,HQ của nó để chọn model vì trừ pearson thì tất cả đều pass
result_VHM_std_pass[which.min(result_VHM_std_pass$BIC) ,]
result_VHM_std_pass[which.min(result_VHM_std_pass$AIC) ,]
result_VHM_std_pass[which.min(result_VHM_std_pass$HQ) ,]
#Vậy là kết quả nhóm chọn ra model VHM (2,5)

#==============================================
#ACB(2,2) FPT(2,4) HPG(4,4) MSN(5,4) VHM(2,5)
#==============================================

#Xem lai ca 5 mo hinh nay va forecast voi GARCH
#===================================
#ACB
ret_ACB <- diff(ts_ACB)
t.test(ret_ACB)
#ACB (2,2)
spec_ACB <- ugarchspec(mean.model = list(armaOrder = c(2,2), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_ACB <- ugarchfit(data=ret_ACB, spec = spec_ACB, solver = "hybrid")
fit_garch_ACB

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_ACB@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_ACB) #Vì persistence 0.9527 nên đây là một kết quả khá ổn
coef(fit_garch_ACB)["alpha1"] #kết quả cho biết model phản ứng với cú sốc mới yếu hơn
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_ACB)),0) 
#Xem sai so chuan
fit_garch_ACB@fit$robust.matcoef #có ar3 có ý nghĩa, ar1 và ar2 thì không 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_ACB)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_ACB <- residuals(fit_garch_ACB, standardize = TRUE)
Box.test(z_ACB, lag = 20, type = "Ljung-Box", fitdf = 4)
Box.test(z_ACB^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_ACB, lags=10) # ARCH không còn, 
Acf(z_ACB)
dir.create("hinh_ve", showWarnings = FALSE)
png( filename = file.path("hinh_ve", "ACB_acf_z.png"), 
     width = 1600, height = 1000, res = 200)
Acf( z_ACB, main = "ACB: ACF của phần dư chuẩn hóa")
dev.off()

#Dự báo cho ACB rolling từ cuối phiên train cho đến hết phiên test 
help(ugarchforecast)
train_ACB <- as.numeric(df_train$ACB_log_price)
test_ACB <- as.numeric(df_test$ACB_log_price)
m_ACB <- length(test_ACB)
m_ACB
spec_roll_ACB <- getspec(fit_garch_ACB)
pred_roll_ACB <- rep(NA, m_ACB)
pred_roll_ACB
for (i in 0:(m_ACB - 1)) {
  history_ACB <- c(train_ACB, head(test_ACB, i))
  fit_ACB <- try(
    ugarchfit(
      spec = spec_roll_ACB,
      data = diff(history_ACB),
      solver = "hybrid"
    ),
    silent = TRUE
  )
  
  if (inherits(fit_ACB, "try-error")) next
  if (fit_ACB@fit$convergence != 0) next
  
  fc_ACB <- ugarchforecast(fit_ACB, n.ahead = 1)
  
  pred_roll_ACB[i + 1] <- exp(
    tail(history_ACB, 1) + as.numeric(fitted(fc_ACB))
  )
}
pred_roll_ACB
#Vẽ dự báo rolling one step cho từng phiên từ 1-100 
dates_test_ACB <- as.Date(index(df_test$ACB_log_price))
actual_ACB <- exp(test_ACB)
actual_ACB
plot(
  dates_test_ACB, actual_ACB,
  type = "l",
  col = "red",
  ylim = range(c(actual_ACB, pred_roll_ACB), na.rm = TRUE),
  xlab = "Ngày giao dịch",
  ylab = "Giá",
  main = "ACB: Dự báo rolling trước 1 phiên"
)
lines(dates_test_ACB, pred_roll_ACB, col = "blue")
legend(
  "topleft",
  legend = c("Thực tế test", "Dự báo trước 1 phiên"),
  col = c("red", "blue"),
  lty = 1,
  bty = "n"
)
#===========================================
#FPT
ret_FPT <- diff(ts_FPT)
t.test(ret_FPT)
#FPT (2,4)
spec_FPT <- ugarchspec(mean.model = list(armaOrder = c(2,4), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_FPT <- ugarchfit(data=ret_FPT, spec = spec_FPT, solver = "hybrid")
fit_garch_FPT

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_FPT@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_FPT) #Vì persistence 0.998 nên đây là một kết quả cũng tạm chấp nhận
coef(fit_garch_FPT)["alpha1"] #phản ứng với cú sốc mới yếu
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_FPT)),0) #693 phien
#Xem sai so chuan
fit_garch_FPT@fit$robust.matcoef #tat ca tham so deu có ý nghĩa 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_FPT)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_FPT <- residuals(fit_garch_FPT, standardize = TRUE)
Box.test(z_FPT, lag = 20, type = "Ljung-Box", fitdf = 6)
Box.test(z_FPT^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_FPT, lags=10) # ARCH không còn, 
Acf(z_FPT)
dir.create("hinh_ve", showWarnings = FALSE)
png( filename = file.path("hinh_ve", "FPT_acf_z.png"), 
     width = 1600, height = 1000, res = 200)
Acf( z_FPT, main = "FPT: ACF của phần dư chuẩn hóa")
dev.off()

#Dự báo cho FPT rolling từ cuối phiên train cho đến hết phiên test 
train_FPT <- as.numeric(df_train$FPT_log_price)
test_FPT <- as.numeric(df_test$FPT_log_price)
m_FPT <- length(test_FPT)
spec_roll_FPT <- getspec(fit_garch_FPT)
pred_roll_FPT <- rep(NA, m_FPT)
pred_roll_FPT
for (i in 0:(m_FPT - 1)) {
  history_FPT <- c(train_FPT, head(test_FPT, i))
  
  fit_FPT <- try(
    ugarchfit(
      spec = spec_roll_FPT,
      data = diff(history_FPT),
      solver = "hybrid"),
    silent = TRUE
  )
  
  if (inherits(fit_FPT, "try-error")) next
  if (fit_FPT@fit$convergence != 0) next
  
  fc_FPT <- ugarchforecast(fit_FPT, n.ahead = 1)
  
  pred_roll_FPT[i + 1] <- exp(
    tail(history_FPT, 1) +
      as.numeric(fitted(fc_FPT))
  )
}
pred_roll_FPT

#Vẽ toàn bộ giá thực tế test và đường rolling
dates_test_FPT <- as.Date(index(df_test$FPT_log_price))
actual_FPT <- exp(test_FPT)
actual_FPT
plot(
  dates_test_FPT, actual_FPT,
  type = "l",
  col = "red",
  ylim = range(c(actual_FPT, pred_roll_FPT), na.rm = TRUE),
  xlab = "Ngày giao dịch",
  ylab = "Giá",
  main = "FPT: Dự báo rolling trước 1 phiên"
)
#Forecast Line
lines(dates_test_FPT, pred_roll_FPT, col = "blue")
legend(
  "topleft",
  legend = c("Thực tế test", "Dự báo trước 1 phiên"),
  col = c("red", "blue"),
  lty = 1,
  bty = "n"
)

#==================================
#HPG 
ret_HPG <- diff(ts_HPG)
t.test(ret_HPG)
#HPG (4,4)
spec_HPG <- ugarchspec(mean.model = list(armaOrder = c(4,4), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_HPG <- ugarchfit(data=ret_HPG, spec = spec_HPG, solver = "hybrid")
fit_garch_HPG

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_HPG@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_HPG) #Vì persistence 0.986 nên đây là một kết quả cũng good
coef(fit_garch_HPG)["alpha1"] #phản ứng với cú sốc mới yếu
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_HPG)),0) #50 phien
#Xem sai so chuan
fit_garch_HPG@fit$robust.matcoef #tat ca tham so deu có ý nghĩa 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_HPG)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_HPG <- residuals(fit_garch_HPG, standardize = TRUE)
Box.test(z_HPG, lag = 20, type = "Ljung-Box", fitdf = 8) #Kết quả này vừa đạt ngưỡng
Box.test(z_HPG^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_HPG, lags=10) # ARCH không còn, 
Acf(z_HPG)  #Còn 3 lag sau 10 vượt nhưng không đáng kể
dir.create("hinh_ve", showWarnings = FALSE)
png( filename = file.path("hinh_ve", "HPG_acf_z.png"), 
     width = 1600, height = 1000, res = 200)
Acf( z_HPG, main = "HPG: ACF của phần dư chuẩn hóa")
dev.off()

#Dự báo cho FPT rolling từ cuối phiên train cho đến hết phiên test 
train_HPG <- as.numeric(df_train$HPG_log_price)
test_HPG <- as.numeric(df_test$HPG_log_price)
m_HPG <- length(test_HPG)
spec_roll_HPG <- getspec(fit_garch_HPG)
pred_roll_HPG <- rep(NA, m_HPG)

for (i in 0:(m_HPG - 1)) {
  history_HPG <- c(train_HPG, head(test_HPG, i))
  fit_HPG <- try(
    ugarchfit(
      spec = spec_roll_HPG,
      data = diff(history_HPG),
      solver = "hybrid"
    ),
    silent = TRUE
  )
  
  if (inherits(fit_HPG, "try-error")) next
  if (fit_HPG@fit$convergence != 0) next
  
  fc_HPG <- ugarchforecast(fit_HPG, n.ahead = 1)
  
  pred_roll_HPG[i + 1] <- exp(
    tail(history_HPG, 1) + as.numeric(fitted(fc_HPG))
  )
}
pred_roll_HPG
#Vẽ dự báo rolling one step từ 1-100 cho HPG
dates_test_HPG <- as.Date(index(df_test$HPG_log_price))
actual_HPG <- exp(test_HPG)
actual_HPG
plot(
  dates_test_HPG, actual_HPG,
  type = "l",
  col = "red",
  ylim = range(c(actual_HPG, pred_roll_HPG), na.rm = TRUE),
  xlab = "Ngày giao dịch",
  ylab = "Giá",
  main = "HPG: Dự báo rolling trước 1 phiên"
)
lines(dates_test_HPG, pred_roll_HPG, col = "blue")
legend(
  "topleft",
  legend = c("Thực tế test", "Dự báo trước 1 phiên"),
  col = c("red", "blue"),
  lty = 1,
  bty = "n"
)

#============================
#VHM
ret_VHM <- diff(ts_VHM)
t.test(ret_VHM)
#VHM (2,5)
spec_VHM <- ugarchspec(mean.model = list(armaOrder = c(2,5), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_VHM <- ugarchfit(data=ret_VHM, spec = spec_VHM, solver = "hybrid")
fit_garch_VHM

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_VHM@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_VHM) #Vì persistence 0.998 nên đây là một kết quả chua good
coef(fit_garch_VHM)["alpha1"] #phản ứng với các cú sốc yếu
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_VHM)),0) #692 phien
#Xem sai so chuan
fit_garch_VHM@fit$robust.matcoef #tat ca tham so deu có ý nghĩa 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_VHM)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_VHM <- residuals(fit_garch_VHM, standardize = TRUE)
Box.test(z_VHM, lag = 20, type = "Ljung-Box", fitdf = 7)
Box.test(z_VHM^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_VHM, lags=10) # ARCH không còn, 
Acf(z_VHM) #ACF cho kết quả khá ổn, không lag nào vượt ngưỡng, lag1 còn hơi cao cận biên 
dir.create("hinh_ve", showWarnings = FALSE)
png( filename = file.path("hinh_ve", "VHM_acf_z.png"), 
     width = 1600, height = 1000, res = 200)
Acf( z_VHM, main = "VHM: ACF của phần dư chuẩn hóa")
dev.off()

# Dự báo VHM rolling từ cuối train đến hết test
train_VHM <- as.numeric(df_train$VHM_log_price)
test_VHM <- as.numeric(df_test$VHM_log_price)
m_VHM <- length(test_VHM)
spec_roll_VHM <- getspec(fit_garch_VHM)
pred_roll_VHM <- rep(NA, m_VHM)

for (i in 0:(m_VHM - 1)) {
  history_VHM <- c(train_VHM, head(test_VHM, i))
  fit_VHM <- try(
    ugarchfit(
      spec = spec_roll_VHM,
      data = diff(history_VHM),
      solver = "hybrid"
    ),
    silent = TRUE
  )
  
  if (inherits(fit_VHM, "try-error")) next
  if (fit_VHM@fit$convergence != 0) next
  
  fc_VHM <- ugarchforecast(fit_VHM, n.ahead = 1)
  
  pred_roll_VHM[i + 1] <- exp(
    tail(history_VHM, 1) + as.numeric(fitted(fc_VHM))
  )
}
pred_roll_VHM

# Vẽ dự báo rolling one step cho VHM
dates_test_VHM <- as.Date(index(df_test$VHM_log_price))
actual_VHM <- exp(test_VHM)
actual_VHM
plot(
  dates_test_VHM, actual_VHM,
  type = "l",
  col = "red",
  ylim = range(c(actual_VHM, pred_roll_VHM), na.rm = TRUE),
  xlab = "Ngày giao dịch",
  ylab = "Giá",
  main = "VHM: Dự báo rolling trước 1 phiên"
)
lines(dates_test_VHM, pred_roll_VHM, col = "blue")
legend(
  "topleft",
  legend = c("Thực tế test", "Dự báo trước 1 phiên"),
  col = c("red", "blue"),
  lty = 1,
  bty = "n"
)

#============================
#MSN
ret_MSN <- diff(ts_MSN)
t.test(ret_MSN)
#MSN (5,4)
spec_MSN <- ugarchspec(mean.model = list(armaOrder = c(5,4), include.mean=FALSE),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       distribution.model = 'std')
fit_garch_MSN <- ugarchfit(data=ret_MSN, spec = spec_MSN, solver = "hybrid")
fit_garch_MSN

# Tiêu chí thứ nhất: mô hình phải hội tụ convergence = 0
fit_garch_MSN@fit$convergence == 0
#Tiêu chí thứ hai: hệ số hợp lý
#Kiểm tra hệ số: Kiểm tra tính dai dẳng
persistence(fit_garch_MSN) #Vì persistence 0.940 nên đây là một kết quả good
coef(fit_garch_MSN)["alpha1"] #phản ứng với các cú sốc yếu
#Số phiên bị ảnh hưỡng kéo dài bởi biến động
round(log(0.5) / log(persistence(fit_garch_MSN)),0) #11 phien
#Xem sai so chuan
fit_garch_MSN@fit$robust.matcoef #tat ca tham so deu có ý nghĩa 
#Tiêu chí thứ ba: so sánh xem AIC và BIC voi cac model khac
infocriteria(fit_garch_MSN)

#Kiểm tra xem GARCH đã sửa được ARCH chưa
z_MSN <- residuals(fit_garch_MSN, standardize = TRUE)
Box.test(z_MSN, lag = 20, type = "Ljung-Box", fitdf = 9)
Box.test(z_MSN^2, lag = 20, type = "Ljung-Box", fitdf = 2)    #lấy tham số của GARCH
ArchTest(z_MSN, lags=10) # ARCH không còn, 
Acf(z_MSN) #ACF cho kết quả khá ổn, không lag nào vượt ngưỡng
dir.create("hinh_ve", showWarnings = FALSE)
png( filename = file.path("hinh_ve", "MSN_acf_z.png"), 
     width = 1600, height = 1000, res = 200)
Acf(z_MSN, main = "MSN: ACF của phần dư chuẩn hóa")
dev.off()

# Dự báo MSN rolling từ cuối train đến hết test
train_MSN <- as.numeric(df_train$MSN_log_price)
test_MSN <- as.numeric(df_test$MSN_log_price)
m_MSN <- length(test_MSN)
spec_roll_MSN <- getspec(fit_garch_MSN)
pred_roll_MSN <- rep(NA, m_MSN)

for (i in 0:(m_MSN - 1)) {
  history_MSN <- c(train_MSN, head(test_MSN, i))
  fit_MSN <- try(
    ugarchfit(
      spec = spec_roll_MSN,
      data = diff(history_MSN),
      solver = "hybrid"
    ),
    silent = TRUE
  )
  
  if (inherits(fit_MSN, "try-error")) next
  if (fit_MSN@fit$convergence != 0) next
  
  fc_MSN <- ugarchforecast(fit_MSN, n.ahead = 1)
  
  pred_roll_MSN[i + 1] <- exp(
    tail(history_MSN, 1) + as.numeric(fitted(fc_MSN))
  )
}
pred_roll_MSN

# Vẽ dự báo rolling one step cho MSN
dates_test_MSN <- as.Date(index(df_test$MSN_log_price))
actual_MSN <- exp(test_MSN)
actual_MSN 
plot(
  dates_test_MSN, actual_MSN,
  type = "l",
  col = "red",
  ylim = range(c(actual_MSN, pred_roll_MSN), na.rm = TRUE),
  xlab = "Ngày giao dịch",
  ylab = "Giá",
  main = "MSN: Dự báo rolling trước 1 phiên"
)
lines(dates_test_MSN, pred_roll_MSN, col = "blue")
legend(
  "topleft",
  legend = c("Thực tế test", "Dự báo trước 1 phiên"),
  col = c("red", "blue"),
  lty = 1,
  bty = "n"
)

#=====================================================================
#   Bắt đầu backtest thử chiến lược dùng dự báo và không dùng dự báo 
#=====================================================================
tickers
# Giá thực tế test
P <- cbind(ACB = exp(as.numeric(df_test[, "ACB_log_price"])),
           FPT = exp(as.numeric(df_test[, "FPT_log_price"])),
           HPG = exp(as.numeric(df_test[, "HPG_log_price"])),
           VHM = exp(as.numeric(df_test[, "VHM_log_price"])),
           MSN = exp(as.numeric(df_test[, "MSN_log_price"])))

# Giá đóng cửa phiên cuối của train
last_train <- c(ACB = exp(tail(as.numeric(df_train[, "ACB_log_price"]), 1)),
                FPT = exp(tail(as.numeric(df_train[, "FPT_log_price"]), 1)),
                HPG = exp(tail(as.numeric(df_train[, "HPG_log_price"]), 1)),
                VHM = exp(tail(as.numeric(df_train[, "VHM_log_price"]), 1)),
                MSN = exp(tail(as.numeric(df_train[, "MSN_log_price"]), 1)))

str(pred_roll_ACB)
str(pred_roll_FPT)
str(pred_roll_HPG)
str(pred_roll_MSN)
str(pred_roll_VHM)

# Giá dự báo cho từng phiên test
pred <- cbind(ACB = pred_roll_ACB,
              FPT = pred_roll_FPT,
              HPG = pred_roll_HPG,
              VHM = pred_roll_VHM,
              MSN = pred_roll_MSN)

dates <- as.Date(index(df_test[, "ACB_log_price"]))
dates
m <- nrow(P)
m

# Giá thực tế ngay trước mỗi phiên test
P_previous <- rbind(last_train, P[-m, , drop = FALSE])
P_previous

# Mức tăng giá dự báo, đặt tín hiệu ngưỡng dự báo là 0.3%
signal_return <- pred / P_previous - 1

threshold <- 0.3/100

# TRUE khi muốn giữ cổ phiếu; FALSE khi muốn giữ tiền mặt
signal <- signal_return > threshold
str(signal) #logical
signal
#Tổng 100 phiên, xem có bao nhiêu tín hiệu giữ cổ phiếu và tiền mặt ở mỗi mã
colSums(signal)
colSums(!signal)

# TẠO BACKTEST DANH MỤC VỚI CHIẾN LƯỢC DÙNG DỰ BÁO 
initial_capital <- 100000 # Giả sử chỉ số vốn ban đầu = 100tr 
fee <- 0.1/100 # Giả sử chi phí giao dịch 0,1% mỗi lần mua bán cổ phiếu 
fee
cash <- rep(initial_capital / 5, 5) #Vốn đầu tư mỗi mã chia đều
shares <- rep(0, 5) #Số cổ phiếu nắm giữ của mỗi mã ban đầu
cash
shares

m
equity_forecast <- numeric(m) #Tổng giá trị tại mỗi phiên dùng dự báo
total_cost <- 0 #Tổng phí ban đầu
n_trades <- 0 #số lần trade ban đầu

for (t in seq_len(m)) {
  
  for (j in seq_along(tickers)) {
    
    price <- P[t, j]
    
    # Cuối kỳ: thanh lý toàn bộ, không mở vị thế mới
    if (t == m) {
      
      if (shares[j] > 0) {
        sale_value <- shares[j] * price
        cost <- sale_value * fee
        
        cash[j] <- cash[j] + sale_value - cost
        shares[j] <- 0
        
        total_cost <- total_cost + cost
        n_trades <- n_trades + 1
      }
      
      next
    }
    
    # Dự báo lỗi: giữ nguyên trạng thái
    if (is.na(signal[t, j])) next
    
    # Tín hiệu mua, hiện đang giữ tiền mặt
    if (signal[t, j] && shares[j] == 0) {
      
      shares[j] <- cash[j] / (price * (1 + fee))
      cost <- shares[j] * price * fee
      cash[j] <- 0
      
      total_cost <- total_cost + cost
      n_trades <- n_trades + 1
      
      # Tín hiệu tiền mặt, hiện đang giữ cổ phiếu
    } else if (!signal[t, j] && shares[j] > 0) {
      #Giá trị giao dịch và phí giao dịch
      sale_value <- shares[j] * price
      cost <- sale_value * fee
      #Bán hết cổ phiếu ở phiên cuối cùng 
      cash[j] <- cash[j] + sale_value - cost
      shares[j] <- 0
      
      total_cost <- total_cost + cost
      n_trades <- n_trades + 1
    }
  }
  
  equity_forecast[t] <- sum(cash + shares * P[t, ])
}
equity_forecast <- equity_forecast #Lưu lại
equity_forecast
equity_forecast[m]
cash #Lỗ nặng nhất là VHM
shares
n_trades
total_cost
#Lợi nhuận cuối kỳ 
(tail(equity_forecast, 1) / initial_capital - 1) * 100

#BACKTEST VỚI DANH MỤC MUA VÀ GIỮ NHƯ BÌNH THƯỜNG
#Chỉ mua 1 lần và bán 1 lần nên tốn phí giao dịch chỉ 1 lần
shares_buyhold <- (initial_capital / 5) / (P[1, ] * (1 + fee))
shares_buyhold
P
#Tổng giá trị qua từng phiên (Vì P là ma trận nên dùng phép nhân ma trận %*%)
equity_buyhold <- as.numeric(P %*% shares_buyhold)
equity_buyhold
# Thanh lý cuối kỳ và tính chi phí bán ở phiên 100
equity_buyhold[m] <- equity_buyhold[m] * (1 - fee)
equity_buyhold[m]
#Lợi nhuận cuối kỳ 
(tail(equity_buyhold, 1) / initial_capital - 1) * 100
#Lỗ 10% so với chiến lược dự báo là lỗ hơn 6% thôi. 
#Vậy qua kết quả lợi nhuận cuối kỳ cho thấy chiến lược dùng dự báo cho thấy kết quả tốt hơn

# Vẽ Plot cho giá trị của 2 chiến lược 
plot(dates, equity_buyhold,
     type = "l",
     col = "black",
     ylim = range(c(equity_buyhold, equity_forecast)),
     xlab = "Ngày giao dịch",
     ylab = "Giá trị danh mục, vốn đầu kỳ = 100",
     main = "So sánh chiến lược dùng dự báo và mua–giữ"
)
#Vẽ giá trị đường dự báo
lines(dates, equity_forecast, col = "red")
legend(
  "topleft",
  legend = c("Mua và giữ", "Dùng dự báo"),
  col = c("black", "red"),
  lty = 1,
  bty = "n"
)
