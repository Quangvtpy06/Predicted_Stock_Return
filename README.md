# Predicted_Stock_Return

Dự án nghiên cứu và phân tích kinh tế lượng / học máy nhằm dự báo tỷ suất sinh lời của cổ phiếu dựa trên dữ liệu giá đóng cửa và khối lượng giao dịch thị trường chứng khoán Việt Nam giai đoạn 2022 – 2026.

---

## 📌 1. Giới thiệu (Overview)
Dự án sử dụng ngôn ngữ R để xử lý dữ liệu chuỗi thời gian (Time Series), tính toán các biến tài chính và xây dựng mô hình hồi quy / dự báo tỷ suất sinh lời của các cổ phiếu niêm yết.

## 🎯 2. Mục tiêu dự án
- Thu thập và chuẩn hóa dữ liệu giá đóng cửa (`Close`) và khối lượng (`Volume`) giai đoạn 2022 – 2026.
- Trực quan hóa biến động giá và biểu đồ phân tích kỹ thuật.
- Xây dựng mô hình kiểm định và đánh giá mối quan hệ giữa khối lượng giao dịch và tỷ suất sinh lời cổ phiếu.

## 🛠️ 3. Công nghệ & Thư viện (Tech Stack)
- **Ngôn ngữ:** R (100%)
- **Thư viện chính:** `tidyverse`, `ggplot2`, `xts`, `zoo`, `quantmod`, `lmtest`

## 📁 4. Cấu trúc thư mục (Project Structure)
```text
Predicted_Stock_Return/
├── hinh_ve/                            # Biểu đồ và hình vẽ xuất ra từ R
├── Code.R                              # Script R chứa toàn bộ quy trình xử lý dữ liệu & mô hình
├── stocks_close_volume_2022_2026.csv   # Dữ liệu giá đóng cửa và khối lượng (2022-2026)
└── README.md                           # Tài liệu hướng dẫn dự án
