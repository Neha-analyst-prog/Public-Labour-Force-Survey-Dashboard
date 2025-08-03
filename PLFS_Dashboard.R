library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
# Define states (37 including "all India")
states <- c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh", "Delhi", "Goa", "Gujarat", 
            "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", 
            "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", 
            "Tamil Nadu", "Telangana", "Tripura", "Uttarakhand", "Uttar Pradesh", "West Bengal", 
            "Andaman & N. Island", "Chandigarh", "Dadra & Nagar Haveli & Daman & Diu", "Jammu & Kashmir", "Ladakh", 
            "Lakshadweep", "Puducherry", "All India")

# rural_data
rural_data <- data.frame(
  State = states,
  Male_0_4 = c(231, 166, 441, 1091, 219, 3, 16, 331, 246, 189, 417, 199, 217, 629, 430, 225, 207, 77, 116, 425, 191, 566, 34, 358, 151, 132, 128, 1510, 527, 22, NA, 22, 353, 30, 7, 10, 9916),
  Female_0_4 = c(203, 164, 443, 883, 260, 7, 14, 284, 204, 154, 388, 186, 203, 553, 404, 207, 279, 99, 129, 420, 168, 521, 32, 310, 134, 124, 150, 1330, 515, 20, NA, 20, 333, 12, 11, 18, 9182),
  Male_5_14 = c(598, 534, 1110, 1889, 511, 25, 56, 674, 634, 495, 880, 573, 541, 1106, 1173, 488, 535, 233, 328, 894, 529, 1269, 111, 856, 363, 307, 373, 3099, 1002, 67, NA, 52, 909, 60, 15, 50, 22339),
  Female_5_14 = c(533, 533, 1046, 1735, 491, 15, 54, 573, 500, 409, 741, 538, 526, 1012, 1051, 467, 529, 239, 354, 811, 523, 1116, 90, 777, 344, 296, 285, 2754, 985, 59, NA, 41, 803, 48, 15, 22, 20315),
  Male_15_29 = c(990, 732, 1664, 2247, 995, 42, 63, 1217, 813, 584, 1135, 954, 589, 2094, 1976, 523, 677, 287, 220, 1402, 783, 1718, 211, 1190, 593, 680, 478, 3981, 1836, 93, NA, 110, 1128, 76, 23, 50, 32154),
  Female_15_29 = c(872, 665, 1598, 2154, 937, 21, 63, 1149, 645, 560, 1173, 776, 610, 1909, 1523, 551, 642, 211, 234, 1625, 660, 1766, 202, 1209, 507, 582, 486, 4011, 1997, 73, NA, 90, 1009, 63, 13, 49, 30635)
)

# urban_data
urban_data <- data.frame(
  State = states,
  Male_0_4 = c(149, 72, 99, 302, 74, 101, 20, 243, 182, 33, 173, 236, 205, 300, 466, 188, 45, 129, 64, 117, 202, 242, 31, 344, 162, 63, 117, 529, 270, 28, 33, 26, 183, 10, 15, 39, 5492),
  Female_0_4 = c(140, 84, 76, 285, 97, 98, 27, 217, 142, 29, 131, 207, 251, 254, 434, 185, 44, 180, 64, 120, 192, 235, 21, 316, 140, 46, 96, 485, 293, 18, 37, 17, 172, 14, 13, 37, 5197),
  Male_5_14 = c(377, 236, 211, 592, 236, 256, 91, 522, 410, 78, 388, 518, 589, 679, 1200, 427, 146, 426, 233, 280, 504, 674, 62, 713, 461, 129, 252, 1341, 659, 55, 99, 52, 450, 23, 39, 91, 13499),
  Female_5_14 = c(387, 230, 212, 538, 192, 207, 79, 463, 317, 80, 356, 479, 560, 625, 1057, 412, 141, 430, 226, 282, 484, 580, 57, 725, 452, 127, 240, 1131, 630, 54, 77, 42, 404, 34, 46, 89, 12445),
  Male_15_29 = c(808, 385, 388, 1004, 471, 517, 86, 1117, 770, 195, 652, 1123, 827, 1202, 2324, 566, 295, 547, 229, 527, 878, 1185, 189, 1401, 714, 248, 406, 2245, 1348, 126, 179, 103, 743, 62, 57, 142, 24059),
  Female_15_29 = c(787, 432, 389, 849, 461, 399, 99, 989, 591, 130, 617, 995, 833, 1108, 2015, 550, 303, 510, 239, 523, 768, 1013, 156, 1449, 680, 245, 398, 1983, 1422, 114, 156, 85, 645, 55, 45, 161, 22194)
)

# household_data
household_data <- data.frame(
  State = states,
  Rural_HH = c(98146, 2304, 65076, 206344, 46523, 920, 1589, 76469, 36278, 15002, 56197, 91323, 48596, 117452, 148716, 4351, 5899, 1189, 3031, 87058, 37024, 108792, 1318, 111520, 58988, 7942, 16783, 304690, 176105, 608, NA, 622, 19285, 377, 33, 1162, 1957710),
  Urban_HH = c(43684, 569, 10169, 21849, 12663, 38220, 2271, 66111, 26755, 2925, 14952, 70468, 45719, 45746, 128965, 1984, 1313, 992, 1300, 17623, 25948, 44475, 572, 100819, 43414, 1924, 5962, 88379, 79402, 499, 3239, 1348, 5490, 87, 95, 2180, 958113),
  Rural_Avg_Size = c(3.3, 4.8, 4.6, 5.1, 4.4, 3.5, 3.8, 4.7, 4.6, 3.8, 4.8, 4, 3.5, 4.7, 4.3, 4.6, 5.1, 4.8, 4.6, 3.9, 4.3, 4.8, 3.6, 3.4, 3.6, 3.9, 4, 4.9, 3.9, 3.4, NA, 4.5, 5.1, 5.1, 4.5, 3.6, 4.4),
  Urban_Avg_Size = c(3.2, 3.9, 3.6, 4.8, 3.9, 3.3, 3.8, 3.8, 3.9, 2.7, 4.1, 3.1, 3.4, 4.1, 3.7, 4.1, 3.8, 3.7, 4.2, 3.4, 3.8, 4.3, 2.5, 3.2, 3.2, 3.4, 3.8, 4.4, 3.5, 3.3, 3.3, 2.8, 4.2, 3.4, 3.9, 3.2, 3.7),
  Rural_Sex_Ratio = c(1019, 922, 977, 959, 1006, 706, 943, 946, 866, 1040, 991, 994, 1121, 931, 981, 1017, 1052, 948, 1046, 1063, 986, 1029, 880, 1028, 1068, 1014, 983, 983, 1006, 920, NA, 925, 932, 847, 881, 976, 989),
  Urban_Sex_Ratio = c(1064, 1035, 990, 909, 931, 841, 974, 910, 869, 821, 950, 961, 1157, 975, 927, 1038, 1094, 1034, 958, 972, 953, 922, 767, 1066, 962, 1047, 985, 918, 1016, 964, 903, 747, 932, 853, 908, 1143, 962)
)

# rural_hh_type
rural_hh_type <- data.frame(
  State = states,
  Self_Employed_Agriculture = c(27.7, 48.7, 24.2, 36.1, 54.3, 5.7, 6.2, 40.7, 24.5, 30.4, 33.4, 41.8, 17.3, 52.3, 37, 42.6, 32, 41.2, 36.3, 29.4, 23, 44.3, 18.1, 17.5, 39, 33.4, 31.3, 48.5, 27.3, 27.5, NA, 2.6, 18.6, 17, 14.7, 5.6, 36.2),
  Self_Employed_NonAgriculture = c(14.8, 16.1, 27.1, 19.9, 7.6, 32.2, 33.1, 11.9, 17.7, 17, 19.7, 10.6, 21.5, 10.8, 12.4, 24.4, 12.3, 25.2, 19, 18.2, 18.5, 19.6, 20.9, 13, 17.5, 22.3, 20.1, 17.9, 22.4, 14.4, NA, 8.5, 30.4, 28.8, 16.5, 12.2, 17.1),
  Regular_Wage = c(13.6, 27.5, 21, 8.5, 14.5, 43.6, 40.8, 24.4, 33, 28.2, 12.4, 11.8, 26.5, 10.5, 19.8, 26.5, 16.8, 29.1, 39.7, 12.5, 24.5, 19.8, 48.8, 27.1, 13.1, 12.4, 26.3, 8.4, 17.2, 33.9, NA, 75.2, 28.5, 42.3, 43.6, 52.5, 16),
  Casual_Labour_Agriculture = c(15.9, 0, 4.8, 6.9, 7.4, 0.1, 5, 9.5, 3.4, 0.9, 0.7, 15.4, 7, 9.5, 18, 0.5, 15.4, 0.4, 0.3, 4.4, 8.4, 2, 0, 12.8, 12.2, 4.5, 3.1, 2.9, 11.2, 1.8, NA, 4.7, 0, 0, 0.1, 8.8, 8.3),
  Casual_Labour_NonAgriculture = c(11.3, 5.3, 21.5, 22.3, 10.9, 16.8, 3.7, 7.9, 11.4, 15, 28.9, 10, 15.1, 14.2, 6.3, 4.9, 23.2, 3.6, 2.8, 23.1, 16.1, 10.4, 11.4, 19.7, 5, 22.1, 9.3, 16.8, 15.5, 14.8, NA, 8, 18.6, 11.5, 21.8, 16.1, 15.2),
  Others = c(16.5, 2.4, 1.3, 6.3, 5.3, 1.7, 11.1, 5.7, 9.9, 8.4, 4.9, 10.4, 12.5, 2.8, 6.4, 1, 0.2, 0.4, 1.9, 12.4, 9.4, 3.8, 0.7, 9.9, 13.2, 5.3, 9.9, 5.5, 6.3, 7.6, NA, 1, 3.8, 0.3, 3.4, 4.8, 7.2)
)

# urban_hh_type
urban_hh_type <- data.frame(
  State = states,
  Self_Employed = c(27.9, 35.2, 37.5, 45.1, 28.6, 32.4, 27.5, 33.4, 30, 19.2, 31.5, 26.5, 31.9, 37.3, 28.6, 47.4, 24.5, 49, 31.3, 28.4, 35.9, 38.5, 30.3, 21.7, 28.1, 40.5, 35.5, 43.8, 37.4, 16.6, 20.5, 10.6, 40.1, 38.3, 19.2, 15, 32.1),
  Regular_Wage = c(39.1, 51.3, 43.5, 28.7, 52.1, 46.1, 50.7, 51.3, 50, 55.5, 39.3, 46.8, 35, 40.4, 53, 35.7, 41.1, 46.5, 52.6, 38.1, 45.6, 37.4, 40.5, 47, 46.4, 29.6, 41.4, 35.4, 38.2, 63.7, 59.9, 86.1, 44.8, 44.1, 41.7, 53.4, 44),
  Casual_Labour = c(11.1, 5.8, 10.7, 14.4, 10, 5, 7.3, 4.1, 8.5, 6.9, 11, 10.3, 15.1, 9.1, 8.2, 6, 14.6, 3.6, 7.1, 13, 10.6, 10.5, 9.8, 14.1, 7.2, 11.6, 6.6, 10.2, 14.2, 9, 4.1, 0.3, 6.8, 10.8, 17.5, 9.2, 10.2),
  Others = c(22, 7.6, 8.3, 11.8, 9.3, 16.4, 14.5, 11.2, 11.5, 18.4, 18.2, 16.4, 18, 13.2, 10.2, 10.9, 19.8, 1, 9, 20.6, 7.9, 13.6, 19.3, 17.2, 18.3, 18.3, 16.5, 10.6, 10.2, 10.7, 15.5, 3.1, 8.2, 6.9, 21.6, 22.4, 13.7)
)

# dependency_data
dependency_data <- data.frame(
  State = states,
  Rural_Dependency = c(43.7, 42.6, 49.3, 67.9, 41.5, 40.1, 41.8, 47.7, 49.6, 40.5, 59.9, 45.8, 50.6, 49.5, 45.6, 49.5, 64.2, 52, 67.7, 48.3, 42.7, 54.5, 33.7, 42.7, 43.6, 41.6, 46.3, 55.2, 42.9, 34.9, NA, 45.7, 48.5, 38, 37.3, 31.7, 50.8),
  Urban_Dependency = c(36.4, 39.7, 36.5, 51.4, 32, 41.1, 51.8, 38.3, 40.5, 34.2, 42.1, 35.8, 50.6, 43.7, 36.4, 45.5, 35.4, 46.1, 54.6, 39.9, 41.5, 38.5, 28.2, 40.7, 42.2, 38.7, 44.5, 41.7, 37.4, 32, 43.7, 37, 41.4, 35.2, 41.4, 37.8, 40),
  Total_Dependency = c(41.4, 42.1, 47.8, 66.3, 39.6, 41.1, 47.4, 43.6, 46, 39.7, 56.3, 41.9, 50.6, 47.9, 41.5, 48.3, 59.4, 49.3, 63.8, 47, 42.2, 49.9, 32.4, 41.8, 43, 41.1, 45.9, 52.2, 41.2, 33.6, 43.7, 40.6, 47.1, 37.6, 40.2, 35.5, 47.5)
)

# literacy_data
literacy_data <- data.frame(
  State = states,
  Rural_Male_7_above = c(74.4, 86.7, 89.7, 81.5, 85, 87.8, 97.2, 88.2, 89.2, 94.3, 80.7, 84.3, 96.1, 80, 90.2, 93.5, 94.5, 99.3, 96.8, 83.2, 85.2, 83.6, 87.5, 86.7, 79.3, 95.2, 89.6, 85, 83.8, 95.2, NA, 87.1, 89.5, 87.1, 100, 95.7, 84.7),
  Rural_Female_7_above = c(60.9, 78.7, 82.7, 65, 68.2, 80.2, 88.2, 70.5, 72.5, 82.1, 68, 71.9, 92.6, 62.6, 76, 88.3, 92.7, 96.7, 93.1, 71, 77.1, 61.8, 76.4, 75.8, 61.1, 90.3, 74.5, 68, 76.8, 85.6, NA, 68.4, 72.7, 71.6, 98.9, 84.2, 70.4),
  Urban_Male_7_above = c(89.1, 94.6, 95.6, 89.8, 90.7, 91.7, 96.9, 95.2, 94.9, 96.6, 91.3, 94.5, 97.4, 91.9, 95.9, 96.9, 98.4, 99.1, 98.2, 93.7, 89.9, 91.6, 94.9, 93.9, 92.8, 98.4, 92.6, 89.3, 90.7, 94.8, 96.4, 98, 89.8, 92.9, 99.2, 97.8, 92.9),
  Urban_Female_7_above = c(80.3, 87.6, 90.8, 77.5, 81.1, 81.5, 91.3, 87.3, 85.5, 91.1, 81.7, 86.5, 95.6, 79.3, 89.5, 93.1, 97.2, 97.4, 96.6, 87, 84.3, 77.1, 90, 87.6, 82.6, 97.2, 84.9, 79.7, 85.4, 88.6, 90.7, 91.2, 78.3, 82.9, 93.3, 91.3, 84.9)
)

# unemployment_data
unemployment_data <- data.frame(
  State = states,
  Rural_Male_Not_Literate = c(0.2, 0, 0, 1.3, 0.3, 0, 0, 0, 1, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0, 0, 1.1, 0.4, 0, 0.4, 0.2, 0, 0, 0.1, 0.3, 0, NA, 0, 0, 0, 0, 0.3, 0.2),
  Rural_Male_Secondary_Above = c(9.8, 12.5, 6.9, 6.2, 3.1, 6.1, 10.3, 1, 5.6, 5, 1.7, 4.4, 8.7, 1.7, 5.1, 7.5, 8.1, 3.3, 9.7, 7.5, 8.8, 8.7, 10, 7, 6.6, 4.5, 8.5, 5.1, 4.6, 13.5, NA, 9.6, 6.4, 3.8, 1.7, 0.3, 5.6),
  Rural_Male_All = c(3.8, 5.4, 3.6, 3.3, 1.6, 3.6, 7.1, 0.5, 3.6, 3.2, 0.8, 2.5, 4.6, 0.6, 2.8, 4.7, 3.6, 1.5, 5.5, 2.9, 5.1, 3.5, 2.7, 3.5, 3.7, 1.6, 4.8, 2.6, 2.2, 6.9, NA, 4.2, 3.5, 2, 1.2, 0.1, 2.7),
  Rural_Female_Not_Literate = c(0, 0, 0, 0.2, 0, 0, 9.4, 0, 0, 0, 0, 0, 3.5, 0, 0, 1.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 1, NA, 0, 0.3, 0, 0, 0, 0.1),
  Rural_Female_Secondary_Above = c(13.3, 19.2, 12, 5.4, 6.2, 93.1, 23.8, 1.3, 4.8, 12.9, 0.3, 3.5, 21.4, 3.3, 4, 13.3, 12.3, 3.9, 14.6, 11.4, 13.7, 16, 5.4, 9.3, 12.1, 11.2, 5.6, 7.2, 8.5, 34.2, NA, 0, 22.6, 18.2, 57.3, 12.4, 9),
  Rural_Female_All = c(2.9, 4.6, 3.5, 1, 1.3, 19.5, 13.8, 0.2, 1.7, 7, 0.1, 0.9, 12.1, 0.3, 1.1, 6.6, 6.6, 1, 5.8, 2, 6.2, 2.7, 1.6, 2.7, 3.1, 1.2, 2.7, 1.5, 2.3, 16.6, NA, 0, 7.1, 6.6, 49.1, 4.7, 2.1),
  Urban_Male_Not_Literate = c(0, 0, 1.7, 1.8, 3.3, 0, 0, 0, 0, 0, 1.2, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0.6, 0, 1, 0, 0, 1, 0.6, 0.3, 0, 0, 0, 0.2, 0, 0, 0, 0.5),
  Urban_Male_Secondary_Above = c(9, 14.2, 8.6, 10.2, 9.5, 2.5, 4.6, 2.7, 5.2, 5.8, 7.6, 5.3, 6.3, 5, 6.2, 6.6, 11.4, 4.6, 16.4, 7.2, 7.1, 9.4, 3.8, 4.9, 8.1, 4.1, 3.8, 8.6, 3.8, 9.3, 4.6, 3.4, 7.1, 7.8, 15.4, 8.6, 6.2),
  Urban_Male_All = c(5.5, 9.4, 5.7, 6.8, 6.4, 2.2, 3.8, 2, 4.1, 4.8, 5.8, 4.1, 4.2, 2.9, 4.9, 5.6, 8.9, 3.2, 11.1, 5.6, 4.7, 6.5, 1.7, 3.2, 6, 2.7, 3.5, 5.7, 2.6, 7, 3.9, 2, 4.8, 5.6, 11.1, 6.3, 4.4),
  Urban_Female_Not_Literate = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 2.3, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 2.4, 0, 0, 14.7, 0, 0, 3.9, 0, 0, 0, 0.1),
  Urban_Female_Secondary_Above = c(16.3, 32.7, 18, 20.3, 19.3, 2.5, 21.3, 6.5, 5.1, 25.5, 12, 7.3, 16.2, 7, 9.7, 15.1, 27.5, 8.3, 19.9, 21.1, 14.5, 25.6, 10.7, 12.1, 17, 10, 18.9, 22.2, 10.2, 41.2, 15.1, 7.7, 41.1, 48.2, 33.5, 10.1, 13.3),
  Urban_Female_All = c(6.9, 18.1, 11.8, 9.2, 10.6, 1.1, 18.6, 3.3, 3.3, 18.2, 5.8, 4.4, 10.9, 2.6, 6, 10.3, 20.6, 3.9, 11.6, 9.7, 8.6, 10.9, 6.9, 6, 10.4, 4.4, 13, 11, 5, 28.6, 15.3, 3.8, 27.2, 34.4, 27.2, 6.9, 7.1),
  Total_Persons_Not_Literate = c(0.1, 0, 0, 0.8, 0.3, 0, 3, 0, 0.4, 0, 0, 0, 1.7, 0, 0, 0.8, 0, 0, 0, 0, 0.5, 0.2, 0, 0.2, 0.1, 0, 0.2, 0.1, 0.2, 2.6, 0, 0, 0.3, 0, 0, 0, 0.2),
  Total_Persons_Secondary_Above = c(10.8, 16, 9, 6.9, 6.2, 2.7, 11.6, 2.4, 5.4, 8.9, 3.3, 5, 12.3, 3.3, 5.9, 9.5, 12.6, 4.8, 13.4, 9.1, 9.6, 11.4, 7.5, 7.3, 9.1, 5.9, 7.6, 6.9, 5.7, 20.2, 7.6, 4.7, 12.7, 10.4, 16.1, 7.5, 7.1),
  Total_Persons_All = c(4.1, 6.1, 3.9, 3, 2.5, 2.1, 8.5, 1.1, 3.4, 5.5, 1.3, 2.7, 7.2, 1, 3.3, 6.1, 6.2, 2.3, 7.1, 3.1, 5.5, 4.2, 2.3, 3.5, 4.8, 1.7, 4.3, 3.1, 2.5, 11.8, 7.1, 2.3, 6.1, 5.1, 11.9, 4.6, 3.2)
)

# Marital status data from Table 5 
marital_data <- data.frame(
  Area = rep(c("Rural", "Urban", "Rural+Urban"), each = 3),
  Sex = rep(c("Male", "Female", "Persons"), 3),
  Never_Married = c(49.1, 38.8, 44.0, 47.6, 36.6, 42.2, 48.6, 38.2, 43.5),
  Currently_Married = c(48.0, 52.0, 50.0, 49.7, 52.0, 50.8, 48.5, 52.0, 50.2),
  Widowed = c(2.5, 8.8, 5.7, 2.3, 10.6, 6.4, 2.4, 9.4, 5.9),
  Divorced_Separated = c(0.3, 0.4, 0.3, 0.5, 0.7, 0.6, 0.4, 0.5, 0.4)
)

# UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Periodic Labour Force Statistics-India"),
  dashboardSidebar(
    selectInput("state_single", "Select State/UT:", choices = states, selected = "All India"),
    pickerInput(
      inputId = "state_multi",
      label = "Select up to 5 States/UTs for Comparison:",
      choices = states[states != "All India"],
      multiple = TRUE,
      selected = c("Andhra Pradesh", "Arunachal Pradesh", "Assam","Kerala"),
      options = list(
        `max-options` = 5,
        `max-options-text` = "Maximum of 5 states can be selected"
      )
    ),
    tags$p("Note: Maximum 5 states can be selected for comparison.", style = "color: white; font-size: 12px;")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Population Pyramid",
               fluidRow(box(plotlyOutput("pyramidPlot"), width = 12))),
      tabPanel("Household Stats",
               fluidRow(
                 box(valueBoxOutput("totalHHBoxRural", width = 12), valueBoxOutput("totalHHBoxUrban", width = 12),
                     valueBoxOutput("avgSizeBoxRural", width = 12), valueBoxOutput("avgSizeBoxUrban", width = 12),
                     width = 4),
                 box(plotlyOutput("hhBarPlot"), width = 8)
               )),
      tabPanel("Age Distribution",
               fluidRow(box(plotlyOutput("heatmapPlot"), width = 12, title = "Age Distribution Heatmap"))),
      tabPanel("Household Types",
               fluidRow(
                 box(plotlyOutput("hhTypeBarPlotUrban"), width = 6, title = "Urban Household Types"),
                 box(plotlyOutput("hhTypeBarPlotRural"), width = 6, title = "Rural Household Types"),
                 box(plotlyOutput("hhTypeDonutUrban"), width = 6, title = "Urban Household Distribution"),
                 box(plotlyOutput("hhTypeDonutRural"), width = 6, title = "Rural Household Distribution")
               )),
      tabPanel("Dependency Ratio",
               fluidRow(box(plotlyOutput("dependencyBarPlot"), width = 12, title = "Dependency Ratio by Area"))),
      tabPanel("Literacy Rate",
               fluidRow(box(plotlyOutput("literacyBarPlot"), width = 12, title = "Literacy Rate (7+ years) by Sex and Area"))),
      tabPanel("Unemployment Rate",
               fluidRow(box(plotlyOutput("unemploymentBarPlot"), width = 12, title = "Unemployment Rate by Sex and Area"))),
      tabPanel("Marital Status",
               fluidRow(box(plotlyOutput("maritalBarPlot"), width = 12, title = "Marital Status Distribution"))),
      tabPanel("Cluster Analysis & Statistics",
               fluidRow(
                 box(selectInput("dataset", "Select Dataset for Analysis:",
                                 choices = c("Rural Population", "Urban Population", "Household Data", "Dependency Data"),
                                 selected = "Rural Population"), width = 4),
                 box(plotlyOutput("kmeansPlot"), width = 6, title = "K-means Clustering"),
                 box(DT::dataTableOutput("kmeansTable"), width = 6, title = "K-means Cluster Assignments"),
                 box(plotlyOutput("hclustPlot"), width = 6, title = "Hierarchical Clustering"),
                 box(DT::dataTableOutput("hclustTable"), width = 6, title = "Hierarchical Cluster Assignments"),
                 box(DT::dataTableOutput("statsTable"), width = 6, title = "Statistical Test Summary"),
                 box(textOutput("statsInference"), width = 6, title = "Inference")
               ))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Notify user if they try to select more than 3 states
  observeEvent(input$state_multi, {
    if (length(input$state_multi) > 3) {
      showNotification("You can select a maximum of 3 states.", type = "warning")
    }
  })
  
  # Reactive data for single-state tabs
  rural_data_single <- reactive({
    rural_data %>% filter(State == input$state_single)
  })
  
  urban_data_single <- reactive({
    urban_data %>% filter(State == input$state_single)
  })
  
  # Reactive data for multi-state tabs
  household_data_multi <- reactive({
    household_data %>% filter(State %in% input$state_multi)
  })
  
  dependency_data_multi <- reactive({
    dependency_data %>% filter(State %in% input$state_multi)
  })
  
  literacy_data_multi <- reactive({
    literacy_data %>% filter(State %in% input$state_multi)
  })
  
  unemployment_data_multi <- reactive({
    unemployment_data %>% filter(State %in% input$state_multi)
  })
  
  # Reactive dataset for cluster analysis (scaled data)
  analysis_data <- reactive({
    data <- switch(input$dataset,
                   "Rural Population" = rural_data %>% select(-State) %>% na.omit(),
                   "Urban Population" = urban_data %>% select(-State) %>% na.omit(),
                   "Household Data" = household_data %>% select(Rural_HH, Urban_HH, Rural_Avg_Size, Urban_Avg_Size) %>% na.omit(),
                   "Dependency Data" = dependency_data %>% select(Rural_Dependency, Urban_Dependency) %>% na.omit())
    scale(data)
  })
  
  # dataset with states for cluster assignment
  original_data <- reactive({
    switch(input$dataset,
           "Rural Population" = rural_data %>% select(State, everything()) %>% na.omit(),
           "Urban Population" = urban_data %>% select(State, everything()) %>% na.omit(),
           "Household Data" = household_data %>% select(State, Rural_HH, Urban_HH, Rural_Avg_Size, Urban_Avg_Size) %>% na.omit(),
           "Dependency Data" = dependency_data %>% select(State, Rural_Dependency, Urban_Dependency) %>% na.omit())
  })
  
  # K-means Clustering
  kmeans_result <- reactive({
    set.seed(123)
    kmeans(analysis_data(), centers = 3, nstart = 25)
  })
  
  # Hierarchical Clustering
  hclust_result <- reactive({
    dist_matrix <- dist(analysis_data(), method = "euclidean")
    hclust(dist_matrix, method = "ward.D2")
  })
  
  # K-means Plot
  output$kmeansPlot <- renderPlotly({
    clusters <- kmeans_result()$cluster
    pca <- prcomp(analysis_data(), scale. = TRUE)
    pca_df <- as.data.frame(pca$x[, 1:2])
    pca_df$Cluster <- as.factor(clusters)
    
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = paste("K-means Clustering (", input$dataset, ")")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # K-means Cluster Assignment Table
  output$kmeansTable <- DT::renderDataTable({
    clusters <- kmeans_result()$cluster
    df <- original_data()
    df$Cluster <- clusters
    DT::datatable(df %>% select(State, Cluster), options = list(pageLength = 5))
  })
  
  # Hierarchical Clustering Plot
  output$hclustPlot <- renderPlotly({
    hc <- hclust_result()
    clusters <- cutree(hc, k = 3)
    pca <- prcomp(analysis_data(), scale. = TRUE)
    pca_df <- as.data.frame(pca$x[, 1:2])
    pca_df$Cluster <- as.factor(clusters)
    
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = paste("Hierarchical Clustering (", input$dataset, ")")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Hierarchical Cluster Assignment Table
  output$hclustTable <- DT::renderDataTable({
    hc <- hclust_result()
    clusters <- cutree(hc, k = 3)
    df <- original_data()
    df$Cluster <- clusters
    DT::datatable(df %>% select(State, Cluster), options = list(pageLength = 5))
  })
  
  # Statistical Tests Summary Table
  output$statsTable <- DT::renderDataTable({
    data <- switch(input$dataset,
                   "Rural Population" = rural_data %>% select(Male_0_4, Female_0_4) %>% na.omit(),
                   "Urban Population" = urban_data %>% select(Male_0_4, Female_0_4) %>% na.omit(),
                   "Household Data" = household_data %>% select(Rural_HH, Urban_HH) %>% na.omit(),
                   "Dependency Data" = dependency_data %>% select(Rural_Dependency, Urban_Dependency) %>% na.omit())
    
    if (ncol(data) < 2) {
      return(data.frame(Test = "N/A", Statistic = NA, P_Value = NA, Message = "Need at least 2 columns for T-test."))
    }
    
    if (nrow(data) < 2) {
      return(data.frame(Test = "N/A", Statistic = NA, P_Value = NA, Message = "Need at least 2 rows for T-test."))
    }
    
    t_test <- tryCatch({
      t.test(data[[1]], data[[2]], paired = FALSE)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(t_test)) {
      t_result <- data.frame(Test = "T-test",
                             Statistic = round(t_test$statistic, 3),
                             P_Value = round(t_test$p.value, 3))
    } else {
      t_result <- data.frame(Test = "T-test", Statistic = NA, P_Value = NA)
    }
    
    if (input$dataset == "Household Data") {
      anova_data <- household_data %>%
        select(Rural_HH, Urban_HH, Rural_Avg_Size) %>%
        na.omit() %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value")
      
      if (length(unique(anova_data$Variable)) > 1 && nrow(anova_data) > 1) {
        anova_result <- tryCatch({
          aov(Value ~ Variable, data = anova_data)
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(anova_result)) {
          anova_summary <- summary(anova_result)[[1]]
          anova_result <- data.frame(Test = "ANOVA",
                                     Statistic = round(anova_summary$"F value"[1], 3),
                                     P_Value = round(anova_summary$"Pr(>F)"[1], 3))
          return(rbind(t_result, anova_result))
        } else {
          anova_result <- data.frame(Test = "ANOVA", Statistic = NA, P_Value = NA)
          return(rbind(t_result, anova_result))
        }
      } else {
        anova_result <- data.frame(Test = "ANOVA", Statistic = NA, P_Value = NA, Message = "Not enough groups for ANOVA.")
        return(rbind(t_result, anova_result))
      }
    }
    
    return(t_result)
  })
  
  # Statistical Inference
  output$statsInference <- renderText({
    data <- switch(input$dataset,
                   "Rural Population" = rural_data %>% select(Male_0_4, Female_0_4) %>% na.omit(),
                   "Urban Population" = urban_data %>% select(Male_0_4, Female_0_4) %>% na.omit(),
                   "Household Data" = household_data %>% select(Rural_HH, Urban_HH) %>% na.omit(),
                   "Dependency Data" = dependency_data %>% select(Rural_Dependency, Urban_Dependency) %>% na.omit())
    
    if (ncol(data) < 2 || nrow(data) < 2) {
      return("Insufficient data for statistical tests (need at least 2 columns and 2 rows).")
    }
    
    t_test <- tryCatch({
      t.test(data[[1]], data[[2]], paired = FALSE)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(t_test)) {
      p_val <- t_test$p.value
      t_inference <- ifelse(p_val < 0.05,
                            "T-test: Significant difference detected (p < 0.05).",
                            "T-test: No significant difference detected (p >= 0.05).")
    } else {
      t_inference <- "T-test: Unable to compute due to data issues."
    }
    
    if (input$dataset == "Household Data") {
      anova_data <- household_data %>%
        select(Rural_HH, Urban_HH, Rural_Avg_Size) %>%
        na.omit() %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value")
      
      if (length(unique(anova_data$Variable)) > 1 && nrow(anova_data) > 1) {
        anova_result <- tryCatch({
          aov(Value ~ Variable, data = anova_data)
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(anova_result)) {
          anova_p_val <- summary(anova_result)[[1]]$"Pr(>F)"[1]
          anova_inference <- ifelse(anova_p_val < 0.05,
                                    "ANOVA: Significant differences among groups (p < 0.05).",
                                    "ANOVA: No significant differences among groups (p >= 0.05).")
          return(paste(t_inference, anova_inference, sep = "\n"))
        } else {
          return(paste(t_inference, "ANOVA: Unable to compute due to data issues.", sep = "\n"))
        }
      } else {
        return(paste(t_inference, "ANOVA: Not enough groups or data for analysis.", sep = "\n"))
      }
    }
    
    return(t_inference)
  })
  
  # Population Pyramid (single state)
  output$pyramidPlot <- renderPlotly({
    df_rural <- rural_data_single() %>%
      pivot_longer(cols = starts_with("Male") | starts_with("Female"),
                   names_to = c("Sex", "Age_Group"), names_sep = "_", values_to = "Population") %>%
      mutate(Population = ifelse(Sex == "Male", -Population, Population), Area = "Rural")
    
    df_urban <- urban_data_single() %>%
      pivot_longer(cols = starts_with("Male") | starts_with("Female"),
                   names_to = c("Sex", "Age_Group"), names_sep = "_", values_to = "Population") %>%
      mutate(Population = ifelse(Sex == "Male", -Population, Population), Area = "Urban")
    
    df <- bind_rows(df_rural, df_urban)
    
    p <- ggplot(df, aes(x = Age_Group, y = Population, fill = interaction(Sex, Area))) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = paste("Population Pyramid -", input$state_single)) +
      scale_y_continuous(labels = abs) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    ggplotly(p)
  })
  
  # Household Stats (multi-state)
  output$totalHHBoxRural <- renderValueBox({
    hh <- household_data_multi()
    value <- paste(lapply(hh$Rural_HH, format), collapse = ", ")
    valueBox(value, paste("Rural Households (", paste(hh$State, collapse = ", "), ")"), icon = icon("home"), color = "blue")
  })
  
  output$totalHHBoxUrban <- renderValueBox({
    hh <- household_data_multi()
    value <- paste(lapply(hh$Urban_HH, format), collapse = ", ")
    valueBox(value, paste("Urban Households (", paste(hh$State, collapse = ", "), ")"), icon = icon("home"), color = "yellow")
  })
  
  output$avgSizeBoxRural <- renderValueBox({
    hh <- household_data_multi()
    value <- paste(hh$Rural_Avg_Size, collapse = ", ")
    valueBox(value, paste("Rural Avg Size (", paste(hh$State, collapse = ", "), ")"), icon = icon("users"), color = "green")
  })
  
  output$avgSizeBoxUrban <- renderValueBox({
    hh <- household_data_multi()
    value <- paste(hh$Urban_Avg_Size, collapse = ", ")
    valueBox(value, paste("Urban Avg Size (", paste(hh$State, collapse = ", "), ")"), icon = icon("users"), color = "red")
  })
  
  output$hhBarPlot <- renderPlotly({
    hh <- household_data_multi() %>%
      select(State, Rural_HH, Urban_HH) %>%
      pivot_longer(cols = c(Rural_HH, Urban_HH), names_to = "Area", values_to = "Households") %>%
      mutate(Area = gsub("_HH", "", Area))
    
    custom_colors <- c("Urban" = "darkblue", "Rural" = "darkred")
    p <- ggplot(hh, aes(x = State, y = Households, fill = Area)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Households by Area and State") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Age Distribution (single state)
  output$heatmapPlot <- renderPlotly({
    df <- bind_rows(rural_data_single() %>% mutate(Area = "Rural"), urban_data_single() %>% mutate(Area = "Urban")) %>%
      pivot_longer(cols = starts_with("Male") | starts_with("Female"),
                   names_to = c("Sex", "Age_Group"), names_sep = "_", values_to = "Population")
    
    p <- ggplot(df, aes(x = Age_Group, y = interaction(Sex, Area), fill = Population)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = paste("Age Distribution Heatmap -", input$state_single)) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    ggplotly(p)
  })
  
  # Household Types - Urban Bar Plot (single state)
  output$hhTypeBarPlotUrban <- renderPlotly({
    df <- urban_hh_type %>%
      filter(State == input$state_single) %>%
      pivot_longer(cols = -State, names_to = "Type", values_to = "Percentage")
    
    p <- ggplot(df, aes(x = Type, y = Percentage)) +
      geom_bar(stat = "identity", fill = "#00AFBB") +
      labs(title = paste("Urban Household Types -", input$state_single)) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Household Types - Rural Bar Plot (single state)
  output$hhTypeBarPlotRural <- renderPlotly({
    df <- rural_hh_type %>%
      filter(State == input$state_single) %>%
      pivot_longer(cols = -State, names_to = "Type", values_to = "Percentage")
    
    p <- ggplot(df, aes(x = Type, y = Percentage)) +
      geom_bar(stat = "identity", fill = "#FC4E07") +
      labs(title = paste("Rural Household Types -", input$state_single)) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Household Types - Urban Donut Chart
  output$hhTypeDonutUrban <- renderPlotly({
    df <- urban_hh_type %>%
      filter(State == input$state_single) %>%
      pivot_longer(cols = -State, names_to = "Type", values_to = "Percentage")
    
    plot_ly(df, labels = ~Type, values = ~Percentage, type = "pie",
            hole = 0.6, textposition = "inside", textinfo = "label+percent") %>%
      layout(title = paste("Urban Household Distribution -", input$state_single),
             showlegend = TRUE)
  })
  
  # Household Types - Rural Donut Chart
  output$hhTypeDonutRural <- renderPlotly({
    df <- rural_hh_type %>%
      filter(State == input$state_single) %>%
      pivot_longer(cols = -State, names_to = "Type", values_to = "Percentage")
    
    plot_ly(df, labels = ~Type, values = ~Percentage, type = "pie",
            hole = 0.6, textposition = "inside", textinfo = "label+percent") %>%
      layout(title = paste("Rural Household Distribution -", input$state_single),
             showlegend = TRUE)
  })
  
  # Dependency Ratio (multi-state)
  output$dependencyBarPlot <- renderPlotly({
    df <- dependency_data_multi() %>%
      select(State, Rural_Dependency, Urban_Dependency) %>%
      pivot_longer(cols = c(Rural_Dependency, Urban_Dependency), names_to = "Area", values_to = "Ratio") %>%
      mutate(Area = gsub("_Dependency", "", Area))
    
    custom_colors <- c("Urban" = "pink", "Rural" = "darkgreen")
    p <- ggplot(df, aes(x = State, y = Ratio, fill = Area)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Dependency Ratio by Area and State", y = "Dependency Ratio") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Literacy Rate (multi-state)
  output$literacyBarPlot <- renderPlotly({
    df <- literacy_data_multi() %>%
      select(State, Rural_Male_7_above, Rural_Female_7_above, Urban_Male_7_above, Urban_Female_7_above) %>%
      pivot_longer(cols = -State, names_to = "Category", values_to = "Rate") %>%
      mutate(Area = ifelse(grepl("Rural", Category), "Rural", "Urban"),
             Sex = ifelse(grepl("Male", Category), "Male", "Female"))
    
    custom_colors <- c("Female.Urban" = "pink", "Female.Rural" = "maroon", "Male.Urban" = "darkgreen", "Male.Rural" = "darkblue")
    p <- ggplot(df, aes(x = State, y = Rate, fill = interaction(Sex, Area))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Literacy Rate (7+ years) by Sex, Area, and State", y = "Literacy Rate (%)") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Unemployment Rate (multi-state) - Stacked Bar Chart
  output$unemploymentBarPlot <- renderPlotly({
    df <- unemployment_data_multi() %>%
      select(State,
             Rural_Male_Secondary_Above, Rural_Female_Secondary_Above,
             Urban_Male_Secondary_Above, Urban_Female_Secondary_Above) %>%
      pivot_longer(cols = -State, names_to = "Category", values_to = "Rate") %>%
      mutate(Area = ifelse(grepl("Rural", Category), "Rural", "Urban"),
             Sex = ifelse(grepl("Male", Category), "Male", "Female"))
    
    custom_colors <- c("Urban" = "darkred", "Rural" = "darkblue")
    p <- ggplot(df, aes(x = Sex, y = Rate, fill = Area)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~State) +
      labs(title = "Unemployment Rate (Secondary & Above) by Sex and Area",
           y = "Unemployment Rate (%)") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Marital Status Bar Plot (single state)
  output$maritalBarPlot <- renderPlotly({
    df <- marital_data %>%
      filter(input$state_single == "All India") %>%  # Adjust if you have state-specific data
      pivot_longer(cols = c(Never_Married, Currently_Married, Widowed, Divorced_Separated),
                   names_to = "Marital_Status", values_to = "Percentage") %>%
      mutate(Group = paste(Area, Sex, sep = " - "))
    
    custom_colors <- c("Never_Married" = "skyblue", "Currently_Married" = "red",
                       "Widowed" = "purple", "Divorced_Separated" = "darkgreen")
    
    p <- ggplot(df, aes(x = Group, y = Percentage, fill = Marital_Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Marital Status Distribution -", input$state_single),
           y = "Percentage (%)") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui, server)
