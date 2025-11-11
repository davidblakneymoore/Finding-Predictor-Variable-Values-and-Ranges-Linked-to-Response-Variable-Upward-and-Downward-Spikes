
# A Function for Determining Predictor-Variable Values and Ranges of Values Corresponding to Upward and Downward Spikes in Response-Variable Values: Explanation and Examples

# David Moore
# November 2025
# davidblakneymoore@gmail.com


# Creating the Function
Determining_Predictor_Variable_Values_and_Ranges_of_Values_Corresponding_to_Upward_and_Downward_Spikes_in_Response_Variable_Values <- function (Predictor_Variable, Response_Variable, Data_Frame, Number_of_Bins = 100, Number_of_Predictor_Variable_Increments = 1000, Number_of_Angle_Increments = 100, Threshold_1 = 0.95, Threshold_2 = 0.95) {
  if (missing(Predictor_Variable)) {
    stop ("The 'Predictor_Variable' argument is missing.")
  }
  if (missing(Response_Variable)) {
    stop ("The 'Response_Variable' argument is missing.")
  }
  Predictor_Variable_Name <- deparse(substitute(Predictor_Variable))
  Response_Variable_Name <- deparse(substitute(Response_Variable))
  if (!missing(Data_Frame)) {
    if (class(Data_Frame) != 'data.frame') {
      stop ("The 'Data_Frame' object must be of class 'data.frame'.")
    }
    Data_Frame <- data.frame(Predictor_Variable = Data_Frame[, which(colnames(Data_Frame) == Predictor_Variable_Name)], Response_Variable = Data_Frame[, which(colnames(Data_Frame) == Response_Variable_Name)])
  } else if (missing(Data_Frame)) {
    if (length(unique(length(Predictor_Variable), length(Response_Variable))) != 1) {
      stop ("The 'Predictor_Variable' and 'Response_Variable' arguments must all contain the same number of elements.")
    }
    Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)
  }
  if (!is.numeric(Data_Frame$Predictor_Variable)) {
    stop ("The 'Predictor_Variable' argument must be of class 'numeric'.")
  }
  if (!is.numeric(Data_Frame$Response_Variable)) {
    stop ("The 'Response_Variable' argument must be of class 'numeric'.")
  }
  if (nrow(Data_Frame) < 3) {
    stop ("The 'Data_Frame' data frame must contain at least 3 rows.")
  }
  if (length(Number_of_Bins) != 1) {
    stop ("The 'Number_of_Bins' argument must be of length 1.")
  }
  if (is.na(Number_of_Bins)) {
    stop ("The 'Number_of_Bins' argument must be provided.")
  }
  if (!is.numeric(Number_of_Bins)) {
    stop ("The 'Number_of_Bins' argument must be a number.")
  }
  if ((Number_of_Bins %% 1) != 0) {
    stop ("The 'Number_of_Bins' argument must be an integer.")
  }
  if (Number_of_Bins <= 0) {
    stop ("The 'Number_of_Bins' argument must be positive.")
  }
  if (length(Number_of_Predictor_Variable_Increments) != 1) {
    stop ("The 'Number_of_Predictor_Variable_Increments' argument must be of length 1.")
  }
  if (is.na(Number_of_Predictor_Variable_Increments)) {
    stop ("The 'Number_of_Predictor_Variable_Increments' argument must be provided.")
  }
  if (!is.numeric(Number_of_Predictor_Variable_Increments)) {
    stop ("The 'Number_of_Predictor_Variable_Increments' argument must be a number.")
  }
  if ((Number_of_Predictor_Variable_Increments %% 1) != 0) {
    stop ("The 'Number_of_Predictor_Variable_Increments' argument must be an integer.")
  }
  if (Number_of_Predictor_Variable_Increments <= 0) {
    stop ("The 'Number_of_Predictor_Variable_Increments' argument must be positive.")
  }
  if (length(Number_of_Angle_Increments) != 1) {
    stop ("The 'Number_of_Angle_Increments' argument must be of length 1.")
  }
  if (is.na(Number_of_Angle_Increments)) {
    stop ("The 'Number_of_Angle_Increments' argument must be provided.")
  }
  if (!is.numeric(Number_of_Angle_Increments)) {
    stop ("The 'Number_of_Angle_Increments' argument must be a number.")
  }
  if ((Number_of_Angle_Increments %% 1) != 0) {
    stop ("The 'Number_of_Angle_Increments' argument must be an integer.")
  }
  if (Number_of_Angle_Increments <= 0) {
    stop ("The 'Number_of_Angle_Increments' argument must be positive.")
  }
  if (length(Threshold_1) != 1) {
    stop ("The 'Threshold_1' argument must be of length 1.")
  }
  if (is.na(Threshold_1)) {
    stop ("The 'Threshold_1' argument must be provided.")
  }
  if (!is.numeric(Threshold_1)) {
    stop ("The 'Threshold_1' argument must be a number.")
  }
  if ((Threshold_1 <= 0) | (Threshold_1 >= 1)) {
    stop ("The 'Threshold_1' argument must be between 0 and 1 (exclusive).")
  }
  if (length(Threshold_2) != 1) {
    stop ("The 'Threshold_2' argument must be of length 1.")
  }
  if (is.na(Threshold_2)) {
    stop ("The 'Threshold_2' argument must be provided.")
  }
  if (!is.numeric(Threshold_2)) {
    stop ("The 'Threshold_2' argument must be a number.")
  }
  if ((Threshold_2 <= 0) | (Threshold_2 >= 1)) {
    stop ("The 'Threshold_2' argument must be between 0 and 1 (exclusive).")
  }
  Predictor_Variable_Bins <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), length.out = (Number_of_Bins + 1))
  Predictor_Variable_Bins <- data.frame(Starting_Predictor_Variable_Value = Predictor_Variable_Bins[-length(Predictor_Variable_Bins)], Ending_Predictor_Variable_Value = Predictor_Variable_Bins[-1])
  Predictor_Variable_Bins$Midpoint <- rowMeans(Predictor_Variable_Bins)
  Positive_Predictor_Variable_Values_Only <- Data_Frame[which(Data_Frame$Response_Variable > 0), ]
  Negative_Predictor_Variable_Values_Only <- Data_Frame[which(Data_Frame$Response_Variable < 0), ]
  Angles <- seq(0, pi / 2, length.out = Number_of_Angle_Increments)
  Positive_Predictor_Variable_Values_Only$Standardized_Predictor_Variable <- (Positive_Predictor_Variable_Values_Only$Predictor_Variable - mean(Positive_Predictor_Variable_Values_Only$Predictor_Variable)) / sd(Positive_Predictor_Variable_Values_Only$Predictor_Variable)
  Positive_Predictor_Variable_Values_Only$Standardized_Response_Variable <- (Positive_Predictor_Variable_Values_Only$Response_Variable - mean(Positive_Predictor_Variable_Values_Only$Response_Variable)) / sd(Positive_Predictor_Variable_Values_Only$Response_Variable)
  Horizontal_Axis_Coordinates <- seq(min(Positive_Predictor_Variable_Values_Only$Predictor_Variable), max(Positive_Predictor_Variable_Values_Only$Predictor_Variable), length.out = Number_of_Predictor_Variable_Increments)
  Standardized_Horizontal_Axis_Coordinates <- seq(min(Positive_Predictor_Variable_Values_Only$Standardized_Predictor_Variable), max(Positive_Predictor_Variable_Values_Only$Standardized_Predictor_Variable), length.out = Number_of_Predictor_Variable_Increments)
  Overall_Sum_of_Squared_Residuals_for_Positive_Points <- do.call("rbind", lapply(seq_along(Standardized_Horizontal_Axis_Coordinates), function (i) {
    as.data.frame(t(sapply(seq_along(Angles), function (j) {
      Indices <- ifelse((Positive_Predictor_Variable_Values_Only$Response_Variable > ((tan(Angles[j]) * Positive_Predictor_Variable_Values_Only$Predictor_Variable) - (Horizontal_Axis_Coordinates[i] * tan(Angles[j])))) & (Positive_Predictor_Variable_Values_Only$Response_Variable > ((-tan(Angles[j]) * Positive_Predictor_Variable_Values_Only$Predictor_Variable) + (Horizontal_Axis_Coordinates[i] * tan(Angles[j])))), T, F)
      Vertical_Points <- Positive_Predictor_Variable_Values_Only[which(Indices), ]
      Horizontal_Points <- Positive_Predictor_Variable_Values_Only[which(!Indices), ]
      Horizontal_Sum_of_Squared_Residuals <- sum((Horizontal_Points$Standardized_Response_Variable) ^ 2)
      Vertical_Sum_of_Squared_Residuals <- sum((Vertical_Points$Standardized_Predictor_Variable - Standardized_Horizontal_Axis_Coordinates[i]) ^ 2)
      Overall_Sum_of_Squared_Residuals <- Horizontal_Sum_of_Squared_Residuals + Vertical_Sum_of_Squared_Residuals
      c(Horizontal_Axis_Coordinate = Horizontal_Axis_Coordinates[i], Angle = Angles[j], Overall_Sum_of_Squared_Residual = Overall_Sum_of_Squared_Residuals)
    })))
  }))
  Positive_Critical_Value <- Overall_Sum_of_Squared_Residuals_for_Positive_Points[which.min(Overall_Sum_of_Squared_Residuals_for_Positive_Points$Overall_Sum_of_Squared_Residual), ]
  Negative_Predictor_Variable_Values_Only$Standardized_Predictor_Variable <- (Negative_Predictor_Variable_Values_Only$Predictor_Variable - mean(Negative_Predictor_Variable_Values_Only$Predictor_Variable)) / sd(Negative_Predictor_Variable_Values_Only$Predictor_Variable)
  Negative_Predictor_Variable_Values_Only$Standardized_Response_Variable <- (Negative_Predictor_Variable_Values_Only$Response_Variable - mean(Negative_Predictor_Variable_Values_Only$Response_Variable)) / sd(Negative_Predictor_Variable_Values_Only$Response_Variable)
  Horizontal_Axis_Coordinates <- seq(min(Negative_Predictor_Variable_Values_Only$Predictor_Variable), max(Negative_Predictor_Variable_Values_Only$Predictor_Variable), length.out = Number_of_Predictor_Variable_Increments)
  Standardized_Horizontal_Axis_Coordinates <- seq(min(Negative_Predictor_Variable_Values_Only$Standardized_Predictor_Variable), max(Negative_Predictor_Variable_Values_Only$Standardized_Predictor_Variable), length.out = Number_of_Predictor_Variable_Increments)
  Overall_Sum_of_Squared_Residuals_for_Negative_Points <- do.call("rbind", lapply(seq_along(Horizontal_Axis_Coordinates), function (i) {
    as.data.frame(t(sapply(seq_along(Angles), function (j) {
      Indices <- ifelse((Negative_Predictor_Variable_Values_Only$Response_Variable < ((tan(Angles[j]) * Negative_Predictor_Variable_Values_Only$Predictor_Variable) - (Horizontal_Axis_Coordinates[i] * tan(Angles[j])))) & (Negative_Predictor_Variable_Values_Only$Response_Variable < ((-tan(Angles[j]) * Negative_Predictor_Variable_Values_Only$Predictor_Variable) + (Horizontal_Axis_Coordinates[i] * tan(Angles[j])))), T, F)
      Vertical_Points <- Negative_Predictor_Variable_Values_Only[which(Indices), ]
      Horizontal_Points <- Negative_Predictor_Variable_Values_Only[which(!Indices), ]
      Horizontal_Sum_of_Squared_Residuals <- sum((Horizontal_Points$Standardized_Response_Variable) ^ 2)
      Vertical_Sum_of_Squared_Residuals <- sum((Vertical_Points$Standardized_Predictor_Variable - Standardized_Horizontal_Axis_Coordinates[i]) ^ 2)
      Overall_Sum_of_Squared_Residuals <- Horizontal_Sum_of_Squared_Residuals + Vertical_Sum_of_Squared_Residuals
      c(Horizontal_Axis_Coordinate = Horizontal_Axis_Coordinates[i], Angle = Angles[j], Overall_Sum_of_Squared_Residual = Overall_Sum_of_Squared_Residuals)
    })))
  }))
  Negative_Critical_Value <- Overall_Sum_of_Squared_Residuals_for_Negative_Points[which.min(Overall_Sum_of_Squared_Residuals_for_Negative_Points$Overall_Sum_of_Squared_Residual), ]
  Critical_Values <- c(Positive_Critical_Value = Positive_Critical_Value$Horizontal_Axis_Coordinate, Negative_Critical_Value = Negative_Critical_Value$Horizontal_Axis_Coordinate)
  Predictor_Variable_Bins$Confidence_Interval_Positive_Predictor_Variable_Values_Only <- setNames(sapply(seq_len(nrow(Predictor_Variable_Bins)), function (i) {
    quantile(Positive_Predictor_Variable_Values_Only$Response_Variable[which((Positive_Predictor_Variable_Values_Only$Predictor_Variable > Predictor_Variable_Bins$Starting_Predictor_Variable_Value[i]) & (Positive_Predictor_Variable_Values_Only$Predictor_Variable < Predictor_Variable_Bins$Ending_Predictor_Variable_Value[i]))], Threshold_1)
  }), NULL)
  Predictor_Variable_Bins$Confidence_Interval_Positive_Predictor_Variable_Values_Only[which(is.na(Predictor_Variable_Bins$Confidence_Interval_Positive_Predictor_Variable_Values_Only))] <- 0
  Significant_Positive_Response_Variable_Values <- setNames(quantile(Predictor_Variable_Bins$Confidence_Interval_Positive_Predictor_Variable_Values_Only, Threshold_2), NULL)
  Predictor_Variable_Bins$Significant_Positive_Response_Variable_Value <- Predictor_Variable_Bins$Confidence_Interval_Positive_Predictor_Variable_Values_Only > Significant_Positive_Response_Variable_Values
  Predictor_Variable_Bins$Confidence_Interval_Negative_Predictor_Variable_Values_Only <- setNames(sapply(seq_len(nrow(Predictor_Variable_Bins)), function (i) {
    quantile(Negative_Predictor_Variable_Values_Only$Response_Variable[which((Negative_Predictor_Variable_Values_Only$Predictor_Variable > Predictor_Variable_Bins$Starting_Predictor_Variable_Value[i]) & (Negative_Predictor_Variable_Values_Only$Predictor_Variable < Predictor_Variable_Bins$Ending_Predictor_Variable_Value[i]))], (1 - Threshold_1))
  }), NULL)
  Predictor_Variable_Bins$Confidence_Interval_Negative_Predictor_Variable_Values_Only[which(is.na(Predictor_Variable_Bins$Confidence_Interval_Negative_Predictor_Variable_Values_Only))] <- 0
  Significant_Negative_Response_Variable_Values <- setNames(quantile(Predictor_Variable_Bins$Confidence_Interval_Negative_Predictor_Variable_Values_Only, (1 - Threshold_2)), NULL)
  Predictor_Variable_Bins$Significant_Negative_Response_Variable_Value <- Predictor_Variable_Bins$Confidence_Interval_Negative_Predictor_Variable_Values_Only < Significant_Negative_Response_Variable_Values
  Positive_Predictor_Variable_Values_Only_Run_Length_Encoding <- rle(Predictor_Variable_Bins$Significant_Positive_Response_Variable_Value)
  Positive_Predictor_Variable_Values_Only_Run_Length_Encoding <- data.frame(Lengths = Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$lengths, Values = Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$values)
  Negative_Predictor_Variable_Values_Only_Run_Length_Encoding <- rle(Predictor_Variable_Bins$Significant_Negative_Response_Variable_Value)
  Negative_Predictor_Variable_Values_Only_Run_Length_Encoding <- data.frame(Lengths = Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$lengths, Values = Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$values)
  Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers <- cumsum(Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Lengths)
  Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Starting_Row_Numbers <- c(1, Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers[-length(Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers)])
  Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers <- cumsum(Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Lengths)
  Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Starting_Row_Numbers <- c(1, Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers[-length(Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers)])
  Positive_Predictor_Variable_Ranges <- sapply(seq_len(nrow(Positive_Predictor_Variable_Values_Only_Run_Length_Encoding)), function (i) {
    if (Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Values[i] == T) {
      c(Predictor_Variable_Bins$Starting_Predictor_Variable_Value[(Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Starting_Row_Numbers[i]) + 1], Predictor_Variable_Bins$Ending_Predictor_Variable_Value[Positive_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers[i]])
    }
  })
  Positive_Predictor_Variable_Ranges <- Positive_Predictor_Variable_Ranges[which(!sapply(Positive_Predictor_Variable_Ranges, is.null))]
  Positive_Predictor_Variable_Ranges <- setNames(as.data.frame(do.call("rbind", Positive_Predictor_Variable_Ranges)), c("Starting_Value", "Ending_Value"))
  Negative_Predictor_Variable_Ranges <- sapply(seq_len(nrow(Negative_Predictor_Variable_Values_Only_Run_Length_Encoding)), function (i) {
    if (Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Values[i] == T) {
      c(Predictor_Variable_Bins$Starting_Predictor_Variable_Value[(Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Starting_Row_Numbers[i]) + 1], Predictor_Variable_Bins$Ending_Predictor_Variable_Value[Negative_Predictor_Variable_Values_Only_Run_Length_Encoding$Ending_Row_Numbers[i]])
    }
  })
  Negative_Predictor_Variable_Ranges <- Negative_Predictor_Variable_Ranges[which(!sapply(Negative_Predictor_Variable_Ranges, is.null))]
  Negative_Predictor_Variable_Ranges <- setNames(as.data.frame(do.call("rbind", Negative_Predictor_Variable_Ranges)), c("Starting_Value", "Ending_Value"))
  list(Critical_Predictor_Variable_Values = list(Positive_Critical_Predictor_Variable_Value = Positive_Critical_Value$Horizontal_Axis_Coordinate, Negative_Critical_Predictor_Variable_Value = Negative_Critical_Value$Horizontal_Axis_Coordinate), Predictor_Variable_Ranges = list(Positive_Predictor_Variable_Ranges = Positive_Predictor_Variable_Ranges, Negative_Predictor_Variable_Ranges = Negative_Predictor_Variable_Ranges))
}

# Importing and Visualizing Sugar Maple Data
if (!require (devtools)) {
  install.packages("devtools")
}
library (devtools)
devtools::install_github("davidblakneymoore/DBM.functions")
par(mar = c(5, 5, 4, 2))
plot(Sap_Flow ~ Wood_Temperature, DBM.functions::Sugar_Maple_Data, main = 'Sugar Maple Wood Temperature and Sap Flow Data', xlab = 'Wood Temperature', ylab = 'Sap Flow', pch = 19)
abline(h = 0, col = 2, lwd = 2.5)

# Using the Function on This Sugar Maple Data
Output <- Determining_Predictor_Variable_Values_and_Ranges_of_Values_Corresponding_to_Upward_and_Downward_Spikes_in_Response_Variable_Values(Wood_Temperature, Sap_Flow, DBM.functions::Sugar_Maple_Data)

# Generating a Figure Showing the Estimated Critical Wood Temperatures and Temperature Ranges Associated With Significant Sap Flows for This Sugar Maple Data
Horizontal_Text_Shifting_Constant_1 <- 0.425
Horizontal_Text_Shifting_Constant_2 <- 0.225
Vertical_Text_Shifting_Constant <- 0.25
par(mar = c(5, 5, 4, 2))
plot(Sap_Flow ~ Wood_Temperature, DBM.functions::Sugar_Maple_Data, main = 'Sugar Maple Wood Temperature and Sap Flow\nNumber of Bins: 100', xlab = 'Wood Temperature', ylab = 'Sap Flow', type = "n")
points(Sap_Flow ~ Wood_Temperature, DBM.functions::Sugar_Maple_Data[which(DBM.functions::Sugar_Maple_Data$Sap_Flow > 0), ], pch = 20, col = 2)
points(Sap_Flow ~ Wood_Temperature, DBM.functions::Sugar_Maple_Data[which(DBM.functions::Sugar_Maple_Data$Sap_Flow < 0), ], pch = 20, col = 3)
lapply(seq_len(nrow(Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges)), function (i) {
  polygon(c(Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Starting_Value[i], Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Ending_Value[i], Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Ending_Value[i], Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Starting_Value[i], Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Starting_Value[i]), c(par("usr")[4], par("usr")[4], 0, 0, par("usr")[4]), col = rgb(1, 0, 0, 0.5))
})
lapply(seq_len(nrow(Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges)), function (i) {
  polygon(c(Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Starting_Value[i], Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Ending_Value[i], Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Ending_Value[i], Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Starting_Value[i], Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Starting_Value[i]), c(0, 0, par("usr")[3], par("usr")[3], 0), col = rgb(0, 1, 0, 0.5))
})
abline(h = 0)
text(((1 - Horizontal_Text_Shifting_Constant_1) * par("usr")[1]) + (Horizontal_Text_Shifting_Constant_1 * par("usr")[2]), (Vertical_Text_Shifting_Constant * par("usr")[3]) + ((1 - Vertical_Text_Shifting_Constant) * par("usr")[4]), paste("Temperatures of Positive Sap Flow (˚ C):", Reduce(function (a, b) {
  paste(a, b, sep = "\n")
}, lapply(seq_len(nrow(Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges)), function (i) {
  paste0(round(Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Starting_Value[i], 3), " to ", round(Output$Predictor_Variable_Ranges$Positive_Predictor_Variable_Ranges$Ending_Value[i], 3))
})), sep = "\n"), col = 2)
text(((1 - Horizontal_Text_Shifting_Constant_1) * par("usr")[1]) + (Horizontal_Text_Shifting_Constant_1 * par("usr")[2]), ((1 - Vertical_Text_Shifting_Constant) * par("usr")[3]) + (Vertical_Text_Shifting_Constant * par("usr")[4]), paste("Temperatures of Negative Sap Flow (˚ C):", Reduce(function (a, b) {
  paste(a, b, sep = "\n")
}, lapply(seq_len(nrow(Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges)), function (i) {
  paste0(round(Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Starting_Value[i], 3), " to ", round(Output$Predictor_Variable_Ranges$Negative_Predictor_Variable_Ranges$Ending_Value[i], 3))
})), sep = "\n"), col = 3)
segments(Output$Critical_Predictor_Variable_Values$Positive_Critical_Predictor_Variable_Value, 0, Output$Critical_Predictor_Variable_Values$Positive_Critical_Predictor_Variable_Value, par("usr")[4], col = 1, lwd = 2.5)
segments(Output$Critical_Predictor_Variable_Values$Negative_Critical_Predictor_Variable_Value, 0, Output$Critical_Predictor_Variable_Values$Negative_Critical_Predictor_Variable_Value, par("usr")[3], col = 1, lwd = 2.5)
text((Horizontal_Text_Shifting_Constant_2 * par("usr")[1]) + ((1 - Horizontal_Text_Shifting_Constant_2) * par("usr")[2]), (Vertical_Text_Shifting_Constant * par("usr")[3]) + ((1 - Vertical_Text_Shifting_Constant) * par("usr")[4]), paste0("Positive Critical Temperature:\n", round(Output$Critical_Predictor_Variable_Values$Positive_Critical_Predictor_Variable_Value, 3), " ˚ C"), col = 2)
text((Horizontal_Text_Shifting_Constant_2 * par("usr")[1]) + ((1 - Horizontal_Text_Shifting_Constant_2) * par("usr")[2]), ((1 - Vertical_Text_Shifting_Constant) * par("usr")[3]) + (Vertical_Text_Shifting_Constant * par("usr")[4]), paste0("Negative Critical Temperature:\n", round(Output$Critical_Predictor_Variable_Values$Negative_Critical_Predictor_Variable_Value, 3), " ˚ C"), col = 3)

# Explanatory Figures
Positive_Response_Variable_Values_Only <- DBM.functions::Sugar_Maple_Data[which(DBM.functions::Sugar_Maple_Data$Sap_Flow > 0), ]
Negative_Response_Variable_Values_Only <- DBM.functions::Sugar_Maple_Data[which(DBM.functions::Sugar_Maple_Data$Sap_Flow < 0), ]

# Determination of Critical Values
Horizontal_Axis_Coordinate <- seq(min(DBM.functions::Sugar_Maple_Data$Wood_Temperature), max(DBM.functions::Sugar_Maple_Data$Wood_Temperature), length.out = 7)
Horizontal_Axis_Coordinate <- Horizontal_Axis_Coordinate[which((seq_along(Horizontal_Axis_Coordinate) %% 2) == 0)]
Angle <- seq(0, (pi / 2), length.out = 7)
Angle <- Angle[which((seq_along(Angle) %% 2) == 0)]
Top_Matrix <- matrix(c(1, 1, 1, 1, 0, 2, 3, 4, 5, 8, 9, 10, 6, 11, 12, 13, 7, 14, 15, 16), byrow = T, ncol = 4)
Bottom_Matrix <- matrix(c(1, 1, 1, 1, 0, 2, 3, 4, 5, 8, 9, 10, 6, 11, 12, 13, 7, 14, 15, 16), byrow = T, ncol = 4)
Bottom_Matrix[which(Bottom_Matrix != 0)] <- Bottom_Matrix[which(Bottom_Matrix != 0)] + max(Top_Matrix) + 1
Final_Layout_Matrix <- cbind(Top_Matrix, rep((max(Top_Matrix) + 1), nrow(Top_Matrix)), Bottom_Matrix)
layout(Final_Layout_Matrix, heights = c(4, 4, 14, 14, 14), widths = c(7, 25, 25, 25, 3, 7, 25, 25, 25))
par(mar = c(1, 1, 1, 1))
plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
text(0, 0, expression(paste(bold("Positive Response-Variable Values Only"))), cex = 2.5)
lapply(1:3, function (j) {
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
  text(0, 0, paste0("Angle:\n", round(Angle[j], 3)), cex = 1.5)
})
lapply(1:3, function (i) {
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
  text(0, 0, paste0("Predictor-Variable Value:\n", round(Horizontal_Axis_Coordinate[i], 3)), cex = 1.5, srt = 90)
})
par(mar = c(5, 5, 2, 2))
lapply(1:3, function (i) {
  lapply(1:3, function (j) {
    plot(Sap_Flow ~ Wood_Temperature, Positive_Response_Variable_Values_Only, type = "n", xlab = "Wood Temperature", ylab = "Sap Flow", cex.axis = 1.25, cex.lab = 1.25)
    Indices <- ifelse((Positive_Response_Variable_Values_Only$Sap_Flow > ((tan(Angle[j]) * Positive_Response_Variable_Values_Only$Wood_Temperature) - (Horizontal_Axis_Coordinate[i] * tan(Angle[j])))) & (Positive_Response_Variable_Values_Only$Sap_Flow > ((-tan(Angle[j]) * Positive_Response_Variable_Values_Only$Wood_Temperature) + (Horizontal_Axis_Coordinate[i] * tan(Angle[j])))), T, F)
    Vertical_Points <- Positive_Response_Variable_Values_Only[which(Indices), ]
    Horizontal_Points <- Positive_Response_Variable_Values_Only[which(!Indices), ]
    points(Vertical_Points$Wood_Temperature, Vertical_Points$Sap_Flow, pch = 19, col = 2)
    points(Horizontal_Points$Wood_Temperature, Horizontal_Points$Sap_Flow, pch = 19, col = 3)
    segments(Horizontal_Axis_Coordinate[i], 0, Horizontal_Axis_Coordinate[i], par("usr")[4], lty = 2, lwd = 2.5, col = "darkred")
    segments(par("usr")[1], 0, par("usr")[2], 0, lty = 2, lwd = 2.5, col = "darkgreen")
    Slope_1 <- tan(Angle[j])
    Intercept_1 <- -(Slope_1 * Horizontal_Axis_Coordinate[i])
    if (((Slope_1 * par("usr")[2]) + Intercept_1) >= par("usr")[4]) {
      segments(Horizontal_Axis_Coordinate[i], 0, ((par("usr")[4] - Intercept_1) / Slope_1), par("usr")[4])
    } else if (((Slope_1 * par("usr")[2]) + Intercept_1) <= par("usr")[4]) {
      segments(Horizontal_Axis_Coordinate[i], 0, par("usr")[2], ((Slope_1 * par("usr")[2]) + Intercept_1))
    }
    Slope_2 <- -tan(Angle[j])
    Intercept_2 <- -(Slope_2 * Horizontal_Axis_Coordinate[i])
    if (((Slope_2 * par("usr")[1]) + Intercept_2) >= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, par("usr")[1], ((Slope_2 * par("usr")[1]) + Intercept_2))
    } else if (((Slope_2 * par("usr")[1]) + Intercept_2) <= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, ((par("usr")[3] - Intercept_2) / Slope_2), par("usr")[3])
    }
  })
})
par(mar = c(1, 1, 1, 1))
plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
abline(v = 0)
plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
text(0, 0, expression(paste(bold("Negative Response-Variable Values Only"))), cex = 2.5)
lapply(1:3, function (j) {
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
  text(0, 0, paste0("Angle:\n", round(Angle[j], 3)), cex = 1.5)
})
lapply(1:3, function (i) {
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
  text(0, 0, paste0("Predictor-Variable Value:\n", round(Horizontal_Axis_Coordinate[i], 3)), cex = 1.5, srt = 90)
})
par(mar = c(5, 5, 2, 2))
lapply(1:3, function (i) {
  lapply(1:3, function (j) {
    plot(Sap_Flow ~ Wood_Temperature, Negative_Response_Variable_Values_Only, type = "n", xlab = "Wood Temperature", ylab = "Sap Flow", cex.axis = 1.25, cex.lab = 1.25)
    Indices <- ifelse((Negative_Response_Variable_Values_Only$Sap_Flow < ((tan(Angle[j]) * Negative_Response_Variable_Values_Only$Wood_Temperature) - (Horizontal_Axis_Coordinate[i] * tan(Angle[j])))) & (Negative_Response_Variable_Values_Only$Sap_Flow < ((-tan(Angle[j]) * Negative_Response_Variable_Values_Only$Wood_Temperature) + (Horizontal_Axis_Coordinate[i] * tan(Angle[j])))), T, F)
    Vertical_Points <- Negative_Response_Variable_Values_Only[which(Indices), ]
    Horizontal_Points <- Negative_Response_Variable_Values_Only[which(!Indices), ]
    points(Vertical_Points$Wood_Temperature, Vertical_Points$Sap_Flow, pch = 19, col = 2)
    points(Horizontal_Points$Wood_Temperature, Horizontal_Points$Sap_Flow, pch = 19, col = 3)
    segments(Horizontal_Axis_Coordinate[i], 0, Horizontal_Axis_Coordinate[i], par("usr")[3], lty = 2, lwd = 2.5, col = "darkred")
    segments(par("usr")[1], 0, par("usr")[2], 0, lty = 2, lwd = 2.5, col = "darkgreen")
    Slope_1 <- tan(Angle[j])
    Intercept_1 <- -(Slope_1 * Horizontal_Axis_Coordinate[i])
    if (((Slope_1 * par("usr")[1]) + Intercept_1) >= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, ((par("usr")[3] - Intercept_1) / Slope_1), par("usr")[3])
    } else if (((Slope_1 * par("usr")[1]) + Intercept_1) <= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, par("usr")[1], ((Slope_1 * par("usr")[1]) + Intercept_1))
    }
    Slope_2 <- -tan(Angle[j])
    Intercept_2 <- -(Slope_2 * Horizontal_Axis_Coordinate[i])
    if (((Slope_2 * par("usr")[2]) + Intercept_2) >= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, par("usr")[2], ((Slope_2 * par("usr")[2]) + Intercept_2))
    } else if (((Slope_2 * par("usr")[2]) + Intercept_2) <= par("usr")[3]) {
      segments(Horizontal_Axis_Coordinate[i], 0, ((par("usr")[3] - Intercept_2) / Slope_2), par("usr")[3])
    }
  })
})

# Determination of Predictor-Variable Ranges Corresponding With Significant Response-Variable Values
layout(matrix(c(0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 7, 13, 14, 15, 16, 17), byrow = T, ncol = 6, nrow = 3), heights = c(1, 5, 5), widths = c(1, 5, 5, 5, 5, 5))
par(mar = c(1, 1, 1, 1))
for (i in 1:5) {
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
  text(0, 0, paste("Step", i), cex = 2)
}
plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
text(0, 0, "Positive Sap-Flow Values Only", cex = 2, srt = 90)
plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
text(0, 0, "Negative Sap-Flow Values Only", cex = 2, srt = 90)
par(mar = c(5, 5, 2, 2))
for (k in 1:5) {
  plot(Sap_Flow ~ Wood_Temperature, Positive_Response_Variable_Values_Only, pch = 19, xlab = "Wood Temperature", ylab = "Sap Flow", cex.axis = 1.25, cex.lab = 1.25)
  if (k == 1) {
    next
  }
  Predictor_Variable_Increments <- seq(min(Positive_Response_Variable_Values_Only$Wood_Temperature), max(Positive_Response_Variable_Values_Only$Wood_Temperature), length.out = (10 + 1))
  segments(min(Positive_Response_Variable_Values_Only$Wood_Temperature), 0, max(Positive_Response_Variable_Values_Only$Wood_Temperature), 0, col = 2)
  lapply(seq_along(Predictor_Variable_Increments), function (i) {
    segments(Predictor_Variable_Increments[i], 0, Predictor_Variable_Increments[i], par("usr")[4], col = 2)
  })
  if (k == 2) {
    next
  }
  Quantiles <- sapply(seq_len(length(Predictor_Variable_Increments) - 1), function (i) {
    Data_Subset <- Positive_Response_Variable_Values_Only[which((Positive_Response_Variable_Values_Only$Wood_Temperature > Predictor_Variable_Increments[i]) & (Positive_Response_Variable_Values_Only$Wood_Temperature < Predictor_Variable_Increments[(i + 1)])), ]
    quantile(Data_Subset$Sap_Flow, 0.95)
  })
  Quantiles[which(is.na(Quantiles))] <- 0
  lapply(seq_along(Quantiles), function (i) {
    segments(Predictor_Variable_Increments[i], Quantiles[i], Predictor_Variable_Increments[(i + 1)], Quantiles[i], col = 2)
    polygon(c(Predictor_Variable_Increments[i], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[i], Predictor_Variable_Increments[i]), c(Quantiles[i], Quantiles[i], 0, 0, Quantiles[i]), col = rgb(1, 0, 0, 0.25), border = NA)
  })
  if (k == 3) {
    next
  }
  Threshold <- quantile(Quantiles, 0.95)
  lapply(seq_along(Quantiles), function (i) {
    if (Quantiles[i] > Threshold) {
      polygon(c(Predictor_Variable_Increments[i], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[i], Predictor_Variable_Increments[i]), c(Quantiles[i], Quantiles[i], 0, 0, Quantiles[i]), col = rgb(1, 0, 0, 0.625), border = NA)
    }
  })
  if (k == 4) {
    next
  }
  Run_Length_Encoding <- rle(Quantiles > Threshold)
  Run_Length_Encoding <- data.frame(Lengths = Run_Length_Encoding$lengths, Values = Run_Length_Encoding$values)
  Run_Length_Encoding$Ending_Row_Numbers <- cumsum(Run_Length_Encoding$Lengths)
  Run_Length_Encoding$Starting_Row_Numbers <- c(1, Run_Length_Encoding$Ending_Row_Numbers[-length(Run_Length_Encoding$Ending_Row_Numbers)])
  Predictor_Variable_Bins <- data.frame(Starting_Predictor_Variable_Value = Predictor_Variable_Increments[-length(Predictor_Variable_Increments)], Ending_Predictor_Variable_Value = Predictor_Variable_Increments[-1])
  Predictor_Variable_Ranges <- sapply(seq_len(nrow(Run_Length_Encoding)), function (i) {
    if (Run_Length_Encoding$Values[i] == T) {
      c(Predictor_Variable_Bins$Starting_Predictor_Variable_Value[(Run_Length_Encoding$Starting_Row_Numbers[i]) + 1], Predictor_Variable_Bins$Ending_Predictor_Variable_Value[Run_Length_Encoding$Ending_Row_Numbers[i]])
    }
  })
  Predictor_Variable_Ranges <- Predictor_Variable_Ranges[which(!sapply(Predictor_Variable_Ranges, is.null))]
  Text <- paste0("Ranges of Wood\nTemperatures\nCorresponding to\nSignificantly Large\nSap Flows:\n", Reduce(function (a, b) {
    paste(a, b, sep = "\n")
  }, lapply(Predictor_Variable_Ranges, function (x) {
    paste(round(x[1], 3), " to ", round(x[2], 3))
  })))
  Text_Box_Center <- c((((1 / 3) * par("usr")[1]) + ((1 - (1 / 3)) * par("usr")[2])), (((1 / 3) * par("usr")[3]) + ((1 - (1 / 3)) * par("usr")[4])))
  Horizontal_Text_Box_Scaling_Factor <- 0.25 * diff(par("usr")[1:2]) / par("pin")[1]
  Vertical_Text_Box_Scaling_Factor <- 0.25 * diff(par("usr")[3:4]) / par("pin")[2]
  Text_Box_Width <- strwidth(Text) + Horizontal_Text_Box_Scaling_Factor
  Text_Box_Height <- strheight(Text) + Vertical_Text_Box_Scaling_Factor
  polygon(c(Text_Box_Center[1] - (Text_Box_Width / 2), Text_Box_Center[1] + (Text_Box_Width / 2), Text_Box_Center[1] + (Text_Box_Width / 2), Text_Box_Center[1] - (Text_Box_Width / 2), Text_Box_Center[1] - (Text_Box_Width / 2)), c(Text_Box_Center[2] + (Text_Box_Height / 2), Text_Box_Center[2] + (Text_Box_Height / 2), Text_Box_Center[2] - (Text_Box_Height / 2), Text_Box_Center[2] - (Text_Box_Height / 2), Text_Box_Center[2] + (Text_Box_Height / 2)), col = "white", border = 2)
  text(Text_Box_Center[1], Text_Box_Center[2], Text, col = 2)
}
for (k in 1:5) {
  plot(Sap_Flow ~ Wood_Temperature, Negative_Response_Variable_Values_Only, pch = 19, xlab = "Wood Temperature", ylab = "Sap Flow", cex.axis = 1.25, cex.lab = 1.25)
  if (k == 1) {
    next
  }
  Predictor_Variable_Increments <- seq(min(Negative_Response_Variable_Values_Only$Wood_Temperature), max(Negative_Response_Variable_Values_Only$Wood_Temperature), length.out = (10 + 1))
  segments(min(Negative_Response_Variable_Values_Only$Wood_Temperature), 0, max(Negative_Response_Variable_Values_Only$Wood_Temperature), 0, col = 2)
  lapply(seq_along(Predictor_Variable_Increments), function (i) {
    segments(Predictor_Variable_Increments[i], 0, Predictor_Variable_Increments[i], par("usr")[3], col = 2)
  })
  if (k == 2) {
    next
  }
  Quantiles <- sapply(seq_len(length(Predictor_Variable_Increments) - 1), function (i) {
    Data_Subset <- Negative_Response_Variable_Values_Only[which((Negative_Response_Variable_Values_Only$Wood_Temperature > Predictor_Variable_Increments[i]) & (Negative_Response_Variable_Values_Only$Wood_Temperature < Predictor_Variable_Increments[(i + 1)])), ]
    quantile(Data_Subset$Sap_Flow, 0.05)
  })
  Quantiles[which(is.na(Quantiles))] <- 0
  lapply(seq_along(Quantiles), function (i) {
    segments(Predictor_Variable_Increments[i], Quantiles[i], Predictor_Variable_Increments[(i + 1)], Quantiles[i], col = 2)
    polygon(c(Predictor_Variable_Increments[i], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[i], Predictor_Variable_Increments[i]), c(Quantiles[i], Quantiles[i], 0, 0, Quantiles[i]), col = rgb(1, 0, 0, 0.25), border = NA)
  })
  if (k == 3) {
    next
  }
  Threshold <- quantile(Quantiles, 0.05)
  lapply(seq_along(Quantiles), function (i) {
    if (Quantiles[i] < Threshold) {
      polygon(c(Predictor_Variable_Increments[i], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[(i + 1)], Predictor_Variable_Increments[i], Predictor_Variable_Increments[i]), c(Quantiles[i], Quantiles[i], 0, 0, Quantiles[i]), col = rgb(1, 0, 0, 0.625), border = NA)
    }
  })
  if (k == 4) {
    next
  }
  Run_Length_Encoding <- rle(Quantiles < Threshold)
  Run_Length_Encoding <- data.frame(Lengths = Run_Length_Encoding$lengths, Values = Run_Length_Encoding$values)
  Run_Length_Encoding$Ending_Row_Numbers <- cumsum(Run_Length_Encoding$Lengths)
  Run_Length_Encoding$Starting_Row_Numbers <- c(1, Run_Length_Encoding$Ending_Row_Numbers[-length(Run_Length_Encoding$Ending_Row_Numbers)])
  Predictor_Variable_Bins <- data.frame(Starting_Predictor_Variable_Value = Predictor_Variable_Increments[-length(Predictor_Variable_Increments)], Ending_Predictor_Variable_Value = Predictor_Variable_Increments[-1])
  Predictor_Variable_Ranges <- sapply(seq_len(nrow(Run_Length_Encoding)), function (i) {
    if (Run_Length_Encoding$Values[i] == T) {
      c(Predictor_Variable_Bins$Starting_Predictor_Variable_Value[(Run_Length_Encoding$Starting_Row_Numbers[i]) + 1], Predictor_Variable_Bins$Ending_Predictor_Variable_Value[Run_Length_Encoding$Ending_Row_Numbers[i]])
    }
  })
  Predictor_Variable_Ranges <- Predictor_Variable_Ranges[which(!sapply(Predictor_Variable_Ranges, is.null))]
  Text <- paste0("Ranges of Wood\nTemperatures\nCorresponding to\nSignificantly Large\nSap Flows:\n", Reduce(function (a, b) {
    paste(a, b, sep = "\n")
  }, lapply(Predictor_Variable_Ranges, function (x) {
    paste(round(x[1], 3), " to ", round(x[2], 3))
  })))
  Text_Box_Center <- c((((1 / 3) * par("usr")[1]) + ((1 - (1 / 3)) * par("usr")[2])), (((1 - (1 / 3)) * par("usr")[3]) + ((1 / 3) * par("usr")[4])))
  Horizontal_Text_Box_Scaling_Factor <- 0.25 * diff(par("usr")[1:2]) / par("pin")[1]
  Vertical_Text_Box_Scaling_Factor <- 0.25 * diff(par("usr")[3:4]) / par("pin")[2]
  Text_Box_Width <- strwidth(Text) + Horizontal_Text_Box_Scaling_Factor
  Text_Box_Height <- strheight(Text) + Vertical_Text_Box_Scaling_Factor
  polygon(c(Text_Box_Center[1] - (Text_Box_Width / 2), Text_Box_Center[1] + (Text_Box_Width / 2), Text_Box_Center[1] + (Text_Box_Width / 2), Text_Box_Center[1] - (Text_Box_Width / 2), Text_Box_Center[1] - (Text_Box_Width / 2)), c(Text_Box_Center[2] + (Text_Box_Height / 2), Text_Box_Center[2] + (Text_Box_Height / 2), Text_Box_Center[2] - (Text_Box_Height / 2), Text_Box_Center[2] - (Text_Box_Height / 2), Text_Box_Center[2] + (Text_Box_Height / 2)), col = "white", border = 2)
  text(Text_Box_Center[1], Text_Box_Center[2], Text, col = 2)
}

# First-Order Hermite Function Figure
Horizontal_Axis_Variable <- seq(-10, 10, 0.01)
Vertical_Axis_Variable <- sqrt(2) * (pi ^ (-0.25)) * Horizontal_Axis_Variable * exp(-0.5 * (Horizontal_Axis_Variable ^ 2))
plot(Horizontal_Axis_Variable, Vertical_Axis_Variable, pch = 19, xlab = "Wood Temperature", ylab = "Sap Flow", main = expression(paste("1" ^ "st" * "-Order Hermite Function")))
abline(h = 0, col = 2, lwd = 2)
abline(v = 0, col = 3, lwd = 2)
