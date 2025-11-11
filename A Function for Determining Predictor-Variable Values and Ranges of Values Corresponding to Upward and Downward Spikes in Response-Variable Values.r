
# A Function for Determining Predictor-Variable Values and Ranges of Values Corresponding to Upward and Downward Spikes in Response-Variable Values

# David Moore
# November 2025
# davidblakneymoore@gmail.com


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
