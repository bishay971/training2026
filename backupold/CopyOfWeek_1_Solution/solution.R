#' ---
#' title: Solution
#' author: 'R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)'
#' copyright: © 2025. Unauthorized distribution or reuse prohibited.
#' ---
#' 

#1
name <- "John"
age <- 28
weight_kg <- 70.5
is_smoker <- FALSE

#2
age + 5
weight_kg * 2.2
age > 18
is_smoker == TRUE

#3
sex <- "M"

# Classification
if_else(sex == "M", "Male", "Female")

#4
# Switch case
plan <- "premium"

case_when(
        plan == "basic" ~ "Basic Plan: $100/month",
        plan == "premium" ~ "Premium Plan: $200/month",
        plan == "vip" ~ "VIP Plan: $500/month"
        )

#5
# Countdown from 10 to 1
for (i in 10:1) {
  print(paste("T-minus", i))
}

# Print launch message after the countdown
print("🚀 Lift off!")


#6
bmi_calc <- function(weight, height) {
  bmi <- weight / (height^2)
  return(bmi)
}

bmi_calc(70, 1.75)

#7
bp <- c(120, 125, 118, 135, 110)
mean(bp)
bp[bp > 120]

#8
patient <- list(
  name = "John",
  age = 28,
  cholesterol = c(200, 180, 190)
)

#9
patient$name
patient$age
patient$cholesterol

#9
df <- data.frame(
  Name = c("John", "Jane", "Alice"),
  Age = c(28, 35, 40),
  Smoker = c(FALSE, TRUE, FALSE)
)

str(df)
df[df$Age > 30, ]


#10
data(cars)

write_xpt(cars, "data/cars.xpt", version=5)
df_read <- read_xpt("data/cars.xpt")

