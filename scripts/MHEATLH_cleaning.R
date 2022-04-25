t <-
  read.table("data_raw/mHealth_subject1.log",
             sep = "\t")

# Clean dataset, renaming columns with appropriate names 
# Ankle vs arm 
# acc = accelerometer, gyro = gyroscope, mag = magnetometer, ecg =electrocardiogram
# x,y,z = Different axis of the same device

dat <- t %>%  
  rename(chest_x = V1,
         chest_y = V2,
         chest_z = V3,
         ecg_1 = V4,
         ecg_2 = V5,
         ankle_acc_x = V6,
         ankle_acc_y = V7,
         ankle_acc_z = V8,
         ankle_gyro_x = V9,
         ankle_gyro_y = V10,
         ankle_gyro_z = V11,
         ankle_mag_x = V12,
         ankle_mag_y = V13,
         ankle_mag_z = V14,
         arm_acc_x  = V15,
         arm_acc_y  = V16,
         arm_acc_z  = V17,
         arm_gyro_x = V18,
         arm_gyro_y = V19,
         arm_gyro_z = V20,
         arm_mag_x  = V21,
         arm_mag_y  = V22,
         arm_mag_z  = V23,
         activity = V24
  ) %>%
  # Remove null class, as over represented
  filter(activity > 0)

# Compare across different activities
# activity == 1 ; standing still (1min)
filter(dat, activity == 1) %>% apply(2, mean)

# activity == 11 ; running (1min)
filter(dat, activity == 11) %>% apply(2, mean)

write.csv(dat, "data/MHEALTH_cleaned.csv",row.names = F)
