# Creating demographics table

# Load the dataset
load("~/Dropbox/ENIGMA/imp_random_06_caret.RData")

# Creating demographics tables
# Generate tables for Sex distribution by Site_name
imp_test_cont_tab <- table(imp_test_cont$Site_name, imp_test_cont$Sex)
imp_train_cont_tab <- table(imp_train_cont$Site_name, imp_train_cont$Sex)
imp_test_MDD_tab <- table(imp_test_MDD$Site_name, imp_test_MDD$Sex)

# Save the tables to CSV files
write.csv(imp_test_cont_tab, file = "imp_test_cont_tab.csv")
write.csv(imp_test_MDD_tab, file = "imp_test_MDD_tab.csv")
write.csv(imp_train_cont_tab, file = "imp_train_cont_tab.csv")

# Re-calculating the age variable
# Generate tables for Age distribution by Site_name
imp_test_cont_age <- table(imp_test_cont$Site_name, imp_test_cont$Age)
imp_train_cont_age <- table(imp_train_cont$Site_name, imp_train_cont$Age)
imp_test_MDD_age <- table(imp_test_MDD$Site_name, imp_test_MDD$Age)

# (Optional) Save the age distribution tables to CSV files if needed
# write.csv(imp_test_cont_age, file = "01_imp_test_cont_age.csv")
# write.csv(imp_train_cont_age, file = "01_imp_train_cont_age.csv")
# write.csv(imp_test_MDD_age, file = "01_imp_test_MDD_age.csv")

#final table in 