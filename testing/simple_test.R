A <- data.frame(id = 1:10)
A$groupA_x <- letters[1:10]
A$groupB_x <- LETTERS[1:10]

tab <- df_to_table(A)
class(tab)
tab <- add_brace(tab, "groupA_x", "groupB_x", "")
class(tab)
print(tab)
tab <- set_column_format(tab, "id", "%s")
print(tab)

tab <- set_column_label(tab, "id", "identifier")
str(tab)

tab <- set_column_name(tab, c("groupA_x", "groupB_x"), c("A_x", "B_x"))
str(tab)

tab <- add_space(tab, "A_x")
print(tab)
