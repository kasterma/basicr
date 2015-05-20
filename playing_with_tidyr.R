library(dplyr)
library(tidyr)

(df1 <- data.frame(groupid = c("one","one","one","two","two","two", "one"),
                   value = c(3,2,1,2,3,1,22),
                   itemid = c(1:6, 6)))

(df4 <- data.frame(groupid = c("one","one","one","two","two","two", "one", "one"),
                   value = c(3,2,1,2,3,1,22,33),
                   itemid = c(1:6, 6 ,6)))

df1 %>% group_by(id) %>% arrange(y) %>% mutate(cc = cumsum(y))

(df2 <- data.frame(id = rep(paste0("week", 1:2), each = 3),
                   y = c(3,2,1,2,3,1),
                   z = 1:6))
df2 %>% separate(id, c("week", "no"), sep = 4)

(df3 <- data.frame(id = c(1,1,1,2,2,2), y = c(3,2,1,2,3,1), z = 1:6))

df3 %>% spread(id,y) %>% dput
df3 %>% spread(id,y) %>% gather(id, y, `1`, `2`)


df1 %>% spread(groupid, value)
df1 %>% spread(groupid, value, fill = 0)
df1 %>% spread(groupid, value, fill = 0) %>% gather(groupid, value, one, two)

df4 %>% spread(groupid, value, fill = 0) %>% gather(groupid, value, one, two)
df4 %>% spread(groupid, value, fill = 0)

#' Add default values for missing groups
#' 
#' Given data about items where each item is identified by an id, and every
#' item can have a value in every group; add a default value for all groups
#' where an item doesn't have a value yet.
add_default_value <- function(data, id, group, value, default) {
  id = as.character(substitute(id))
  group = as.character(substitute(group))
  value = as.character(substitute(value))
  groups <- unique(as.character(data[[group]]))
  
  # spread checks that the columns outside of group and value uniquely
  # determine the row.  Here we check that that already is the case within
  # each group using only id.  I.e. there is no repeated (id, group).
  id_group_cts <- data %>% group_by_(id, group) %>% do(data.frame(.ct = nrow(.)))
  if (any(id_group_cts$.ct > 1)) {
    badline <- id_group_cts %>% filter(.ct > 1) %>% top_n(1, .ct)
    stop("There is at least one (", id, ", ", group, ")",
         " combination with two members: (",
         as.character(badline[[id]]), ", ", as.character(badline[[group]]), ")")
  }
  
  gather_(spread_(data, group, value, fill = default), group, value, groups)
}