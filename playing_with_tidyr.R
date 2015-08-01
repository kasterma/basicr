library(dplyr)
library(tidyr)

(df1 <- data.frame(groupid = c("one","one","one","two","two","two", "one"),
                   value = c(3,2,1,2,3,1,22),
                   itemid = c(1:6, 6)))

complete(df1, groupid, itemid)

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
