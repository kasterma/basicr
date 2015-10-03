library(rvest)
library(tidyr)
library(stringi)

get_bol_price <- function(url) {
  read_html(url) %>%
    html_nodes(".price-big [itemprop=\"price\"]") %>%
    html_attr("content") %>%
    sub(",", ".", .) %>%
    as.numeric
}

get_bol_price("http://www.bol.com/nl/p/nerf-n-strike-elite-retaliator-xd-blaster/9200000038630197/?bltg=itm_event%3dclick%26pg_nm%3dmain%26slt_id%3d303%26slt_nm%3dFlexblocks-row-2%26slt_pos%3dB7%26slt_owner%3dbm%26itm_type%3dproduct%26itm_lp%3d4%26itm_id%3d9200000038630197%26itm_role%3din&promo=main_303_Flexblocks-row-2_B7_product_4_9200000038630197")
get_bol_price("http://www.bol.com/nl/p/nerf-n-strike-elite-stryfe-blaster/1004004013346709/?bltg=itm_event%3dclick%26pg_nm%3dpdp%26slt_id%3dprd_reco%26slt_nm%3dproduct_recommendations%26slt_pos%3dC1%26slt_owner%3dccs%26itm_type%3dproduct%26itm_lp%3d2%26itm_id%3d1004004013346709%26itm_role%3din")

get_fietsslot_nl_price <- function(url) {
  read_html(url) %>%
    html_nodes(".productInfo .price") %>%
    html_text %>%
    stri_extract_all(regex = "[0-9,]+") %>%
    sub(",", ".", .) %>%
    as.numeric
}

get_fietsslot_nl_price("http://www.fietsslot.nl/axa-kettingslot-axa-cherto-100cm-art-2.html")
