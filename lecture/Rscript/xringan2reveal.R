#' Loop detect text list and get its range index between the prefix string (regex pattern) and
#'   the suffix string (regex pattern) .
#'
#' @param txt vector. List of string rows from the Rmarkdown/Qmarkdown text file.
#' @param ptn_prefix character.Regex pattern for the target the prefix string.
#' @param ptn_suffix character.Regex pattern for the target the prefix string.
#'
#' @return data.frame. 
#' @export detect_seq
#'
#' @examples
#' tbl_range <- detect_seq(
#'   txt = tbl_text$text, 
#'   ptn_prefix = "^\\.footnote\\[",
#'   ptn_suffix = "^\\]$"
#'   )
#'   

detect_seq <- function(txt, ptn_prefix, ptn_suffix){
  #txt <- tbl_text$text
  len_txt <- length(txt)
  
  set_start <- NULL
  set_end <- NULL
  index <- 0
  # 循环识别目标区域
  # i <- 885
  # j <- 887
  for (i in 1:len_txt) {
    start_pos <- str_detect(txt[i], ptn_prefix)
    if (start_pos) {
      index <- index +1
      for (j in (i +1):len_txt){
        end_pos <- str_detect(txt[j], ptn_suffix)
        if (end_pos) {
          set_start[index] <- i
          set_end[index] <- j
          break
        }
      }
    }
  }
  
  ## 获得目标分组以及行序号
  k <- 1
  index_range <- NULL
  for (k in 1:length(set_start)){
    index_range[[k]] <- seq(set_start[k],set_end[k])
  }
  tbl_index <- tibble(
    range_id = 1:length(set_start), 
    row = index_range
  ) %>%
    unnest(row)
  
  return(tbl_index)
}

# file_qmd <- here("xaringan/SEM-slide-part0-endogeneity.qmd")


#' Convert Rmarkdown file (Xaringan slide) to Quarto file (Reveal slide)
#'
#' @param rmd character. Xaringan Rmarkdown file path.
#'
#' @return NULL
#' @export xaringan2reveal
#' 
#' @import data.table
#' @import magrittr
#' @import glue
#' @import fs
#' @import tidyverse
#' @import here
#' @import xfun
#' 
#' @examples
#' file_rmd <- here("slide-reveal/lecture-20-sem-estimation.Rmd")
#' xaringan2reveal(rmd = file_rmd)

xaringan2reveal <- function(rmd){
  # 包准备====
  library(tidyverse)
  library(knitr)
  library(fs)
  library(glue)
  library(here)
  library(xfun)
  library(magrittr)
  library(data.table)
  
  # 设定文件路径
  ext <- fs::path_ext(rmd)
  dir <- fs::path_dir(rmd)
  qmd_tem <- fs::file_temp(
    pattern = "xaringan2reveal",
    tmp_dir = dir,ext = "qmd"
  )
  qmd <- str_replace(
    file_rmd, 
    paste0("(?<=\\.)(", ext,"$)"),
    "qmd"
  )
  # source(here("code/fun-utilities.R"))
  
  # 代码块头参数转化 ====
  ## 一次运行即可
  knitr::convert_chunk_header(
    input = rmd,
    output = qmd_tem,
    type = "yaml")
  cat(
    glue("step success: tem file created at: \n {qmd_tem}. \n And R chunk header convert finished !"),
    collapse = "\n"
  )
  
  # ===读取qmd文件====
  text <- read_lines(qmd_tem)
  tbl_text <- tibble(text = text) %>%
    mutate(text = trimws(text)) %>% 
    add_column(row = 1:nrow(.), .before = "text")
  
  # 删除yaml区域行 ====
  text <- tbl_text$text
  tar_star <- "^---$"
  tar_end <- "^---$"
  tbl_detect <- detect_seq(txt = text, ptn_prefix = tar_star, ptn_suffix = tar_end) %>%
    filter(range_id ==1)
  
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_detect, by = "row") %>%
    mutate(grp_id = ifelse(is.na(range_id), 0, range_id)) %>%
    mutate(symbol_hand = ifelse(grp_id >0, TRUE, FALSE)) %>%
    group_by(grp_id) %>%
    mutate(
      index_grouped = ifelse(symbol_hand, data.table::rowid(grp_id), 0),
      index_max = max(index_grouped)
    ) %>%
    ungroup() %>%
    filter(!(symbol_hand &index_grouped <index_max)) %>%
    select(row, text) %>%
    mutate(row = 1:nrow(.))
  cat(
    glue("step success: xariang yaml lines are clean!"),
    collapse = "\n"
  )
  
  # 变更文件路径====
  tbl_text <- tbl_text %>%
    mutate(
      text = str_replace(text, "\\.\\./", "")
    )
  cat(
    glue("step success: file path relationship changed and ignore path prefix '../'!"),
    collapse = "\n"
  )
  
  # ==== 处理数学公式====
  
  ## ====处理环境公式====
  tbl_text <- tbl_text %>%
    mutate(text = str_replace(text, "(^\\$\\$)", "\\1\\\n")) %>%
    mutate(text = str_replace(text, "(\\$\\$$)", "\\\n\\1"))
  
  ## ====处理行内公式====
  tbl_text <- tbl_text %>%
    mutate(
      symbol_pos = str_detect(text, "^\\$[^\\$]"),
      #index_raw = row_number(row),
      index_detect = ifelse(
        symbol_pos==TRUE,
        row,NA)
    ) %>%
    mutate(index_detect_lag = lead(index_detect, 1)) %>%
    # 确认所有应该处理的行，并进行TRUE/FALSE指示
    mutate(
      symbol_hand = ifelse(
        !(is.na(index_detect)&is.na(index_detect_lag)),
        TRUE, FALSE)
    ) %>%
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rwordld网站关于长程群组序号标定的内容。
    mutate(
      index_grouped = as.integer(factor((FALSE^!symbol_hand) *rleid(symbol_hand)))) %>%
    group_by(index_grouped) %>%
    # 合并目标群组的文本内容
    mutate(
      text_new = ifelse(index_grouped >1, str_c(text, collapse = ""), text),
      index_keep = ifelse(index_grouped >1, min(row), row)
    ) %>%
    ungroup() %>%
    select(index_keep, text_new) %>%
    unique() %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  cat(
    glue("step success: Math enviroment symbols handled!"),
    collapse = "\n"
  )
  
  # 演讲者备注====
  ## 识别`???`和`---`====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- "^\\?\\?\\?$"
  tar_end <- "^---$"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end) %>%
    # 不保留群组内的最后一行（含有`---`标识符）
    group_by(range_id) %>%
    mutate(row_max =max(row)) %>%
    ungroup() %>%
    filter(!(row==row_max))
  
  ## 替换并合并为reveal样式====
  ## `::: {.notes}\n your-text \n :::\n`
  tbl_text <- tbl_text %>%
    left_join(., tbl_detect, by = "row") %>%
    mutate(
      #grp_id = ifelse(is.na(range_id), 0, grp_id),
      symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE) 
    ) %>%
    # 获得长程分组标号
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rwordld网站关于长程群组序号标定的内容。
    mutate(
      rle_id = data.table::rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, rle_id, 0)))
    ) %>%
    # 获得组内次序编号
    group_by(rle_id) %>%
    mutate(
      #index_grouped = data.table::rowid(grp_id),
      index_max = max(row)
    ) %>%
    ungroup() %>%
    # 替换css样式
    group_by(index_grouped) %>%
    mutate(
      text = str_replace(text, "^\\?{3}", "::: {.notes}\\\n"), # !!!
      text = ifelse(
        symbol_hand & row==index_max, 
        paste0(c("",":::", ""), collapse="\n"), # 不能使用str_c !!!!
        text)
    ) %>%
    # 合并目标群组的文本内容
    mutate(
      text_new = ifelse(index_grouped >1, str_c(text, collapse = ""), text),
      index_keep = ifelse(index_grouped >1, min(row), row)
    ) %>%
    ungroup() %>%
    select(index_keep, text_new) %>%
    unique() %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  cat(
    ("step success: speaker notes convert from `???` to `::: {.notes}`!"),
    collapse = "\n"
  )
  
  
  # 处理xarigan layout属性类和自定义div类====
  ## 识别layout属性区域====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- "^---$"
  tar_end <- "^#{1,4}[^|]"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end) %>%
    # 不保留群组内的第一行（含有`---`标识符）
    group_by(range_id) %>%
    mutate(row_min =min(row)) %>%
    ungroup() %>%
    filter(!(row==row_min))
  
  tbl_layout <- tbl_text %>%
    left_join(., tbl_detect, by = "row") %>%
    mutate(
      #grp_id = ifelse(is.na(range_id), 0, grp_id),
      symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE) 
    ) %>%
    # 获得长程分组标号
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rwordld网站关于长程群组序号标定的内容。
    mutate(
      rle_id = data.table::rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, rle_id, 0)))
    ) %>%
    # 获得组内次序编号
    group_by(rle_id) %>%
    mutate(
      #index_grouped = data.table::rowid(grp_id),
      index_max = max(row)
    ) %>%
    # 目标区域进行行排序，逆序
    mutate(order_grouped = cumsum(symbol_hand)) %>%
    ungroup() %>%
    arrange(rle_id, desc(order_grouped), row)
  
  
  ## 替换layout属性区域====  
  class_reveal <- ".monash-bg-blue .mcenter"
  tbl_text <- tbl_layout %>%
    # layout:，直接清除
    mutate(
      text_new = ifelse(
        symbol_hand & str_detect(text, "^layout:"),
        "", NA)
    ) %>%
    # class:，使用reveal样式class_reveal
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^class"),
      class_reveal, text_new)
    ) %>%
    # name:，使用reveal样式
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^name:"),
      paste0("#",trimws(str_extract(text, "(?<=name:)(.+)"))), 
      text_new)
    ) %>%
    # background-image:，使用reveal样式
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^background-image:"),
      paste0("background-image=",trimws(str_extract(text, '(?<=\\()(.+)(?=\\))'))), 
      text_new)
    ) %>%
    # count:，使用reveal样式
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^count:?.false$") ,
      paste0("visibility=",'"', "uncounted", '"'), 
      text_new)
    ) %>%
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^count:?.true$") ,
      "", 
      text_new)
    ) %>%
    mutate(text_new = ifelse(symbol_hand&!is.na(text_new), text_new, "")) %>%
    # 合并上述属性，整合为全新的reveal css样式
    group_by(rle_id) %>%
    mutate(order_max = ifelse(symbol_hand, max(order_grouped), NA)) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & order_grouped < order_max,
        paste0(text_new, collapse = " "),
        text
      )
    )%>%
    ungroup() %>%
    # 添加reveal 属性区域符号"{}"
    mutate(
      text_new = ifelse(
        symbol_hand & (order_grouped < order_max),
        paste0("{",trimws(text_new), "}"),
        text_new
      )
    ) %>%
    # 唯一化重复属性行
    group_by(rle_id) %>%
    mutate(order_grouped = ifelse(symbol_hand,1, row_number(row))) %>%
    ungroup() %>%
    select(symbol_hand, rle_id, order_grouped, text_new) %>%
    unique()  %>%
    # 完全整合标题内容与属性
    group_by(rle_id) %>%
    mutate(
      text = ifelse(symbol_hand, paste0(text_new, collapse = ""), text_new)
    ) %>%
    ungroup() %>%
    select(-text_new) %>%
    unique() %>%
    mutate(row = 1:nrow(.)) %>%
    select(row, text) %>%
    mutate(
      text = ifelse(
        str_detect(text, "\\{\\}$"),
        str_replace(text, "(\\{\\}$)", ""),
        text
      )
    )
  cat(
    ("step success: convert heading slide's layout area parameters
       (`layout:`;`class:`;`name:`;`count:`;`background-image:`) 
       to unify style
       ` # heading {#name background-image='path-to-pic.png' .monash-bg-blue .mcenter}`!"),
    collapse = "\n"
  )
  
  
  # 注释文本块footnote====
  ## xaringan的注释文本块`[.footnote your-text ]`
  ## 变更为reveal的注释文本块样式`::: {.aside} your-text :::`
  
  ## 识别目标区域.footnote====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- "^\\.footnote\\[$"
  tar_end <- "^\\]$"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end)
  
  stop( length(tbl_detect) <1, "No `.footnote[]` detected!") 
  
  ## 替换.footnote属性====
  ## reveal的注释文本块样式`::: {.aside} your-text :::`  
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_detect, by="row") %>%
    #mutate(grp_id = ifelse(is.na(range_id), 0, rle_id)) %>%
    mutate(symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE)) %>%
    # 长程群组序号标定
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rworld网站关于长程群组序号标定的内容。
    mutate(
      grp_id = rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, grp_id, 0)))) %>%
    # .footnote:，替换为reveal样式
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^\\.footnote\\["),
      paste0(c("::: {.aside}", ""),collapse="\n"), NA
    )
    ) %>%
    mutate(text_new = ifelse(
      symbol_hand & str_detect(text, "^\\]"),
      paste0(c("",":::"), collapse="\n"), text_new
    )
    ) %>%
    mutate(text_new = ifelse(is.na(text_new), text, text_new) ) %>%
    # 完全整合标题内容与属性
    group_by(grp_id) %>%
    mutate(
      text = ifelse(symbol_hand, paste0(text_new, collapse = ""), text_new),
      index_grouped = ifelse(symbol_hand, max(row), row)
    ) %>%
    ungroup() %>%
    select(index_grouped, text) %>%
    unique() %>%
    filter(!is.na(text)) %>%
    mutate(row = 1:nrow(.))  %>%
    select(row, text) 
  cat(
    ("step success: footnote convert from `.footnote[]` to `:::{.aside}:::`!"),
    collapse = "\n"
  )
  
  # 递增点击动画符`--`====
  ## 可能存在一个递增点击和多个递增点击
  
  ## 循环识别：`--`和`---`之间，且不再包含`--`====
  txt <- tbl_text$text
  len_txt <- length(txt)
  
  ptn_prefix <- "^--$"
  ptn_suffix <- "^---$"
  set_start <- NULL
  set_end <- NULL
  index <- 0
  # 循环识别目标区域
  # i <- 885
  # j <- 887
  for (i in 1:len_txt) {
    start_pos <- str_detect(txt[i], ptn_prefix)
    if (start_pos) {
      index <- index +1
      for (j in (i +1):len_txt){
        rep_pos <- str_detect(txt[j], ptn_prefix)
        end_pos <- str_detect(txt[j], ptn_suffix)
        if (rep_pos |end_pos) {
          set_start[index] <- i
          set_end[index] <- j
          break
        }
      }
    }
  }
  
  ## 获得目标分组以及行序号====
  k <- 1
  index_range <- NULL
  for (k in 1:length(set_start)){
    index_range[[k]] <- seq(set_start[k],set_end[k])
  }
  
  tbl_index <- tibble(
    range_id = 1:length(set_start), 
    row = index_range
  ) %>%
    unnest(row) %>%
    group_by(range_id) %>%
    mutate(
      row_min =min(row),
      row_max = max(row)
    ) %>%
    ungroup() %>%
    filter(!(row==row_min | row ==row_max)) %>%
    select(range_id, row)
  
  ## 替换递增点击动画符样式====
  
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_index, by="row") %>%
    #mutate(grp_id = ifelse(is.na(range_id), 0, rle_id)) %>%
    mutate(symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE)) %>%
    # 长程群组序号标定
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rworld网站关于长程群组序号标定的内容。
    mutate(
      grp_id = rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, grp_id, 0)))
    ) %>%
    # .footnote:，替换为reveal样式
    group_by(grp_id) %>%
    mutate(text_new = ifelse(
      symbol_hand,
      paste0(
        c("::: {.incremental}", 
          paste0(text),
          ":::",
          ""
        ),
        collapse="\n"), 
      text)
    ) %>%
    mutate(order_grouped = ifelse(symbol_hand, max(row), row)) %>%
    ungroup() %>%
    select(order_grouped, text_new) %>%
    unique() %>%
    filter(!is.na(text_new)) %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.)) 
  cat(
    ("step success: incremental list convert from `--` to `:::{.incremental}:::`!"),
    collapse = "\n"
  )
  
  # css布局样式====
  
  ## 识别目标区域.pull-left====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- "^\\.pull-left\\[$"
  tar_end <- "^\\]$"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end)
  
  ## 替换.pull-left属性====
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_detect, by="row") %>%
    #mutate(grp_id = ifelse(is.na(range_id), 0, rle_id)) %>%
    mutate(symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE)) %>%
    # 长程群组序号标定
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rworld网站关于长程群组序号标定的内容。
    mutate(
      grp_id = rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, grp_id, 0)))
    ) %>%
    group_by(grp_id) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & row==min(row),
        paste0(
          c(":::: {.columns}", "", '::: {.column width="40%"}', ""), 
          collapse = "\n"
        ),
        text
      )
    ) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & row==max(row),
        paste0(c(":::", ""),collapse = "\n"),
        text_new
      )
    ) %>%
    ungroup() %>%
    select(row, text_new) %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  
  ## 识别目标区域.pull-right====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- "^\\.pull-right\\[$"
  tar_end <- "^\\]$"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end)
  
  ## 替换.pull-right属性====
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_detect, by="row") %>%
    #mutate(grp_id = ifelse(is.na(range_id), 0, rle_id)) %>%
    mutate(symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE)) %>%
    # 长程群组序号标定
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rworld网站关于长程群组序号标定的内容。
    mutate(
      grp_id = rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, grp_id, 0)))
    ) %>%
    group_by(grp_id) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & row==min(row),
        paste0(
          c('::: {.column width="40%"}', ""), 
          collapse = "\n"
        ),
        text
      )
    ) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & row==max(row),
        paste0(c(":::", "", "::::"),collapse = "\n"),
        text_new
      )
    ) %>%
    ungroup() %>%
    select(row, text_new) %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  cat(
    ("step success: columns layout convert from \n`.pull-left/right[]` to `:::{column width='40%'}:::`!"),
    collapse = "\n"
  )
  
  # 注释化自定义css样式====
  
  ### 识别目标区域====
  ## 使用fun-utilities.R下的自定义函数`detect_seq()`
  text_input <- tbl_text$text
  tar_star <- paste0(
    c(
      "^\\.tbl-fontsize\\[",  # 表格字体样式
      "^\\.scroll-output\\[", # 输出结果下拉样式
      "^\\.scroll-box-\\d{1,2}\\[", # 滚动下拉样式
      "^\\.large\\["  # 字号样式
    ),
    collapse = "|"
  )
  tar_end <- "^\\]$"
  tbl_detect <- detect_seq(txt = text_input, ptn_prefix = tar_star, ptn_suffix = tar_end)
  
  ### 替换为注释属性样式====
  tbl_text <- tbl_text %>%
    mutate(row = 1:nrow(.)) %>%
    left_join(., tbl_detect, by="row") %>%
    #mutate(grp_id = ifelse(is.na(range_id), 0, rle_id)) %>%
    mutate(symbol_hand = ifelse(!is.na(range_id), TRUE, FALSE)) %>%
    # 长程群组序号标定
    # 依次对TRUE指示按顺次分组编号
    # library(data.table), 参看：https://stackoverflow.com/questions/61936592/r-tidyverse-create-groups-based-on-index-column
    # 或者参看rworld网站关于长程群组序号标定的内容。
    mutate(
      grp_id = rleid(symbol_hand),
      index_grouped = as.integer(as.factor(ifelse(symbol_hand, grp_id, 0)))
    ) %>%
    group_by(grp_id) %>%
    mutate(
      row_max = max(row),
      row_min = min(row)
    ) %>%
    ungroup() %>%
    # 注释化处理
    mutate(
      #text_new = text,
      text_new = ifelse(
        symbol_hand & (row==row_min),
        str_replace(text, "(^\\..+)", "<!---\\1--->"),
        text
      )
    ) %>%
    mutate(
      text_new = ifelse(
        symbol_hand & (row==row_max),
        str_replace(text, "^(\\])", "<!---\\1--->"),
        text_new
      )
    ) %>%
    #ungroup() %>%
    select(row, text_new) %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  cat(
    ("step success: comment out some custom css style including \n (`.tbl-fontsize[`;`.scroll-output[`;`.scroll-box-16[`;`.large[]`)!"),
    collapse = "\n"
  )
  
  # 删除特定行====
  tbl_text <- tbl_text %>%
    ## 删除页码分隔符`---`和递增点击动画符`--`
    mutate(
      text_new = ifelse(
        str_detect(text, "^---$|^--$"),
        "",
        text
      )
    ) %>%
    # 删除
    mutate(
      text_new = ifelse(
        str_detect(text, "^<div>|^<a>|^\\&emsp"),
        "",
        text_new
      )
    ) %>%
    select(row, text_new) %>%
    rename_all(., ~c("row", "text")) %>%
    mutate(row = 1:nrow(.))
  cat(
    ("step success: clean lines with `---` and `--`!"),
    collapse = "\n"
  )
  
  # 行内css样式====
  
  ## 行内文本颜色`.red[]`
  tbl_text <- tbl_text %>%
    #.[50:100,] %>%
    # 识别并添加{.red}属性
    mutate(
      text_new = ifelse(
        str_detect(text, "\\.red\\["),
        str_replace_all(
          text,
          "(?<=\\.red)(\\[.+?\\])",
          "\\1\\{\\.red\\}"
        ),
        text
      )
    ) %>%
    # 删除前置的`.red`[your-text]{.red}
    mutate(
      text_new = ifelse(
        str_detect(text_new, "\\.red\\["),
        str_replace_all(text_new, "\\.red(?=\\[)",""),
        text_new
      )
    ) %>%
    select(row, text_new) %>%
    rename_all(., ~c("row", "text")) 
  cat(
    ("step success: convert inline css  `.red[text]` to `[text]{.red}`!"),
    collapse = "\n"
  )
  
  #====写出文件====
  # dir_tar <- fs::path_dir(file_qmd)
  # file_tar <- "xaringan-convert.qmd"
  # (file_out <- paste0(dir_tar, "/",file_tar))
  
  # writeLines("\n空行", file_out)
  # writeLines(tbl_clean$text[189], file_out)
  
  writeLines(unlist(tbl_text$text), qmd)
  # 删除临时文件
  fs::file_delete(qmd_tem)
  cat(
    glue("step success: write out file at \n {qmd} \n and delete tem file{qmd_tem}!"),
    collapse = "\n"
  )
  
}
