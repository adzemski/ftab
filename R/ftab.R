

df_to_table <- function(df, col_labels = colnames(df), ...) {
  tb <- make_empty_table(...)

  if (anyDuplicated(colnames(df))) stop("Non-unique column names.")

  if (length(col_labels) != ncol(df)) {
    col_labels <- colnames(df)
    warning("Provided col_labels are of incorrect length. Default to column names.")
  }

  col_labels[!is.character(col_labels)] <-
    paste0('column', seq_len(ncol(df)))[!is.character(col_labels)]

  for (c in seq_along(colnames(df))) {
    tb <- add_column(tb, name = colnames(df)[[c]], data = df[[c]], label = col_labels[[c]])
  }

  tb
}

make_empty_table <- function(default_digits = 2, default_sanitize_text = TRUE,
                             default_NA_symbol = "NA", default_NAN_symbol = "NAN") {
  tb <- list(cols = list(), braces = list(), hspace = list(),
             default_digits = default_digits, default_sanitize_text = default_sanitize_text,
             default_NA_symbol = default_NA_symbol, default_NAN_symbol = default_NAN_symbol)
  class(tb) <- c("ftab", class(tb))
  tb
}

add_column <- function(tb, name, data, label = name,
                       format = if(is.character(data)) "%s" else sprintf("%%.%df", digits),
                       digits = tb$default_digits,
                       align = "r", sanitize_text = tb$default_sanitize_text,
                       NA_symbol = tb$default_NA_symbol, NAN_symbol = tb$default_NAN_symbol) {

  if (anyDuplicated(c(names(tb$cols), name)))
    stop('Adding column would make column name non-unique')

  if (is.character(data)) if (sanitize_text) {data <- xtable::sanitize(data)}

  tb$cols <-
    setNames(c(tb$cols, list(list(label = label, data = data, format = format, align = align,
                                  NA_symbol = NA_symbol, NAN_symbol = NAN_symbol))),
             c(names(tb$cols), name))

  tb
}

add_brace <- function(tb, start_col, end_col, text = "") {
  tb$braces <- c(tb$braces, list(list(start_col = start_col, end_col = end_col, text = text)))

  tb
}

add_space <- function(tb, after_col, space = 0, text = sprintf("@{\\hspace{%fem}}", space)) {
  tb$hspace <- c(tb$hspace, list(list(after_col = after_col, text = text)))

  tb
}

set_column_format <- function(tb, column_name, new_format) {

  if (!is.character(column_name)) stop("Column names must be characters")

  column_name <- column_name[column_name %in% names(tb$cols)]

  if (length(new_format) != length(column_name))
    new_format <- rep(new_format[[1]], times = length(column_name))

  for (i in seq_along(column_name))
    tb$cols[[column_name[[i]]]]$format <- new_format[[i]]

  tb
}

set_column_label <- function(tb, column_name, new_label) {

  if (!is.character(column_name)) stop("Column names must be characters")

  column_name <- column_name[column_name %in% names(tb$cols)]

  if (length(new_label) != length(column_name))
    new_label <- rep(new_label[[1]], times = length(column_name))

  for (i in seq_along(column_name))
    tb$cols[[column_name[[i]]]]$label <- new_label[[i]]

  tb
}

set_column_name <- function(tb, old_name, new_name) {

  if (!is.character(old_name)) stop("Column names must be characters")

  old_name <- old_name[old_name %in% names(tb$cols)]
  new_name <- new_name[old_name %in% names(tb$cols)]

  if (length(old_name) == 0) return(NULL)

  new_names <- names(tb$cols)

  new_names[match(old_name, new_names)] <- new_name
  names(new_names) <- names(tb$cols)

  names(tb$cols) <- new_names

  # change names in braces
  change_col_name_braces <- function(brace) {
    brace$start_col = new_names[brace$start_col]
    brace$end_col = new_names[brace$end_col]
    brace
  }

  tb$braces <- lapply(tb$braces, change_col_name_braces)

  tb
}

get_ready <- function(tb, order_col = names(tb$cols),
                      vadj = rep.int("\\\\ \n", times = tb_nrow)) {

  tb_nrow <- max(sapply(tb$cols, FUN = function(x) length(x$data)))

  # force evaluation or order_col
  order_col <- order_col

  # add artifical columns for horizontal space
  if (length(tb$hspace) > 0) {
    for (i in tb$hspace) {
      tb <-
        add_column(tb,  paste0('_after_', i$after_col), label = '',
                   data = rep.int('', times = tb_nrow))
      hrow_col <- which(order_col == i$after_col)
      order_col <- c(order_col[seq_len(hrow_col)], paste0('_after_', i$after_col),
                    order_col[hrow_col + seq_len(length(order_col) - hrow_col)])
    }
  }

  format_col <- function(col) {
    NA_rows <- is.na(col$data)
    NAN_rows <- is.nan(col$data)

    col$data <- sprintf(col$format, col$data)

    col$data[NA_rows] <- col$NA_symbol
    col$data[NAN_rows] <- col$NAN_symbol

    col$data <- c(col$data, rep.int("", times = tb_nrow - length(col$data)))
    col
  }

  tb$cols <- lapply(tb$cols[order_col], format_col)

  tb_colnames <- paste(sapply(tb$cols, function(x) x$label), collapse = " & ")

  rows <- vector(mode = 'character', length = tb_nrow)

  for (i in seq_len(tb_nrow)) {
    rows[[i]] <- paste(sapply(tb$cols, function(x) x$data[[i]]), collapse = " & ")
  }

  body <- paste(paste0(rows, vadj), collapse = '')

  tb_ncol <- length(tb$cols)

  halign <- lapply(tb$cols, function(x) x$align)
  names(halign) <- names(tb$cols)

  if (length(tb$hspace) > 0) {
    for (i in tb$hspace) {
      halign[[paste0('_after_', i$after_col)]] <-
        paste0(halign[[paste0('_after_', i$after_col)]], i$text, sep = ' ')
    }
  }

  if (length(tb$braces) > 0) {

    translate.brace <- function(b) {
      b$start_col = which(names(tb$cols) == b$start_col)
      b$end_col = which(names(tb$cols) == b$end_col)
      b$length <- b$end_col - b$start_col + 1
      if (b$length < 1) stop('brace does not have positive length')
      b
    }

    # translate to column numbers
    braces <- lapply(tb$braces, translate.brace)
    # sort
    braces <- braces[order(sapply(braces, function(x) x$start_col))]

    b <- braces[[1]]
    rowbraces1 <-
      paste(c(rep.int("", b$start_col - 1), sprintf("\\multicolumn{%d}{c}{%s}", b$length, b$text)),
            collapse = ' & ')
    rowbraces2 <- sprintf('\\cmidrule{%d-%d}', b$start_col, b$end_col)
    endlast <- b$end_col

    for (b in braces[1 + seq_len(length(braces) - 1)]) {
      if (b$start_col - 1 - endlast < 0) stop('overlapping braces')
      rowbraces1 <- paste(c(rowbraces1, rep.int("", b$start_col - 1 - endlast),
                            sprintf("\\multicolumn{%d}{c}{%s}", b$length, b$text)), collapse = ' & ')
      endlast <- b$end_col
      rowbraces2 <- paste0(rowbraces2, sprintf('\\cmidrule{%d-%d}', b$start_col, b$end_col))
    }

    rowbraces1 <-
      paste0(rowbraces1, paste(rep.int(' & ', tb_ncol - endlast), collapse = ""), '\\\\ \n')

  } else
    rowbraces1 <- rowbraces2 <- ''

  list(body = body, header = paste0(rowbraces1, rowbraces2, tb_colnames),
       halign = paste0(c('@{}', halign), collapse = ''))
}

print.ftab <- function(tb, filename = NULL, environment = 'tabular', env_arg1 = NULL, ...) {

  ready <- get_ready(tb, ...)

  if (is.character(env_arg1))
    env_arg1 <- sprintf('{%s}', env_arg1)
  else
    env_arg1 <- ''

  table <- paste0(c(sprintf("\\begin{%s}%s{%s}", environment, env_arg1, ready$halign),
                    "\\toprule",
                    ready$header,
                    "\\\\ \\midrule",
                    ready$body,
                    "\\bottomrule",
                    sprintf("\\end{%s}", environment)), collapse = '\n')

  if (!is.null(filename)) {
    write(table, file = filename)
    message("Saved table to file.")
  }

  table
}
