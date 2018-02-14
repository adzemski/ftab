

df_to_table <- function(df, ...) {
  tb <- make_empty_table(...)

  if (any(is.na(colnames(df)))) colnames(df) <- paste0('column', seq_len(ncol(df)))

  if (anyDuplicated(colnames(df)))
    labels <- paste0('column', seq_len(ncol(df)))
  else
    labels <- colnames(df)

  for (c in seq_along(colnames(df))) {
    tb <- add_column(tb, name = colnames(df)[[c]], data = df[[c]], label = labels[[c]])
  }

  tb
}

make_empty_table <- function(default_digits = 2) {
  tb <- list(cols = list(), braces = list(), hspace = list(), default_digits = default_digits)
  class(tb) <- c("ftab", class(tb))
  tb
}

add_column <- function(tb, name, data, label = name, format = if(is.character(data)) "%s" else sprintf("%%.%df", digits), digits = tb$default_digits,
                        align = "r", sanitize.text = TRUE) {
  if (anyDuplicated(c(names(tb$cols), name))) stop('Adding column would make column name non-unique')

  if (is.character(data)) if (sanitize.text) {data <- xtable::sanitize(data)}

  tb$cols <- setNames(c(tb$cols, list(list(label = label, data = data, format = format, align = align))), c(names(tb$cols), name))

  tb
}

add_brace <- function(tb, startCol, endCol, text = "") {
  tb$braces <- c(tb$braces, list(list(startCol = startCol, endCol = endCol, text = text)))

  tb
}

add_space <- function(tb, afterCol, space = 0, text = sprintf("@{\\hspace{%fem}}", space)) {
  tb$hspace <- c(tb$hspace, list(list(afterCol = afterCol, text = text)))

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
    brace$startCol = new_names[brace$startCol]
    brace$endCol = new_names[brace$endCol]
    brace
  }

  tb$braces <- lapply(tb$braces, change_col_name_braces)

  tb
}

get_ready <- function(tb, orderCol = names(tb$cols), vadj = rep.int("\\\\ \n", times = tb.nrow)) {

  tb.nrow <- max(sapply(tb$cols, FUN = function(x) length(x$data)))

  # force evaluation or orderCol
  orderCol <- orderCol

  # add artifical columns for horizontal space
  if (length(tb$hspace) > 0) {
    for (i in tb$hspace) {
      tb <- add_column(tb,  paste0('_after_', i$afterCol), label = '', data = rep.int('', times = tb.nrow))
      hrow.col <- which(orderCol == i$afterCol)
      orderCol <- c(orderCol[seq_len(hrow.col)], paste0('_after_', i$afterCol), orderCol[hrow.col + seq_len(length(orderCol) - hrow.col)])
    }
  }

  formatCol <- function(col) {
    col$data <- c(sprintf(col$format, col$data), rep.int("", times = tb.nrow - length(col$data)))
    col
  }

  tb$cols <- lapply(tb$cols[orderCol], formatCol)

  tb.colnames <- paste(sapply(tb$cols, function(x) x$label), collapse = " & ")

  rows <- vector(mode = 'character', length = tb.nrow)

  for (i in seq_len(tb.nrow)) {
    rows[[i]] <- paste(sapply(tb$cols, function(x) x$data[[i]]), collapse = " & ")
  }

  body <- paste(paste0(rows, vadj), collapse = '')

  tb.ncol <- length(tb$cols)

  halign <- lapply(tb$cols, function(x) x$align)
  names(halign) <- names(tb$cols)

  if (length(tb$hspace) > 0) {
    for (i in tb$hspace) {
      halign[[paste0('_after_', i$afterCol)]] <- paste0(halign[[paste0('_after_', i$afterCol)]], i$text, sep = ' ')
    }
  }

  if (length(tb$braces) > 0) {

    translate.brace <- function(b) {
      b$startCol = which(names(tb$cols) == b$startCol)
      b$endCol = which(names(tb$cols) == b$endCol)
      b$length <- b$endCol - b$startCol + 1
      if (b$length < 1) stop('brace does not have positive length')
      b
    }

    # translate to column numbers
    braces <- lapply(tb$braces, translate.brace)
    # sort
    braces <- braces[order(sapply(braces, function(x) x$startCol))]

    b <- braces[[1]]
    rowbraces1 <- paste(c(rep.int("", b$startCol - 1), sprintf("\\multicolumn{%d}{c}{%s}", b$length, b$text)), collapse = ' & ')
    rowbraces2 <- sprintf('\\cmidrule{%d-%d}', b$startCol, b$endCol)
    endlast <- b$endCol

    for (b in braces[1 + seq_len(length(braces) - 1)]) {
      if (b$startCol - 1 - endlast < 0) stop('overlapping braces')
      rowbraces1 <- paste(c(rowbraces1, rep.int("", b$startCol - 1 - endlast), sprintf("\\multicolumn{%d}{c}{%s}", b$length, b$text)), collapse = ' & ')
      endlast <- b$endCol
      rowbraces2 <- paste0(rowbraces2, sprintf('\\cmidrule{%d-%d}', b$startCol, b$endCol))
    }

    rowbraces1 <- paste0(rowbraces1, paste(rep.int(' & ', tb.ncol - endlast), collapse = ""), '\\\\ \n')

  } else
    rowbraces1 <- rowbraces2 <- ''

  list(body = body, header = paste0(rowbraces1, rowbraces2, tb.colnames), halign = paste0(c('@{}', halign), collapse = ''))
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
