# Caesar cipher for encrypting and decrypting text

#' Encrypt and Decrypt text using the Caesar cipher
#'
#' @param text
#' String to be ciphered or deciphered
#' @param direction
#' Move the original characters the right or to the left
#' @param distance
#' How far to move the characters in the direction you choose.
#' @param reverse
#' If TRUE, deciphers the coded text
#'
#' @return
#' String of the ciphered/deciphered text
#' @export
#'
#' @examples
#' # Please see this for more info.
#' # https://en.wikipedia.org/wiki/Caesar_cipher
#'
#' caesar("HELLO WORLD")
#' caesar("khoor zruog", reverse = TRUE)
#'
#' caesar("HELLO WORLD", distance = 12)
#' caesar("tqxxa iadxp", distance = 12, reverse = TRUE)
#'
caesar <- function(text, distance = 3, reverse = FALSE) {
  if (!is.character(text)) {
    stop("text must be a string!")
  }

  if (!is.numeric(distance)) {
    stop("distance must be a number!")
  }

  if (!distance %in% -24:24) {
    stop("distance must be between -24 and 24")
  }

  direction <- 'left'

  alphabet <- data.frame(original = letters,
                         stringsAsFactors = FALSE)

  alphabet$cipher <- binhf::shift(alphabet$original,
                                  places = distance,
                                  dir = direction)

  if (!reverse) {
    text <- tolower(text)

    for (i in 1:nchar(text)) {

      if (substr(text, i, i)!=".") {

        index_num <- grep(substr(text, i, i), alphabet$original)

        if (length(index_num)>0)
          substr(text, i, i) <- alphabet$cipher[index_num]
      }

    }
  } else {

    for (i in 1:nchar(text)) {

      if (substr(text, i, i)!=".") {
        index_num <- grep(substr(text, i, i), alphabet$cipher)

        if (length(index_num)>0)
          substr(text, i, i) <- alphabet$original[index_num]
      }

    }
  }

  cat(text,"\n\r")

  return(text)
}

