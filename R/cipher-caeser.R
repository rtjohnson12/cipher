
#' @examples
#' encrypt_cipher_caeser("This is my secret message.", key = 13)
#' #> "guv6Iv6Iz!I6rp5r7Izr66ntrL"
encrypt_cipher_caeser <-
  function(
    plaintext, key
    , dictionary = c("A", "D", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
                     "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X",
                     "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                     "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                     "y", "z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", " ",
                     "!", "?", ".")
  ) {

    ciphertext <- plaintext %>%
      base::strsplit(split = "") %>% `[[`(1) %>%
      sapply(function(x) {
        if (x %in% dictionary) {
          idx = (match(x, dictionary) + key) %% length(dictionary)
          dictionary[ifelse(idx == 0, length(dictionary), idx)]
        } else x
      }) %>%
      paste(collapse = "")

    ciphertext

  }

decrypt_cipher_caeser <-
  function(
    ciphertext, key
    , dictionary = c("A", "D", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
                     "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X",
                     "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                     "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                     "y", "z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", " ",
                     "!", "?", ".")
  ) {

    plaintext <- ciphertext %>%
      base::strsplit(split = "") %>% `[[`(1) %>%
      sapply(function(x) {
        if (x %in% dictionary) {
          idx = (match(x, dictionary) - key) %% length(dictionary)
          dictionary[ifelse(idx == 0, length(dictionary), idx)]
        } else x
      }) %>%
      paste(collapse = "")

    plaintext

  }
