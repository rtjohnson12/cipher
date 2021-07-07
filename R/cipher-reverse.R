
encrypt_cipher_reverse <- function(plaintext) {

  ciphertext <- plaintext %>%
    base::strsplit(split = "") %>%
    `[[`(1) %>% base::rev() %>%
    paste(collapse = "")

  ciphertext

}

decrypt_cipher_reverse <- function(ciphertext) {

  plaintext <- ciphertext %>%
    base::strsplit(split = "") %>%
    `[[`(1) %>% base::rev() %>%
    paste(collapse = "")

  plaintext

}
