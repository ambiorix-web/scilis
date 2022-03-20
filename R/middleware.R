#' Scilis
#' 
#' Signs (and unsigns) cookies to ensure they are not
#' altered by clients.
#' 
#' @param secret Secret for has creation. 
#' Ideally use an environment variable so as to keep
#' this secret.
#' 
#' @details This signs the values of cookies with
#' a base 64 encoded sha256 hash. Note that the 
#' value of the cookie is still visible.
#' When cookies are being read the value is compared
#' to the expected hash to ensure it was not altered
#' by the client.
#' If the cookie appears aletered the returned value
#' is `character(0L)`.
#' 
#' @examples
#' app <- ambiorix::Ambiorix$
#'  new()$
#'  use(scilis("secret"))
#' 
#' @export 
scilis <- function(
  secret
) {
  if(missing(secret))
    stop("Missing `secret`")

  fns <- list(
    make_parser(secret),
    make_preprocessor(secret)
  )
  invisible(fns)
}

#' @importFrom ambiorix as_cookie_parser
make_parser <- function(
  secret
) {
  fn <- \(req) {
    ambiorix::default_cookie_parser(req) |> 
      lapply(\(cookie) {
        unsign(cookie, secret)
      })
  }

  ambiorix::as_cookie_parser(fn)
}

#' @importFrom ambiorix as_cookie_preprocessor
make_preprocessor <- function(
  secret
) {
  fn <- \(name, value, ...) {
    sign(value, secret)
  }

  ambiorix::as_cookie_preprocessor(fn)
}

#' @importFrom digest hmac
#' @importFrom base64enc base64encode
sign <- function(value, secret) {
  hash <- secret |> 
    digest::hmac(
      as.character(value), 
      algo = "sha256"
    ) |> 
    charToRaw() |> 
    base64enc::base64encode()

  hash <- gsub("\\=+$", "", hash)
  paste0(value, ".", hash)
}

#' @importFrom base64enc base64decode
unsign <- function(value, secret) {
  tentative_value <- strsplit(as.character(value), split = "\\.")[[1]][1]
  expected_input <- sign(tentative_value, secret)
  expected_raw <- charToRaw(expected_input)
  input_raw <- charToRaw(value)

  if(length(expected_raw) != length(input_raw))
    return(character(0L))

  if(!all(input_raw == expected_raw))
    return(character(0L))

  return(tentative_value)
}
