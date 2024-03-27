export const STORAGE_USER = 'NEXT_USER'
export const STORAGE_ACCOUNT = 'NEXT_ACCOUNT'
export const STORAGE_AUTH_TOKEN = 'NEXT_AUTH_TOKEN'
export const STORAGE_TIMEZONE = 'NEXT_TIMEZONE'
export const STORAGE_LOCALE = 'NEXT_LOCALE'
export const STORAGE_THEME = 'NEXT_THEME'
export const STORAGE_PINNED_MENU = 'NEXT_PINNED_MENU'
export const STORAGE_COOKIE_CONSENT = 'NEXT_LOCAL_CONSENT'

/**
 * Checks if the given value is a valid password.
 * @param {string} value - The value to be checked
 * @return {boolean} Whether the value is a valid password
 */
export const PASSWORD_STRENGTH_REGEX =
  /^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[!@#\$%\^&\*])(?=.{8,})/

/**
 * Checks if the given value is a valid name.
 *
 * @param {string} value - The value to be checked
 * @return {boolean} Whether the value is a valid name
 */
export const NAME_REGEX = /(^[\p{L}\d'\.\s\-]*$)/u

/**
 * Checks if the given value is a valid slug.
 *
 * @param {string} value - The value to be checked
 * @return {boolean} Whether the value is a valid slug
 */
export const SLUG_REGEX = /^[a-z\d]+(?:(\.|-|_)[a-z\d]+)*$/

/**
 * Checks if the given value is a valid bcrypt hash.
 *
 * @param {string} value - The value to be checked
 * @return {boolean} Whether the value is a valid bcrypt hash
 */
export const BCRYPT_HASH = /\$2[abxy]?\$\d{1,2}\$[A-Za-z\d\./]{53}/

/**
 * Checks if the given value is a valid email.
 *
 * @param {string} value - The value to be checked
 * @return {boolean} Whether the value is a valid email
 */
export const EMAIL_REGEX =
  /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
