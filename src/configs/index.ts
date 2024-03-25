export const STORAGE_USER = 'NEXT_USER'
export const STORAGE_CUSTOMER = 'NEXT_CUSTOMER'
export const STORAGE_AUTH_TOKEN = 'NEXT_AUTH_TOKEN'
export const STORAGE_TIMEZONE = 'NEXT_TIMEZONE'
export const STORAGE_LOCALE = 'NEXT_LOCALE'
export const STORAGE_THEME = 'NEXT_THEME'
export const STORAGE_PINNED_MENU = 'NEXT_PINNED_MENU'
export const STORAGE_COOKIE_CONSENT = 'NEXT_LOCAL_CONSENT'

export const EMAIL_REGEX =
  /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/

export const PASSWORD_STRENGTH_REGEX =
  /^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[!@#\$%\^&\*])(?=.{8,})/
