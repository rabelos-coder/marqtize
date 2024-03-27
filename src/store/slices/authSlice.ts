import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import Cookies from 'js-cookie'
import { jwtDecode } from 'jwt-decode'

import {
  STORAGE_AUTH_TOKEN,
  STORAGE_LOCALE,
  STORAGE_TIMEZONE,
  STORAGE_USER,
} from '@/configs'
import { APP_LANGUAGE, APP_TIMEZONE } from '@/environment'
import { Auth, AuthState } from '@/types/auth'
import { JWT } from '@/types/jwt'
import { User } from '@/types/user'

let isLoggedIn = false

let language: any = Cookies.get(STORAGE_LOCALE) ?? null
let timezone: any = Cookies.get(STORAGE_TIMEZONE) ?? null
let user: any = Cookies.get(STORAGE_USER) ?? null
let token: any = Cookies.get(STORAGE_AUTH_TOKEN) ?? null

if (!language) {
  language = APP_LANGUAGE
  Cookies.set(STORAGE_LOCALE, language)
}

if (!timezone) {
  timezone = APP_TIMEZONE
  Cookies.set(STORAGE_TIMEZONE, timezone)
}

try {
  if (user) {
    user = JSON.parse(user) as User
    Cookies.set(STORAGE_LOCALE, user?.language ?? APP_LANGUAGE)
    Cookies.set(STORAGE_TIMEZONE, user.timezone?.code ?? APP_TIMEZONE)
  }
} catch {
  Cookies.remove(STORAGE_USER)
}

let jwt: any = null

if (token) {
  try {
    const data = jwtDecode(token) as JWT
    if (data?.exp >= Date.now() / 1000) {
      Cookies.set(STORAGE_AUTH_TOKEN, `${token}`)
      jwt = data
      isLoggedIn = true
    } else {
      jwt = null
      token = null
      user = null
      isLoggedIn = false
      Cookies.remove(STORAGE_AUTH_TOKEN)
    }
  } catch {
    jwt = null
    token = null
    user = null
    isLoggedIn = false
    Cookies.remove(STORAGE_AUTH_TOKEN)
  }
} else {
  Cookies.remove(STORAGE_USER)
  Cookies.remove(STORAGE_AUTH_TOKEN)
}

const initialState: AuthState = {
  user,
  token,
  jwt,
  timezone,
  language,
  isLoggedIn,
}

export const authSlice = createSlice({
  name: 'auth',
  initialState,
  reducers: {
    resetAuth: (state) => {
      state.user = null
      state.token = null
      state.jwt = null
      state.language = APP_LANGUAGE
      state.timezone = APP_TIMEZONE

      Cookies.remove(STORAGE_USER)
      Cookies.remove(STORAGE_AUTH_TOKEN)

      Cookies.set(STORAGE_LOCALE, state.language)
      Cookies.set(STORAGE_TIMEZONE, state.timezone)
    },

    setUser: (state, action: PayloadAction<User>) => {
      state.user = action.payload
      Cookies.set(STORAGE_USER, JSON.stringify(action.payload))
      Cookies.set(STORAGE_LOCALE, state.user.language ?? APP_LANGUAGE)
      Cookies.set(STORAGE_TIMEZONE, state.user.timezone.code ?? APP_TIMEZONE)
    },
    setToken: (state, action: PayloadAction<string>) => {
      state.token = action.payload

      Cookies.set(STORAGE_AUTH_TOKEN, `${state.token}`)

      try {
        const jwt = jwtDecode(state.token) as JWT
        state.jwt = jwt
      } catch {
        state.jwt = null
        Cookies.remove(STORAGE_USER)
        Cookies.remove(STORAGE_AUTH_TOKEN)
      }
    },
    setLanguage: (state, action: PayloadAction<string>) => {
      state.language = action.payload
      Cookies.set(STORAGE_LOCALE, state.language ?? APP_LANGUAGE)
    },
    setTimezone: (state, action: PayloadAction<string>) => {
      state.timezone = action.payload
      Cookies.set(STORAGE_TIMEZONE, state.timezone ?? APP_TIMEZONE)
    },
    setAuth(state, action: PayloadAction<Auth>) {
      state.user = action.payload.user
      state.token = action.payload.token
      state.language = action.payload.user.language ?? APP_LANGUAGE
      state.timezone = action.payload.user.timezone.code ?? APP_TIMEZONE
      state.isLoggedIn = true

      Cookies.set(STORAGE_USER, JSON.stringify(state.user))
      Cookies.set(STORAGE_AUTH_TOKEN, state.token)
      Cookies.set(STORAGE_LOCALE, state.user.language ?? APP_LANGUAGE)
      Cookies.set(STORAGE_TIMEZONE, state.user.timezone.code ?? APP_TIMEZONE)

      try {
        const jwt = jwtDecode(state.token) as JWT
        state.jwt = jwt
      } catch {
        state.jwt = null
        Cookies.remove(STORAGE_USER)
        Cookies.remove(STORAGE_AUTH_TOKEN)
      }
    },
  },
})

export const {
  resetAuth,
  setAuth,
  setUser,
  setToken,
  setLanguage,
  setTimezone,
} = authSlice.actions
export default authSlice.reducer
