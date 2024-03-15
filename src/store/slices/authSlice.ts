import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import Cookies from "js-cookie";
import { jwtDecode } from "jwt-decode";
import secureLocalStorage from "react-secure-storage";

import {
  STORAGE_AUTH_TOKEN,
  STORAGE_LANGUAGE,
  STORAGE_LOCALE,
  STORAGE_TIMEZONE,
  STORAGE_USER,
} from "@/configs";
import { APP_LANGUAGE, APP_TIMEZONE } from "@/environment";
import { Auth, AuthState } from "@/types/auth";
import { JWT } from "@/types/jwt";
import { User } from "@/types/user";

let isLoggedIn = false;

let language: any =
  typeof window !== "undefined"
    ? (localStorage.getItem(STORAGE_LANGUAGE) as string) ?? null
    : null;

let timezone: any =
  typeof window !== "undefined"
    ? (localStorage.getItem(STORAGE_TIMEZONE) as string) ?? null
    : null;

let user: any =
  typeof window !== "undefined"
    ? (secureLocalStorage.getItem(STORAGE_USER) as string) ?? null
    : null;

let token: any =
  typeof window !== "undefined"
    ? (secureLocalStorage.getItem(STORAGE_AUTH_TOKEN) as string) ?? null
    : null;

if (!language) {
  language = APP_LANGUAGE;
  if (typeof window !== "undefined")
    localStorage.setItem(STORAGE_LANGUAGE, language);
  Cookies.set(STORAGE_LOCALE, language);
}

if (!timezone) {
  timezone = APP_TIMEZONE;
  if (typeof window !== "undefined")
    localStorage.setItem(STORAGE_TIMEZONE, timezone);
}

if (user) {
  user = JSON.parse(user) as User;
  Cookies.set(STORAGE_LOCALE, user.language?.replace("_", "-")?.toLowerCase());
} else if (!!Cookies.get(STORAGE_USER)) {
  const userCookie = Cookies.get(STORAGE_USER) as string;
  user = JSON.parse(userCookie) as User;
  if (typeof window !== "undefined")
    secureLocalStorage.setItem(STORAGE_USER, `${JSON.stringify(user)}`);
  Cookies.set(STORAGE_LOCALE, user.language?.replace("_", "-")?.toLowerCase());
}

let jwt: any = null;

if (token) {
  try {
    const data: JWT = jwtDecode(token);
    if (data.exp >= Date.now() / 1000) {
      Cookies.set(STORAGE_AUTH_TOKEN, `${token}`);
      jwt = data;
      isLoggedIn = true;
    } else {
      jwt = null;
      token = null;
      user = null;
      isLoggedIn = false;
      Cookies.remove(STORAGE_AUTH_TOKEN);
    }
  } catch {
    jwt = null;
    token = null;
    user = null;
    isLoggedIn = false;
    Cookies.remove(STORAGE_AUTH_TOKEN);
  }
} else if (!!Cookies.get(STORAGE_AUTH_TOKEN)) {
  token = Cookies.get(STORAGE_AUTH_TOKEN) as string;
  try {
    const data: JWT = jwtDecode(token);
    if (data.exp >= Date.now() / 1000) {
      if (typeof window !== "undefined")
        secureLocalStorage.setItem(STORAGE_AUTH_TOKEN, `${token}`);
      jwt = data;
      isLoggedIn = true;
    } else {
      jwt = null;
      token = null;
      user = null;
      isLoggedIn = false;
      if (typeof window !== "undefined")
        secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
      Cookies.remove(STORAGE_AUTH_TOKEN);
    }
  } catch {
    jwt = null;
    token = null;
    user = null;
    isLoggedIn = false;
    if (typeof window !== "undefined")
      secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
    Cookies.remove(STORAGE_AUTH_TOKEN);
  }
} else {
  if (typeof window !== "undefined")
    secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
  Cookies.remove(STORAGE_AUTH_TOKEN);
}

const initialState: AuthState = {
  user,
  token,
  jwt,
  timezone,
  language,
  isLoggedIn,
};

export const authSlice = createSlice({
  name: "auth",
  initialState,
  reducers: {
    resetAuth: (state) => {
      state.user = initialState.user;
      state.token = initialState.token;
      state.jwt = initialState.jwt;
      state.language = APP_LANGUAGE;
      state.timezone = APP_TIMEZONE;

      Cookies.remove(STORAGE_USER);
      Cookies.remove(STORAGE_AUTH_TOKEN);

      if (typeof window !== "undefined") {
        secureLocalStorage.removeItem(STORAGE_USER);
        secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
        localStorage.setItem(STORAGE_LANGUAGE, APP_LANGUAGE);
        localStorage.setItem(STORAGE_TIMEZONE, APP_TIMEZONE);
      }
    },

    setUser: (state, action: PayloadAction<User>) => {
      state.user = action.payload;
      if (typeof window !== "undefined")
        secureLocalStorage.setItem(
          STORAGE_USER,
          JSON.stringify(action.payload)
        );
      Cookies.remove(STORAGE_USER);
    },
    setToken: (state, action: PayloadAction<string>) => {
      state.token = action.payload;

      Cookies.set(STORAGE_AUTH_TOKEN, `${state.token}`);

      if (typeof window !== "undefined")
        secureLocalStorage.setItem(STORAGE_AUTH_TOKEN, action.payload);

      try {
        const jwt: JWT = jwtDecode(state.token as string);
        state.jwt = jwt;
      } catch {
        state.jwt = null;
        Cookies.remove(STORAGE_USER);
        Cookies.remove(STORAGE_AUTH_TOKEN);
      }
    },
    setLanguage: (state, action: PayloadAction<string>) => {
      state.language = action.payload.replace("_", "-").toLowerCase();
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_LANGUAGE, state.language);
    },
    setTimezone: (state, action: PayloadAction<string>) => {
      state.timezone = action.payload;
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_TIMEZONE, state.timezone);
    },
    setIsLoggedIn: (state, action: PayloadAction<boolean>) => {
      state.isLoggedIn = action.payload;
    },
    setAuth(state, action: PayloadAction<Auth>) {
      state.user = action.payload?.user;
      state.token = action.payload?.token;
      state.language = action.payload?.user?.language
        ?.replace("_", "-")
        ?.toLowerCase();
      state.timezone = action.payload?.user?.timezone?.code;

      Cookies.set(STORAGE_AUTH_TOKEN, `${state.token}`);

      if (typeof window !== "undefined") {
        secureLocalStorage.setItem(STORAGE_USER, JSON.stringify(state.user));
        secureLocalStorage.setItem(STORAGE_AUTH_TOKEN, `${state.token}`);
        localStorage.setItem(STORAGE_LANGUAGE, state.language);
        localStorage.setItem(STORAGE_TIMEZONE, state.timezone);
      }

      try {
        const jwt: JWT = jwtDecode(state.token as string);
        state.jwt = jwt;
        state.isLoggedIn = true;
      } catch {
        if (typeof window !== "undefined") {
          secureLocalStorage.removeItem(STORAGE_USER);
          secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
          localStorage.setItem(STORAGE_LANGUAGE, APP_LANGUAGE);
          localStorage.setItem(STORAGE_TIMEZONE, APP_TIMEZONE);
        }

        Cookies.remove(STORAGE_USER);
        Cookies.remove(STORAGE_AUTH_TOKEN);

        state.user = null;
        state.token = null;
        state.jwt = null;
        state.isLoggedIn = false;
        state.timezone = initialState.timezone;
      }
    },
  },
});

export const {
  resetAuth,
  setAuth,
  setUser,
  setToken,
  setLanguage,
  setTimezone,
  setIsLoggedIn,
} = authSlice.actions;
export default authSlice.reducer;
