import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import Cookies from "js-cookie";
import { jwtDecode } from "jwt-decode";
import secureLocalStorage from "react-secure-storage";

import {
  STORAGE_AUTH_TOKEN,
  STORAGE_LANGUAGE,
  STORAGE_TIMEZONE,
  STORAGE_USER,
} from "@/configs";
import { APP_LANGUAGE, APP_TIMEZONE } from "@/environment";
import { AUTH_LOGIN, REGISTER } from "@/graphql/auth";
import { AuthState, LoginInput, RegisterInput } from "@/types/auth";
import { JWT } from "@/types/jwt";
import { createApolloClient } from "@/utils/apollo";

let language =
  typeof window !== "undefined"
    ? (localStorage.getItem(STORAGE_LANGUAGE) as string) ?? null
    : null;

let timezone =
  typeof window !== "undefined"
    ? (localStorage.getItem(STORAGE_TIMEZONE) as string) ?? null
    : null;

let user: any =
  typeof window !== "undefined"
    ? (secureLocalStorage.getItem(STORAGE_USER) as string) ?? null
    : null;

const token =
  typeof window !== "undefined"
    ? (secureLocalStorage.getItem(STORAGE_AUTH_TOKEN) as string) ?? null
    : null;

if (!language) {
  language = APP_LANGUAGE;
  if (typeof window !== "undefined")
    localStorage.setItem(STORAGE_LANGUAGE, language);
}

if (!timezone) {
  timezone = APP_TIMEZONE;
  if (typeof window !== "undefined")
    localStorage.setItem(STORAGE_TIMEZONE, timezone);
}

if (user) user = JSON.parse(user);

let jwt: any = null;

if (token) {
  try {
    const data: JWT = jwtDecode(token);
    if (data.exp >= Date.now() / 1000) {
      Cookies.set(STORAGE_AUTH_TOKEN, `${token}`);
      jwt = data;
    } else {
      jwt = null;
      Cookies.remove(STORAGE_AUTH_TOKEN);
    }
  } catch {
    jwt = null;
    Cookies.remove(STORAGE_AUTH_TOKEN);
  }
} else {
  Cookies.remove(STORAGE_AUTH_TOKEN);
}

const initialState: AuthState = {
  user,
  token,
  jwt,
  timezone,
  language,
  loading: false,
  error: null,
};

export const fetchAuth = createAsyncThunk(
  "auth/fetchAuth",
  async (variables: LoginInput) => {
    const client = createApolloClient();

    try {
      const { data, errors } = await client.mutate({
        mutation: AUTH_LOGIN,
        variables,
      });

      if (!data && errors?.length) throw new Error(errors[0].message);

      if (data) {
        const { authLogin } = data;

        return authLogin;
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }
);

export const fetchRegister = createAsyncThunk(
  "auth/fetchRegister",
  async (input: RegisterInput) => {
    const client = createApolloClient();

    try {
      const { data, errors } = await client.mutate({
        mutation: REGISTER,
        variables: {
          data: input.data,
        },
      });

      if (!data && errors?.length) throw new Error(errors[0].message);

      if (data) {
        const { register } = data;

        return register;
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }
);

export const authSlice = createSlice({
  name: "auth",
  initialState,
  reducers: {
    resetAuth: (state) => {
      state.loading = false;
      state.error = null;
      state.user = initialState.user;
      state.token = initialState.token;
      state.jwt = initialState.jwt;
      state.language = APP_LANGUAGE;
      state.timezone = APP_TIMEZONE;

      Cookies.remove(STORAGE_AUTH_TOKEN);

      if (typeof window !== "undefined") {
        secureLocalStorage.removeItem(STORAGE_USER);
        secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
        localStorage.setItem(STORAGE_LANGUAGE, APP_LANGUAGE);
        localStorage.setItem(STORAGE_TIMEZONE, APP_TIMEZONE);
      }
    },
    resetError: (state) => {
      state.loading = false;
      state.error = null;
    },
    setUser: (state, action) => {
      state.user = action.payload;
      if (typeof window !== "undefined")
        secureLocalStorage.setItem(
          STORAGE_USER,
          JSON.stringify(action.payload)
        );
    },
    setToken: (state, action) => {
      state.token = action.payload;

      Cookies.set(STORAGE_AUTH_TOKEN, `${state.token}`);

      if (typeof window !== "undefined")
        secureLocalStorage.setItem(STORAGE_AUTH_TOKEN, action.payload);

      try {
        const jwt: JWT = jwtDecode(action.payload.token.toString() || "");
        state.jwt = jwt;
      } catch {
        state.jwt = null;
        Cookies.remove(STORAGE_AUTH_TOKEN);
      }
    },
    setLanguage: (state, action) => {
      state.language = action.payload;
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_LANGUAGE, state.language);
    },
    setTimezone: (state, action) => {
      state.timezone = action.payload;
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_TIMEZONE, state.timezone);
    },
    setAuth(state, action) {
      state.loading = false;
      state.error = null;
      state.user = action.payload?.user;
      state.token = action.payload?.token;
      state.language = action.payload?.user?.language;
      state.timezone = action.payload?.user?.timezone;

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
      } catch {
        if (typeof window !== "undefined") {
          secureLocalStorage.removeItem(STORAGE_USER);
          secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
          localStorage.setItem(STORAGE_LANGUAGE, APP_LANGUAGE);
          localStorage.setItem(STORAGE_TIMEZONE, APP_TIMEZONE);
        }

        Cookies.remove(STORAGE_AUTH_TOKEN);

        state.user = null;
        state.token = null;
        state.jwt = null;
        state.timezone = initialState.timezone;
      }
    },
  },
  extraReducers: (builder) => {
    builder.addCase(fetchAuth.pending, (state) => {
      state.loading = true;
      state.error = null;
    });
    builder.addCase(fetchAuth.rejected, (state, action) => {
      state.loading = false;
      state.error = `${action.error.message}`;
    });
    builder.addCase(fetchAuth.fulfilled, (state, action) => {
      state.loading = false;
      state.error = null;
      state.user = action.payload?.user ?? null;
      state.token = action.payload?.token ?? null;
      state.language = action.payload?.user?.language ?? APP_LANGUAGE;
      state.timezone = action.payload?.user?.timezone?.code ?? APP_TIMEZONE;

      Cookies.set(STORAGE_AUTH_TOKEN, `${state.token}`);

      if (typeof window !== "undefined") {
        if (state.user)
          secureLocalStorage.setItem(STORAGE_USER, JSON.stringify(state.user));
        if (state.token)
          secureLocalStorage.setItem(STORAGE_AUTH_TOKEN, `${state.token}`);
        localStorage.setItem(STORAGE_LANGUAGE, state.language);
        localStorage.setItem(STORAGE_TIMEZONE, state.timezone);
      }

      try {
        const jwt: JWT = jwtDecode(state.token as string);
        state.jwt = jwt;
      } catch {
        if (typeof window !== "undefined") {
          Cookies.remove(STORAGE_AUTH_TOKEN);
          secureLocalStorage.removeItem(STORAGE_USER);
          secureLocalStorage.removeItem(STORAGE_AUTH_TOKEN);
          localStorage.setItem(STORAGE_LANGUAGE, APP_LANGUAGE);
          localStorage.setItem(STORAGE_TIMEZONE, APP_TIMEZONE);
        }

        state.user = null;
        state.token = null;
        state.jwt = null;
        state.timezone = initialState.timezone;
      }
    });
  },
});

export const {
  resetAuth,
  resetError,
  setAuth,
  setUser,
  setToken,
  setLanguage,
  setTimezone,
} = authSlice.actions;
export default authSlice.reducer;
