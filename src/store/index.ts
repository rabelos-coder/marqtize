import { configureStore } from '@reduxjs/toolkit'

import { APP_META_TITLE, IS_DEVELOPMENT } from '@/environment'

import account from './slices/accountSlice'
import auth from './slices/authSlice'
import theme from './slices/themeSlice'

export const makeStore = () => {
  return configureStore({
    reducer: { auth, account, theme },
    devTools: IS_DEVELOPMENT
      ? { name: `${APP_META_TITLE}-store`.toLowerCase() }
      : false,
    middleware: (getDefaultMiddleware) =>
      getDefaultMiddleware({
        serializableCheck: false,
      }),
  })
}

export type AppStore = ReturnType<typeof makeStore>
export type RootState = ReturnType<AppStore['getState']>
export type AppDispatch = AppStore['dispatch']
