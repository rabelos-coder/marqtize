import { createSlice, PayloadAction } from '@reduxjs/toolkit'

import { STORAGE_PINNED_MENU, STORAGE_THEME } from '@/configs'
import { ThemeState } from '@/types/theme'

const defaultTheme = 'light'

let theme: any =
  typeof window !== 'undefined'
    ? (localStorage.getItem(STORAGE_THEME) as string) ?? null
    : null

if (!theme) {
  theme = defaultTheme
  if (typeof window !== 'undefined') localStorage.setItem(STORAGE_THEME, theme)
}

let pinnedMenu: any =
  typeof window !== 'undefined'
    ? (localStorage.getItem(STORAGE_PINNED_MENU) as string) ?? null
    : null

if (!pinnedMenu) {
  pinnedMenu = []
  try {
    if (typeof window !== 'undefined')
      localStorage.setItem(STORAGE_PINNED_MENU, JSON.stringify(pinnedMenu))
  } catch {}
} else {
  try {
    pinnedMenu = JSON.parse(pinnedMenu)
  } catch {}
}

const initialState: ThemeState = {
  theme,
  pinnedMenu,
  loading: false,
}

export const themeSlice = createSlice({
  name: 'theme',
  initialState,
  reducers: {
    resetTheme: (state) => {
      state.theme = defaultTheme
      state.pinnedMenu = []
      if (typeof window !== 'undefined') {
        localStorage.setItem(STORAGE_THEME, defaultTheme)
        localStorage.setItem(
          STORAGE_PINNED_MENU,
          JSON.stringify(state.pinnedMenu)
        )
      }
    },
    setTheme: (state, action: PayloadAction<string>) => {
      state.theme = action.payload
      if (typeof window !== 'undefined')
        localStorage.setItem(STORAGE_THEME, state.theme)
    },
    setPinnedMenu: (state, action: PayloadAction<string[]>) => {
      state.pinnedMenu = action.payload
      if (typeof window !== 'undefined')
        localStorage.setItem(
          STORAGE_PINNED_MENU,
          JSON.stringify(state.pinnedMenu)
        )
    },
    setLoading: (state, action: PayloadAction<boolean>) => {
      state.loading = action.payload
    },
  },
})

export const { resetTheme, setTheme, setPinnedMenu, setLoading } =
  themeSlice.actions
export default themeSlice.reducer
