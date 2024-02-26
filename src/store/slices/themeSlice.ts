import { STORAGE_THEME } from "@/configs";
import { ThemeState } from "@/types/theme";
import { createSlice } from "@reduxjs/toolkit";

const defaultTheme = "light";

let theme: any =
  typeof window !== "undefined"
    ? (localStorage.getItem(STORAGE_THEME) as string) ?? null
    : null;

if (!theme) {
  theme = defaultTheme;
  if (typeof window !== "undefined") localStorage.setItem(STORAGE_THEME, theme);
}

const initialState: ThemeState = {
  theme,
};

export const themeSlice = createSlice({
  name: "theme",
  initialState,
  reducers: {
    resetTheme: (state) => {
      state.theme = defaultTheme;
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_THEME, defaultTheme);
    },
    setTheme: (state, action) => {
      state.theme = action.payload;
      if (typeof window !== "undefined")
        localStorage.setItem(STORAGE_THEME, state.theme);
    },
  },
});

export const { resetTheme, setTheme } = themeSlice.actions;
export default themeSlice.reducer;
