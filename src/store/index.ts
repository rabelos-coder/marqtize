import { configureStore } from "@reduxjs/toolkit";
import auth from "./slices/authSlice";
import customer from "./slices/customerSlice";
import theme from "./slices/themeSlice";
import { IS_DEVELOPMENT } from "@/environment";

export const store = configureStore({
  reducer: {
    auth,
    customer,
    theme,
  },
  devTools: IS_DEVELOPMENT,
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({
      serializableCheck: false,
    }),
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;
