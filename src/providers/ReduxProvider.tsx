"use client";

import { useRef } from "react";
import { Provider } from "react-redux";

import { fetchCustomer } from "@/store/slices/customerSlice";
import { ReduxProviderProps } from "@/types/common";

import { AppStore, makeStore } from "../store";

export function ReduxProvider({ host, children }: ReduxProviderProps) {
  const storeRef = useRef<AppStore | null>(null);

  if (!storeRef.current) {
    // Create the store instance the first time this renders
    storeRef.current = makeStore();
    storeRef.current.dispatch(fetchCustomer(host));
  }

  return <Provider store={storeRef.current}>{children}</Provider>;
}
