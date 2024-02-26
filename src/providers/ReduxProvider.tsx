"use client";

import { Provider } from "react-redux";

import { ChildrenProps } from "@/types/children";

import { store } from "../store";

export function ReduxProvider({ children }: ChildrenProps) {
  return <Provider store={store}>{children}</Provider>;
}
