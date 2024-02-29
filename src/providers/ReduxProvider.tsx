"use client";

import { Provider } from "react-redux";

import { ChildrenProps } from "@/types/common";

import { store } from "../store";

export function ReduxProvider({ children }: ChildrenProps) {
  return <Provider store={store}>{children}</Provider>;
}
