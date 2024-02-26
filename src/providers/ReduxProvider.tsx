"use client";

import { Provider } from "react-redux";

import { store } from "../store";
import { ChildrenProps } from "@/types/children";

export function ReduxProvider({ children }: ChildrenProps) {
  return <Provider store={store}>{children}</Provider>;
}
