import { CSSProperties, ReactNode } from "react";

export type IconProviderProps = {
  children: ReactNode;
  className?: string;
  color?: string;
  size?: string;
  style?: CSSProperties;
  title?: string;
  [key: string]: any;
};
