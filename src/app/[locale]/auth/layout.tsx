import type { Metadata } from "next";
import { ComponentProps } from "@/types";
import { AuthProvider } from "@/providers/AuthProvider";

export const metadata: Metadata = {
  title: "Create Next App",
  description: "Generated by create next app",
  icons: [
    {
      url: "/assets/images/favicon.png",
      rel: "icon",
      type: "image/x-icon",
    },
    {
      url: "/assets/images/favicon.png",
      rel: "shortcut icon",
      type: "image/x-icon",
    },
  ],
};

export default function AuthLayout({ children }: ComponentProps) {
  return <AuthProvider>{children}</AuthProvider>;
}
