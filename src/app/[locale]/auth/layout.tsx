import "../../assets/scss/app.scss";

import { Metadata } from "next/types";
import { ToastContainer } from "react-toastify";

import {
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_SLOGAN,
} from "@/environment";
import { GuestLayout } from "@/layout/common/GuestLayout";
import { AuthProvider } from "@/providers/AuthProvider";
import { ChildrenProps } from "@/types/common";
import { concatTitle, icons } from "@/utils/helpers";

export function generateMetadata(): Metadata {
  const title = concatTitle(APP_META_SLOGAN);

  return {
    title,
    description: APP_META_DESCRIPTION,
    keywords: APP_META_KEYWORDS,
    icons,
  };
}

export default function AuthLayout({ children }: ChildrenProps) {
  return (
    <AuthProvider>
      <GuestLayout>{children}</GuestLayout>
      <ToastContainer />
    </AuthProvider>
  );
}
