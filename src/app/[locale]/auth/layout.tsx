import { getTranslations } from "next-intl/server";

import { APP_META_DESCRIPTION, APP_META_KEYWORDS } from "@/environment";
import { AuthProvider } from "@/providers/AuthProvider";
import { ChildrenProps } from "@/types/common";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("backend"));

  return {
    title,
    description: APP_META_DESCRIPTION,
    keywords: APP_META_KEYWORDS,
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
}

export default function AuthLayout({ children }: ChildrenProps) {
  return <AuthProvider>{children}</AuthProvider>;
}
