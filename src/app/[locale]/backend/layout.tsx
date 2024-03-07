import { Metadata } from "next/types";

import { AuthGuard } from "@/components/backend/Guards/AuthGuard";
import {
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_SLOGAN,
} from "@/environment";
import { AuthProvider } from "@/providers/AuthProvider";
import { CustomizerProvider } from "@/providers/CustomizerProvider";
import { LayoutProvider } from "@/providers/LayoutProvider";
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

export default async function BackendLayout({ children }: ChildrenProps) {
  return (
    <>
      <AuthProvider>
        <CustomizerProvider>
          <LayoutProvider>
            <AuthGuard>{children}</AuthGuard>
          </LayoutProvider>
        </CustomizerProvider>
      </AuthProvider>
    </>
  );
}
