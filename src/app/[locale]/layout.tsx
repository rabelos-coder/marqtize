import "../scss/globals.scss";

import { headers } from "next/headers";
import { notFound } from "next/navigation";
import { Metadata } from "next/types";
import { NextIntlClientProvider } from "next-intl";
import { unstable_setRequestLocale } from "next-intl/server";
import { Suspense } from "react";

import { CookieConsent } from "@/components/common/CookieConsent";
import { Spinner } from "@/components/common/Spinner";
import { locales } from "@/configs/i18n";
import {
  APP_LANGUAGE,
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_SLOGAN,
} from "@/environment";
import { ApolloProvider } from "@/providers/ApolloProvider";
import { ReduxProvider } from "@/providers/ReduxProvider";
import { ComponentWithLocaleProps } from "@/types/common";
import { concatTitle, icons } from "@/utils/helpers";

export async function generateMetadata(): Promise<Metadata> {
  return {
    title: concatTitle(APP_META_SLOGAN),
    description: APP_META_DESCRIPTION,
    keywords: APP_META_KEYWORDS,
    icons,
  };
}

export default async function LocaleLayout({
  children,
  params,
}: ComponentWithLocaleProps) {
  const header = headers();
  const host = header.get("host") ?? "";

  const locale = params?.locale ?? APP_LANGUAGE;
  unstable_setRequestLocale(locale);

  let messages;
  try {
    messages = (await import(`../../locales/${locale}.json`)).default;
  } catch (error) {
    console.error(error);

    return notFound();
  }

  if (!locales.includes(locale)) notFound();

  return (
    <html lang={locale} suppressHydrationWarning={true}>
      <body suppressHydrationWarning={true}>
        <NextIntlClientProvider locale={locale} messages={messages}>
          <ApolloProvider>
            <ReduxProvider host={host}>
              <Suspense fallback={<Spinner />}>
                {children}
                <CookieConsent />
              </Suspense>
            </ReduxProvider>
          </ApolloProvider>
        </NextIntlClientProvider>
      </body>
    </html>
  );
}
