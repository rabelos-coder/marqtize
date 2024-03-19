import "../assets/scss/globals.scss";

import axios from "axios";
import { headers } from "next/headers";
import { notFound } from "next/navigation";
import Script from "next/script";
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
  APP_META_TITLE,
  APP_WEBSITE,
  IS_DEVELOPMENT,
} from "@/environment";
import { ApolloProvider } from "@/providers/ApolloProvider";
import { ReduxProvider } from "@/providers/ReduxProvider";
import { ComponentWithLocaleProps } from "@/types/common";
import { concatTitle, icons } from "@/utils/helpers";

export async function generateMetadata({
  params: { locale },
}: any): Promise<Metadata> {
  const title = concatTitle(APP_META_SLOGAN);
  const headersList = headers();

  const referer = headersList?.get("referer") ?? APP_WEBSITE;
  const refererURL = new URL(referer);
  const protocol = refererURL.protocol;

  const host = headersList?.get("host") ?? "";
  const baseUrl = `${protocol}//${host}`;
  const url = headersList?.get("next-url")
    ? `${baseUrl}${headersList?.get("next-url")}`
    : "http://localhost:3000";

  const description = APP_META_DESCRIPTION;
  const keywords = APP_META_KEYWORDS;

  return {
    title,
    metadataBase: new URL(baseUrl),
    alternates: {
      canonical: "/",
      languages: {
        en: "/en",
        "en-US": "/en",
        pt: "/pt-br",
        "pt-BR": "/pt-br",
      },
    },
    description,
    keywords,
    icons,
    robots: {
      index: true,
      follow: true,
      googleBot: {
        index: true,
        follow: true,
        "max-video-preview": -1,
        "max-image-preview": "large",
        "max-snippet": -1,
      },
    },
    appleWebApp: true,
    openGraph: {
      title,
      locale,
      type: "website",
      description,
      url,
      siteName: APP_META_TITLE,
      images: [
        {
          url: "/assets/images/theme/landing/og-image-800x600.png",
          width: 800,
          height: 600,
        },
        {
          url: "/assets/images/theme/landing/og-image-1800x1600.png",
          width: 1800,
          height: 1600,
        },
      ],
    },
  };
}

export default async function LocaleLayout({
  children,
  params,
}: ComponentWithLocaleProps) {
  const header = headers();
  const host = header.get("host") ?? "";

  let connected = false;
  try {
    const { data } = await axios.get(`http://localhost:8097`, {
      timeout: 3000,
    });

    connected = !!data;
  } catch {}

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
    <html lang={locale === "pt-br" ? "pt-BR" : locale} suppressHydrationWarning>
      <body suppressHydrationWarning>
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
        {IS_DEVELOPMENT && connected && <Script src="http://localhost:8097" />}
      </body>
    </html>
  );
}
