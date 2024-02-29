import { notFound } from "next/navigation";
import { NextIntlClientProvider } from "next-intl";
import { unstable_setRequestLocale } from "next-intl/server";
import { Suspense } from "react";
import { ToastContainer } from "react-toastify";

import { locales } from "@/configs/i18n";
import {
  APP_LANGUAGE,
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_TITLE,
} from "@/environment";
import { Spinner } from "@/layouts/backend/Spinner";
import { ApolloProvider } from "@/providers/ApolloProvider";
import { ReduxProvider } from "@/providers/ReduxProvider";
import { ComponentWithLocaleProps } from "@/types/common";

export async function generateMetadata() {
  return {
    title: APP_META_TITLE,
    description: APP_META_DESCRIPTION,
    keywords: APP_META_KEYWORDS,
  };
}

export default async function LocaleLayout({
  children,
  params,
}: ComponentWithLocaleProps) {
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
            <ReduxProvider>
              <Suspense fallback={<Spinner />}>{children}</Suspense>
            </ReduxProvider>
          </ApolloProvider>
        </NextIntlClientProvider>
        <ToastContainer />
      </body>
    </html>
  );
}
