import { notFound } from "next/navigation";
import { getRequestConfig } from "next-intl/server";

import { locales } from "@/configs/i18n";
import { APP_TIMEZONE } from "./environment";

export default getRequestConfig(async ({ locale }) => {
  if (!locales.includes(locale)) notFound();

  return {
    now: new Date(),
    timeZone: APP_TIMEZONE,
    messages: (await import(`./locales/${locale}.json`)).default,
  };
});
