import createMiddleware from "next-intl/middleware";
import {
  defaultLocale,
  localeDetection,
  localePrefix,
  locales,
} from "./configs/i18n";

export default createMiddleware({
  locales,
  localePrefix,
  localeDetection,
  defaultLocale,
});

export const config = {
  matcher: ["/", "/((?!api|_next|_vercel|.*\\..*).*)", "/(pt|en)/:path*"],
};
