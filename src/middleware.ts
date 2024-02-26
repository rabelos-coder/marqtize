import { jwtDecode } from "jwt-decode";
import { NextRequest, NextResponse } from "next/server";
import createMiddleware from "next-intl/middleware";

import { STORAGE_AUTH_TOKEN, STORAGE_LOCALE } from "./configs";
import {
  defaultLocale,
  localeDetection,
  localePrefix,
  locales,
} from "./configs/i18n";
import { APP_LANGUAGE } from "./environment";
import { JWT } from "./types/jwt";

const intlMiddleware = createMiddleware({
  locales,
  localePrefix,
  localeDetection,
  defaultLocale,
});

export default function middleware(request: NextRequest) {
  const token = request.cookies.get(STORAGE_AUTH_TOKEN)?.value;
  const locale = request.cookies.get(STORAGE_LOCALE)?.value ?? APP_LANGUAGE;

  const isAuth = request.nextUrl.pathname.split("/").includes("auth");
  const isBackend = request.nextUrl.pathname.split("/").includes("backend");

  if (isBackend && !token && !isAuth) {
    return NextResponse.redirect(new URL(`/${locale}/auth/login`, request.url));
  } else if (isBackend && token && !isAuth) {
    const { exp } = jwtDecode(token) as JWT;
    if (exp < Math.round(Date.now() / 1000)) {
      return NextResponse.redirect(
        new URL(`/${locale}/auth/login`, request.url)
      );
    }
  }

  if (isAuth && token && !isBackend) {
    const { exp } = jwtDecode(token) as JWT;
    if (exp > Math.round(Date.now() / 1000)) {
      return NextResponse.redirect(new URL(`/${locale}/backend`, request.url));
    }
  }

  return intlMiddleware(request);
}

export const config = {
  matcher: ["/", "/((?!api|_next|_vercel|.*\\..*).*)", "/(pt|en)/:path*"],
};
