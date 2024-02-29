"use client";

import { redirect, usePathname } from "next/navigation";

import { APP_LANGUAGE } from "@/environment";

export default function AppNotFound() {
  const pathname = usePathname();

  if (!pathname.split("/").includes(APP_LANGUAGE))
    redirect(`/${APP_LANGUAGE}${pathname}`);
}
