"use client";

import { APP_LANGUAGE } from "@/environment";
import { redirect, usePathname } from "next/navigation";

export default function NotFound() {
  const pathname = usePathname();

  if (!pathname.split("/").includes(APP_LANGUAGE))
    redirect(`/${APP_LANGUAGE}${pathname}`);
}
