import { redirect } from "next/navigation";

import { APP_LANGUAGE } from "@/environment";

export default function RootPage() {
  redirect(`/${APP_LANGUAGE}`);
}
