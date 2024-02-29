import { redirect } from "next/navigation";

import { APP_LANGUAGE } from "@/environment";

export default function AppPage() {
  redirect(`/${APP_LANGUAGE}`);
}
