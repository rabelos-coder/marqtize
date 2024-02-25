import { APP_LANGUAGE } from "@/environment";
import { redirect } from "next/navigation";

export default function RootPage() {
  redirect(`/${APP_LANGUAGE}`);
}
