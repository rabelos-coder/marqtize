import { authConfig } from "@/configs/auth";
import { redirect } from "@/navigation";
import { Login } from "@/views/auth/Login";
import { getServerSession } from "next-auth";

export default async function LoginPage() {
  const session = await getServerSession(authConfig);

  if (session) redirect("/backend");

  return <Login />;
}
