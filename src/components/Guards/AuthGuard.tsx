import { getServerSession } from "next-auth";
import { authConfig } from "@/configs/auth";
import { redirect } from "@/navigation";
import { ComponentProps } from "@/types";

/**
 * Asynchronous function to guard access to authenticated routes by checking for a server session.
 *
 * @param {ComponentProps} children - The child components to be rendered within the guarded route.
 * @return {Promise<JSX.Element>} The original child components to be rendered if the user is authenticated.
 */
const AuthGuard = async ({
  children,
}: ComponentProps): Promise<JSX.Element> => {
  const session = await getServerSession(authConfig);

  if (!session) redirect("/auth/login");

  return <>{children}</>;
};

export default AuthGuard;
