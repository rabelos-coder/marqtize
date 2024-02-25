import "next-auth";
import { User as DefaultUser } from "./types/auth";

declare module "next-auth" {
  interface User extends DefaultUser {}

  interface Session extends DefaultSession {
    user: User;
    expires: string;
    error: string;
  }
}
