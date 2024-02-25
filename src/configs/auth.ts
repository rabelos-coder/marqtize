import { NextAuthOptions } from "next-auth";
import GoogleProvider from "next-auth/providers/google";
import { jwtDecode } from "jwt-decode";
export const authConfig: NextAuthOptions = {
  secret: process.env.NEXTAUTH_SECRET ?? "auth_secret",
  session: {
    strategy: "jwt",
  },
  providers: [
    GoogleProvider({
      clientId: process.env.GOOGLE_CLIENT_ID ?? "",
      clientSecret: process.env.GOOGLE_CLIENT_SECRET ?? "",
    }),
  ],
  callbacks: {
    async jwt({ token, user, account }) {
      if (user) {
        token.id = user.id;
      }

      if (account?.id_token) {
        token.accessToken = account.id_token;
      }

      return token;
    },
    async session({ session, token }) {
      const jwt: any = jwtDecode(token?.accessToken?.toString() || "");
      return {
        ...session,
        user: {
          ...session.user,
          role: token.role ?? "guest",
          id: token.id,
          image: token.picture ?? "",
          language: jwt?.locale?.substring(0, 2) ?? "pt",
          timezone: jwt?.timezone ?? "America/Sao_Paulo",
          customer: jwt?.customer ?? null,
        },
        token: token.accessToken ?? null,
      };
    },
  },
};
