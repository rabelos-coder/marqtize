import { NextAuthOptions } from "next-auth";
import CredentialsProvider from "next-auth/providers/credentials";
import GoogleProvider from "next-auth/providers/google";
import FacebookProvider from "next-auth/providers/facebook";
import AzureADProvider from "next-auth/providers/azure-ad";
import EmailProvider from "next-auth/providers/email";
import { jwtDecode } from "jwt-decode";
import { ApiAdapter } from "@/utils/api-adapter";
import { AUTH_LOGIN } from "@/graphql/auth";
import { trim } from "lodash";
import { createApolloClient } from "@/utils/apollo";
import { defaultLocale } from "./i18n";
import { APP_TIMEZONE } from "@/environment";
import { expiresTime } from "@/utils/helpers";

export const authConfig: NextAuthOptions = {
  adapter: ApiAdapter(),
  secret: process.env.NEXTAUTH_SECRET ?? "auth_secret",
  session: {
    strategy: "jwt",
    maxAge: expiresTime(7),
  },
  pages: {
    signIn: "/auth/login",

    // signOut: '/logout'
    // error: '/404'
  },
  providers: [
    EmailProvider({
      server: {
        host: process.env.EMAIL_SMTP_HOST,
        port: parseInt(process.env.EMAIL_SMTP_PORT ?? "587"),
        auth: {
          user: process.env.EMAIL_SMTP_USER,
          pass: process.env.EMAIL_SMTP_PASSWORD,
        },
      },
      from: process.env.EMAIL_SMTP_FROM,
    }),
    AzureADProvider({
      clientId: process.env.MICROSOFT_CLIENT_ID ?? "",
      clientSecret: process.env.MICROSOFT_CLIENT_SECRET ?? "",
      tenantId: process.env.MICROSOFT_TENANT_ID ?? "",
      authorization: {
        params: { scope: "openid email profile User.Read  offline_access" },
      },
      httpOptions: { timeout: 10000 },
    }),
    FacebookProvider({
      clientId: process.env.FACEBOOK_CLIENT_ID ?? "",
      clientSecret: process.env.FACEBOOK_CLIENT_SECRET ?? "",
    }),
    GoogleProvider({
      clientId: process.env.GOOGLE_CLIENT_ID ?? "",
      clientSecret: process.env.GOOGLE_CLIENT_SECRET ?? "",
    }),
    CredentialsProvider({
      name: "Credentials",
      type: "credentials",
      credentials: {},
      async authorize(credentials) {
        console.log(credentials);
        const client = createApolloClient();

        let { email, password, rememberMe } = credentials as {
          email: string;
          password: string;
          rememberMe: any;
        };

        email = trim(email);
        password = trim(password);
        rememberMe = rememberMe === "true";

        console.log({
          email,
          password,
          rememberMe,
        });

        if (!email || !password) return null;

        try {
          const { data, errors } = await client.mutate({
            mutation: AUTH_LOGIN,
            variables: {
              data: { email, password, rememberMe },
            },
          });

          if (!data && errors?.length) {
            console.log(errors[0].message);
            return null;
          } else if (data) {
            const {
              authLogin: { user, token },
            } = data;
            return {
              id: user.id,
              name: user.name,
              email: user.email,
              image: user.image,
              randomKey: token,
            } as any;
          }
          return null;
        } catch (error: any) {
          console.log(error.message);
          throw error;
        }
      },
    }),
  ],
  callbacks: {
    async jwt({ token, user, account }) {
      if (user) {
        const u = user as any;
        token.id = u.id;
        token.accessToken = u?.randomKey ?? "";
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
          language: jwt?.locale?.substring(0, 2) ?? defaultLocale,
          timezone: jwt?.timezone ?? APP_TIMEZONE,
          customer: jwt?.customer ?? null,
        },
        token: token.accessToken ?? null,
      };
    },
  },
};
