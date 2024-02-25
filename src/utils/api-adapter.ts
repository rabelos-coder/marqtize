import {
  CREATE_SESSION,
  CREATE_SOCIAL_LOGIN,
  CREATE_VERIFICATION_TOKEN,
  DELETE_SESSION,
  FIND_USER_BY_ACCOUNT,
  FIND_USER_BY_EMAIL,
  FIND_USER_BY_ID,
  GET_SESSION_AND_USER,
  LINK_USER_ACCOUNT,
  UNLINK_USER_ACCOUNT,
  UPDATE_SESSION,
  UPDATE_SOCIAL_LOGIN,
  USE_VERIFICATION_TOKEN,
} from "@/graphql/auth";
import { createApolloClient } from "@/utils/apollo";
import { Adapter } from "next-auth/adapters";

const client = createApolloClient();

export function ApiAdapter(): Adapter {
  return {
    async createUser(user: any) {
      return await client
        .query({
          query: FIND_USER_BY_EMAIL,
          variables: {
            email: user.email,
          },
        })
        .then(async ({ data }) => {
          if (!data.findUserByEmail)
            return await client
              .mutate({
                mutation: CREATE_SOCIAL_LOGIN,
                variables: {
                  data: user,
                },
              })
              .then(({ data }) => data.createSocialLogin)
              .catch(() => null);
          else return data.findUserByEmail;
        })
        .catch(
          async () =>
            await client
              .mutate({
                mutation: CREATE_SOCIAL_LOGIN,
                variables: {
                  data: user,
                },
              })
              .then(({ data }) => data.createSocialLogin)
              .catch(() => null)
        );
    },
    getUser(id: string) {
      return client
        .query({
          query: FIND_USER_BY_ID,
          variables: { id },
        })
        .then(({ data }) => data.findUserById)
        .catch(() => null);
    },
    getUserByEmail(email: string) {
      return client
        .query({
          query: FIND_USER_BY_EMAIL,
          variables: {
            email,
          },
        })
        .then(({ data }) => data.findUserByEmail)
        .catch(() => null);
    },
    async getUserByAccount({ providerAccountId, provider }) {
      const { data, errors } = await client.query({
        query: FIND_USER_BY_ACCOUNT,
        variables: {
          data: { provider, providerAccountId },
        },
      });

      if (!data.findUserByAccount && errors?.length) return null;
      else if (data.findUserByAccount) return data.findUserByAccount;

      return null;
    },
    updateUser(user: any) {
      return client
        .mutate({
          mutation: UPDATE_SOCIAL_LOGIN,
          variables: {
            data: user,
          },
        })
        .then(({ data }) => data.updateSocialLogin)
        .catch(() => null);
    },
    linkAccount(account: any) {
      return client
        .mutate({
          mutation: LINK_USER_ACCOUNT,
          variables: {
            data: account,
          },
        })
        .then(({ data }) => data.linkUserAccount)
        .catch(() => null);
    },
    unlinkAccount(account: any) {
      return client
        .mutate({
          mutation: UNLINK_USER_ACCOUNT,
          variables: {
            data: account,
          },
        })
        .then(({ data }) => data.unlinkUserAccount)
        .catch(() => null);
    },
    createSession(session: any) {
      return client
        .mutate({
          mutation: CREATE_SESSION,
          variables: {
            data: session,
          },
        })
        .then(({ data }) => data.createSession)
        .catch(() => null);
    },
    async getSessionAndUser(sessionToken: string) {
      const { data, errors } = await client.query({
        query: GET_SESSION_AND_USER,
        variables: {
          sessionToken,
        },
      });

      if (!data.findSessionAndUser && errors?.length) return null;
      else if (data.findSessionAndUser) return data.findSessionAndUser;

      return null;
    },
    updateSession(session: any) {
      return client
        .mutate({
          mutation: UPDATE_SESSION,
          variables: {
            data: session,
          },
        })
        .then(({ data }) => data.updateSession)
        .catch(() => null);
    },
    deleteSession(sessionToken: string) {
      return client
        .mutate({
          mutation: DELETE_SESSION,
          variables: {
            sessionToken,
          },
        })
        .then(({ data }) => data.deleteSession)
        .catch(() => null);
    },
    createVerificationToken(verificationToken: any) {
      return client
        .mutate({
          mutation: CREATE_VERIFICATION_TOKEN,
          variables: {
            data: verificationToken,
          },
        })
        .then(({ data }) => data.createVerificationToken)
        .catch(() => null);
    },
    async useVerificationToken(identifierToken: any) {
      const { data, errors } = await client.mutate({
        mutation: USE_VERIFICATION_TOKEN,
        variables: {
          data: identifierToken,
        },
      });

      if (!data.useVerificationToken && errors?.length) return null;
      else if (data.useVerificationToken) return data.useVerificationToken;

      return null;
    },
  };
}
