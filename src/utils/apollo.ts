import { ApolloClient, from, InMemoryCache, split } from "@apollo/client";
import { setContext } from "@apollo/client/link/context";
import { onError } from "@apollo/client/link/error";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { getMainDefinition } from "@apollo/client/utilities";
import createUploadLink from "apollo-upload-client/createUploadLink.mjs";
import { createClient } from "graphql-ws";
import Cookies from "js-cookie";

import { STORAGE_AUTH_TOKEN, STORAGE_LOCALE } from "@/configs";
import {
  APP_LANGUAGE,
  APP_META_TITLE,
  APP_VERSION,
  IS_DEVELOPMENT,
  SERVER_URL,
} from "@/environment";

type ApolloClientParams = {
  token?: string;
  locale?: string;
};

export const createApolloClient = (params?: ApolloClientParams) => {
  let token: any = null;
  let lang: any = null;

  if (!params?.token) token = Cookies.get(STORAGE_AUTH_TOKEN);
  else token = params.token;
  if (!params?.locale) lang = Cookies.get(STORAGE_LOCALE);
  else lang = params.locale;

  lang = lang?.replace("_", "-")?.toLowerCase();

  const authMiddleware = setContext(async (operation, { headers }) => {
    return {
      headers: {
        ...headers,
        "X-Lang": lang ?? APP_LANGUAGE,
        "Apollo-Require-Preflight": "true",
        Authorization: token ? `Bearer ${token}` : "",
      },
    };
  });

  // GraphQL server URL
  const graphqlUrl = `${SERVER_URL}/graphql`;

  // Websocket link for subscriptions
  const graphqlWsLink = `ws${graphqlUrl.startsWith("https") ? "s" : ""}://${
    graphqlUrl.startsWith("https")
      ? graphqlUrl.substring(8)
      : graphqlUrl.substring(7)
  }`;

  // Websocket link for subscriptions
  const wsLink = new GraphQLWsLink(
    createClient({
      url: graphqlWsLink,
      connectionParams: {
        authToken: token ? `Bearer ${token}` : "",
      },
    })
  );

  // Error handler for Apollo Client
  const errorLink = onError(({ graphQLErrors, networkError }) => {
    if (graphQLErrors && IS_DEVELOPMENT)
      graphQLErrors.forEach(({ message, locations, path }) =>
        console.log(
          `[GraphQL Error]: Message: ${message}, Location: ${JSON.stringify(
            locations
          )}, Path: ${path}`
        )
      );
    if (networkError && IS_DEVELOPMENT)
      console.log(`[GraphQL Network Error]: ${networkError}`);
  });

  // HTTP link for queries and mutations
  const httpLink = createUploadLink({
    uri: graphqlUrl,
  });

  // Split link for routing subscriptions through Websocket and other operations through HTTP
  const splitLink = split(
    ({ query }) => {
      const definition = getMainDefinition(query);

      return (
        definition.kind === "OperationDefinition" &&
        definition.operation === "subscription"
      );
    },
    wsLink,

    // @ts-ignore
    httpLink
  );

  // Apollo Client instance
  const client = new ApolloClient({
    name: APP_META_TITLE,
    version: APP_VERSION,
    connectToDevTools: IS_DEVELOPMENT,
    link: from([errorLink, authMiddleware, splitLink]),
    cache: new InMemoryCache(),
  });

  return client;
};
