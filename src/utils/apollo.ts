import { ApolloClient, from, InMemoryCache, split } from "@apollo/client";
import { onError } from "@apollo/client/link/error";
import createUploadLink from "apollo-upload-client/createUploadLink.mjs";
import { getMainDefinition } from "@apollo/client/utilities";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import { setContext } from "@apollo/client/link/context";
import axios from "axios";
import {
  APP_META_TITLE,
  APP_VERSION,
  IS_DEVELOPMENT,
  SERVER_URL,
} from "@/environment";

export const createApolloClient = () => {
  const authMiddleware = setContext(async (operation, { headers }) => {
    const { token } = await axios
      .get("/api/auth/session")
      .then((res) => res.data);

    return {
      headers: {
        ...headers,
        "Apollo-Require-Preflight": "true",
        Authorization: `Bearer ${token}`,
      },
      connectionParams: {
        authToken: token,
      },
    };
  });

  // GraphQL server URL
  const graphqlLink = `${SERVER_URL}/graphql`;

  // Websocket link for subscriptions
  const graphqlWsLink = `ws${graphqlLink.startsWith("https") ? "s" : ""}://${
    graphqlLink.startsWith("https")
      ? graphqlLink.substring(8)
      : graphqlLink.substring(7)
  }`;

  // Websocket link for subscriptions
  const wsLink = new GraphQLWsLink(
    createClient({
      url: graphqlWsLink,
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
    uri: graphqlLink,
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
