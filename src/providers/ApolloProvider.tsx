"use client";

import { ApolloProvider as ApolloClientProvider } from "@apollo/client";

import { ComponentProps } from "@/types";
import { createApolloClient } from "@/utils/apollo";

/**
 * ApolloProvider component to provide Apollo client to its children.
 *
 * @param {ComponentProps} children - The child components to provide Apollo client to.
 * @return {JSX.Element} The Apollo client provider with the children.
 */
export function ApolloProvider({ children }: ComponentProps): JSX.Element {
  return (
    <ApolloClientProvider client={createApolloClient()}>
      {children}
    </ApolloClientProvider>
  );
}
