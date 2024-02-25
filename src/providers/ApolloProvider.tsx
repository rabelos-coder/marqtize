"use client";

import { createApolloClient } from "@/utils/apollo";
import { ComponentProps } from "@/types";
import { ApolloProvider as ApolloClientProvider } from "@apollo/client";

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
