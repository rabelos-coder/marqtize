'use client'

import { ApolloProvider as ApolloClientProvider } from '@apollo/client'

import { ChildrenProps } from '@/types/common'
import { createApolloClient } from '@/utils/apollo'

/**
 * ApolloProvider component to provide Apollo client to its children.
 *
 * @param {ChildrenProps} children - The child components to provide Apollo client to.
 * @return {JSX.Element} The Apollo client provider with the children.
 */
export function ApolloProvider({ children }: ChildrenProps): JSX.Element {
  return (
    <ApolloClientProvider client={createApolloClient()}>
      {children}
    </ApolloClientProvider>
  )
}
