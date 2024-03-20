import { gql, TypedDocumentNode } from '@apollo/client'

import { ContactInput, SendContact } from '@/types/contact'

export const CONTACT: TypedDocumentNode<SendContact, ContactInput> = gql`
  mutation SendContact($data: ContactInput!) {
    sendContact(data: $data)
  }
`
