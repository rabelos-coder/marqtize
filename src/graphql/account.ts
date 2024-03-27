import { gql, TypedDocumentNode } from '@apollo/client'

import {
  Account,
  CountAccount,
  CreateAccount,
  CreateAccountInput,
  DeleteAccount,
  DeleteManyAccount,
  FindByIdAccount,
  FindBySlugOrHostAccount,
  FindBySlugOrHostInput,
  FindFirstAccount,
  FindManyAccount,
  RemoveAccount,
  RemoveManyAccount,
  RestoreAccount,
  RestoreManyAccount,
  UpdateAccount,
  UpdateAccountInput,
} from '@/types/account'
import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  PaginatedObject,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'

const FRAGMENT_ACCOUNT_PROPS = gql`
  fragment AccountProps on Account {
    id
    userId
    erpId
    corporateNumber
    corporateName
    tradingName
    tradingLogo
    systemName
    email
    slug
    phone
    mobile
    postcode
    address
    number
    complement
    neighborhood
    countryId
    stateId
    cityId
    isActive
    createdAt
    updatedAt
    deletedAt
    country {
      name
    }
    state {
      name
    }
    city {
      name
    }
  }
`

export const COUNT_ACCOUNTS: TypedDocumentNode<CountAccount, WhereInput> = gql`
  query CountAccount($where: SearchAccountInput) {
    countAccount(where: $where)
  }
`

export const FIND_MANY_ACCOUNTS: TypedDocumentNode<
  FindManyAccount,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_ACCOUNT_PROPS}
  query FindManyAccount(
    $where: SearchAccountInput
    $orderBy: SortAccountInput
  ) {
    findManyAccount(where: $where, orderBy: $orderBy) {
      ...AccountProps
    }
  }
`

export const PAGINATED_ACCOUNTS: TypedDocumentNode<
  PaginatedObject<Account>,
  PaginatedInput
> = gql`
  ${FRAGMENT_ACCOUNT_PROPS}
  query PaginatedAccount(
    $page: Int!
    $perPage: Int!
    $where: SearchAccountInput
    $orderBy: SortAccountInput
  ) {
    paginatedAccount(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...AccountProps
      }
      meta {
        total
        lastPage
        currentPage
        perPage
        prev
        next
      }
    }
  }
`

export const FIND_ACCOUNT: TypedDocumentNode<FindByIdAccount, FindByIdInput> =
  gql`
    ${FRAGMENT_ACCOUNT_PROPS}
    query FindByIdAccount($id: String!) {
      findByIdAccount(id: $id) {
        ...AccountProps
      }
    }
  `

export const FIND_FIRST_ACCOUNT: TypedDocumentNode<
  FindFirstAccount,
  WhereInput
> = gql`
  ${FRAGMENT_ACCOUNT_PROPS}
  query FindFirstAccount(
    $where: SearchAccountInput
    $orderBy: SortAccountInput
  ) {
    findFirstAccount(where: $where, orderBy: $orderBy) {
      ...AccountProps
    }
  }
`

export const FIND_ACCOUNT_BY_SLUG: TypedDocumentNode<
  FindBySlugOrHostAccount,
  FindBySlugOrHostInput
> = gql`
  query FindBySlugOrHostAccount($slug: String!, $host: String!) {
    findBySlugOrHostAccount(slug: $slug, host: $host) {
      systemName
      tradingName
      corporateName
      tradingLogo
    }
  }
`

export const CREATE_ACCOUNT: TypedDocumentNode<
  CreateAccount,
  CreateAccountInput
> = gql`
  mutation CreateAccount($data: CreateAccountInput!) {
    createAccount(data: $data) {
      id
    }
  }
`

export const UPDATE_ACCOUNT: TypedDocumentNode<
  UpdateAccount,
  UpdateAccountInput
> = gql`
  mutation UpdateAccount($data: UpdateAccountInput!) {
    updateAccount(data: $data) {
      id
    }
  }
`

export const DELETE_ACCOUNT: TypedDocumentNode<DeleteAccount, FindByIdInput> =
  gql`
    mutation DeleteAccount($id: String!) {
      deleteAccount(id: $id)
    }
  `

export const DELETE_ACCOUNTS: TypedDocumentNode<
  DeleteManyAccount,
  FindByIdsInput
> = gql`
  mutation DeleteManyAccount($ids: [String!]!) {
    deleteManyAccount(ids: $ids)
  }
`

export const REMOVE_ACCOUNT: TypedDocumentNode<RemoveAccount, FindByIdInput> =
  gql`
    mutation RemoveAccount($id: String!) {
      removeAccount(id: $id)
    }
  `

export const REMOVE_ACCOUNTS: TypedDocumentNode<
  RemoveManyAccount,
  FindByIdsInput
> = gql`
  mutation RemoveManyAccount($ids: [String!]!) {
    removeManyAccount(ids: $ids)
  }
`

export const RESTORE_ACCOUNT: TypedDocumentNode<RestoreAccount, FindByIdInput> =
  gql`
    mutation RestoreAccount($id: String!) {
      restoreAccount(id: $id)
    }
  `

export const RESTORE_ACCOUNTS: TypedDocumentNode<
  RestoreManyAccount,
  FindByIdsInput
> = gql`
  mutation RestoreManyAccount($ids: [String!]!) {
    restoreManyAccount(ids: $ids)
  }
`
