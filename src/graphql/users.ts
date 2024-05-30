import { gql, TypedDocumentNode } from '@apollo/client'

import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'
import {
  CountUser,
  CreateUser,
  CreateUserInput,
  DeleteManyUser,
  DeleteUser,
  FindByIdUser,
  FindFirstUser,
  FindManyUser,
  PaginatedUser,
  RemoveManyUser,
  RemoveUser,
  RestoreManyUser,
  RestoreUser,
  UpdateUser,
  UpdateUserInput,
} from '@/types/user'

export const FRAGMENT_USER_PROPS = gql`
  fragment UserProps on User {
    id
    accountId
    name
    systemName
    email
    image
    isActive
    isSuperAdmin
    language
    timezoneId
    type
    claims
    createdAt
    updatedAt
    deletedAt
    roles {
      id
      name
    }
    timezone {
      code
      name
    }
    tokens {
      id
      userId
    }
    account {
      id
      corporateName
      tradingName
      systemName
    }
    userAccounts {
      provider
    }
    userSessions {
      expiresAt
    }
  }
`

export const COUNT_USERS: TypedDocumentNode<CountUser, WhereInput> = gql`
  query CountUser($where: SearchUserInput) {
    countUser(where: $where)
  }
`

export const FIND_MANY_USERS: TypedDocumentNode<
  FindManyUser,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_USER_PROPS}
  query FindManyUser($where: SearchUserInput, $orderBy: SortUserInput) {
    findManyUser(where: $where, orderBy: $orderBy) {
      ...UserProps
    }
  }
`

export const PAGINATED_USERS: TypedDocumentNode<PaginatedUser, PaginatedInput> =
  gql`
    ${FRAGMENT_USER_PROPS}
    query PaginatedUser(
      $page: Int!
      $perPage: Int!
      $where: SearchUserInput
      $orderBy: SortUserInput
    ) {
      paginatedUser(
        page: $page
        perPage: $perPage
        where: $where
        orderBy: $orderBy
      ) {
        data {
          ...UserProps
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

export const FIND_USER: TypedDocumentNode<FindByIdUser, FindByIdInput> = gql`
  ${FRAGMENT_USER_PROPS}
  query FindByIdUser($id: Int!) {
    findByIdUser(id: $id) {
      ...UserProps
    }
  }
`

export const FIND_FIRST_USER: TypedDocumentNode<FindFirstUser, WhereInput> =
  gql`
    ${FRAGMENT_USER_PROPS}
    query FindFirstUser($where: SearchUserInput, $orderBy: SortUserInput) {
      findFirstUser(where: $where, orderBy: $orderBy) {
        ...UserProps
      }
    }
  `

export const CREATE_USER: TypedDocumentNode<CreateUser, CreateUserInput> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation CreateUser($data: CreateUserInput!) {
    createUser(data: $data) {
      ...UserProps
    }
  }
`

export const UPDATE_USER: TypedDocumentNode<UpdateUser, UpdateUserInput> = gql`
  ${FRAGMENT_USER_PROPS}
  mutation UpdateUser($data: UpdateUserInput!) {
    updateUser(data: $data) {
      ...UserProps
    }
  }
`

export const DELETE_USER: TypedDocumentNode<DeleteUser, FindByIdInput> = gql`
  mutation DeleteUser($id: Int!) {
    deleteUser(id: $id)
  }
`

export const DELETE_USERS: TypedDocumentNode<DeleteManyUser, FindByIdsInput> =
  gql`
    mutation DeleteManyUser($ids: [String!]!) {
      deleteManyUser(ids: $ids)
    }
  `

export const REMOVE_USER: TypedDocumentNode<RemoveUser, FindByIdInput> = gql`
  mutation RemoveUser($id: Int!) {
    removeUser(id: $id)
  }
`

export const REMOVE_USERS: TypedDocumentNode<RemoveManyUser, FindByIdsInput> =
  gql`
    mutation RemoveManyUser($ids: [String!]!) {
      removeManyUser(ids: $ids)
    }
  `

export const RESTORE_USER: TypedDocumentNode<RestoreUser, FindByIdInput> = gql`
  mutation RestoreUser($id: Int!) {
    restoreUser(id: $id)
  }
`

export const RESTORE_USERS: TypedDocumentNode<RestoreManyUser, FindByIdsInput> =
  gql`
    mutation RestoreManyUser($ids: [String!]!) {
      restoreManyUser(ids: $ids)
    }
  `
