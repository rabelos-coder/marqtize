import { gql, TypedDocumentNode } from '@apollo/client'

import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'
import {
  CountLogging,
  DeleteLogging,
  DeleteManyLogging,
  FindByIdLogging,
  FindFirstLogging,
  FindManyLogging,
  PaginatedLogging,
} from '@/types/logging'

const FRAGMENT_LOGGING_PROPS = gql`
  fragment LoggingProps on Logging {
    id
    userId
    tokenId
    method
    operation
    endpoint
    ipAddress
    origin
    userAgent
    requestData
    responseData
    createdAt
    updatedAt
    user {
      id
      name
      systemName
      email
      accountId
      account {
        systemName
        tradingName
      }
      type
      image
    }
  }
`

export const COUNT_LOGGINGS: TypedDocumentNode<CountLogging, WhereInput> = gql`
  query CountLogging($where: SearchLoggingInput) {
    countLogging(where: $where)
  }
`

export const FIND_MANY_LOGGINGS: TypedDocumentNode<
  FindManyLogging,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_LOGGING_PROPS}
  query FindManyLogging(
    $where: SearchLoggingInput
    $orderBy: SortLoggingInput
  ) {
    findManyLogging(where: $where, orderBy: $orderBy) {
      ...LoggingsProps
    }
  }
`

export const PAGINATED_LOGGINGS: TypedDocumentNode<
  PaginatedLogging,
  PaginatedInput
> = gql`
  ${FRAGMENT_LOGGING_PROPS}
  query PaginatedLogging(
    $page: Int!
    $perPage: Int!
    $where: SearchLoggingInput
    $orderBy: SortLoggingInput
  ) {
    paginatedLogging(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...LoggingProps
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

export const FIND_LOGGING: TypedDocumentNode<FindByIdLogging, FindByIdInput> =
  gql`
    ${FRAGMENT_LOGGING_PROPS}
    query FindByIdLogging($id: Int!) {
      findByIdLogging(id: $id) {
        ...LoggingProps
      }
    }
  `

export const FIND_FIRST_LOGGING: TypedDocumentNode<
  FindFirstLogging,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_LOGGING_PROPS}
  query FindFirstLogging(
    $where: SearchLoggingInput
    $orderBy: SortLoggingInput
  ) {
    findFirstLogging(where: $where, orderBy: $orderBy) {
      ...LoggingProps
    }
  }
`

export const DELETE_LOGGING: TypedDocumentNode<DeleteLogging, FindByIdInput> =
  gql`
    mutation DeleteLogging($id: Int!) {
      deleteLogging(id: $id)
    }
  `

export const DELETE_LOGGINGS: TypedDocumentNode<
  DeleteManyLogging,
  FindByIdsInput
> = gql`
  mutation DeleteManyLogging($ids: [String!]!) {
    deleteManyLogging(ids: $ids)
  }
`
