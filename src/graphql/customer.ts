import { gql, TypedDocumentNode } from '@apollo/client'

import {
  FindByIdInput,
  FindByIdsInput,
  FindBySlugInput,
  PaginatedInput,
  PaginatedObject,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'
import {
  CountCustomer,
  CreateCustomer,
  CreateCustomerInput,
  Customer,
  DeleteCustomer,
  DeleteManyCustomer,
  FindByIdCustomer,
  FindBySlugCustomer,
  FindFirstCustomer,
  FindManyCustomer,
  RemoveCustomer,
  RemoveManyCustomer,
  RestoreCustomer,
  RestoreManyCustomer,
  UpdateCustomer,
  UpdateCustomerInput,
} from '@/types/customer'

const FRAGMENT_CUSTOMER_PROPS = gql`
  fragment CustomerProps on Customer {
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

export const COUNT_CUSTOMERS: TypedDocumentNode<CountCustomer, WhereInput> =
  gql`
    query CountCustomer($where: SearchCustomerInput) {
      countCustomer(where: $where)
    }
  `

export const FIND_CUSTOMERS: TypedDocumentNode<
  FindManyCustomer,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_CUSTOMER_PROPS}
  query FindManyCustomer(
    $where: SearchCustomerInput
    $orderBy: SortCustomerInput
  ) {
    findManyCustomer(where: $where, orderBy: $orderBy) {
      ...CustomerProps
    }
  }
`

export const PAGINATED_CUSTOMERS: TypedDocumentNode<
  PaginatedObject<Customer>,
  PaginatedInput
> = gql`
  ${FRAGMENT_CUSTOMER_PROPS}
  query PaginatedCustomer(
    $page: Int!
    $perPage: Int!
    $where: SearchCustomerInput
    $orderBy: SortCustomerInput
  ) {
    paginatedCustomer(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...CustomerProps
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

export const FIND_CUSTOMER: TypedDocumentNode<FindByIdCustomer, FindByIdInput> =
  gql`
    ${FRAGMENT_CUSTOMER_PROPS}
    query FindByIdCustomer($id: String!) {
      findByIdCustomer(id: $id) {
        ...CustomerProps
      }
    }
  `

export const FIND_FIRST_CUSTOMER: TypedDocumentNode<
  FindFirstCustomer,
  WhereInput
> = gql`
  ${FRAGMENT_CUSTOMER_PROPS}
  query FindFirstCustomer(
    $where: SearchCustomerInput
    $orderBy: SortCustomerInput
  ) {
    findFirstCustomer(where: $where, orderBy: $orderBy) {
      ...CustomerProps
    }
  }
`

export const FIND_CUSTOMER_BY_SLUG: TypedDocumentNode<
  FindBySlugCustomer,
  FindBySlugInput
> = gql`
  ${FRAGMENT_CUSTOMER_PROPS}
  query FindBySlugCustomer($slug: String!) {
    findBySlugCustomer(slug: $slug) {
      ...CustomerProps
    }
  }
`

export const CREATE_CUSTOMER: TypedDocumentNode<
  CreateCustomer,
  CreateCustomerInput
> = gql`
  mutation CreateCustomer($data: CreateCustomerInput!) {
    createCustomer(data: $data) {
      id
    }
  }
`

export const UPDATE_CUSTOMER: TypedDocumentNode<
  UpdateCustomer,
  UpdateCustomerInput
> = gql`
  mutation UpdateCustomer($data: UpdateCustomerInput!) {
    updateCustomer(data: $data) {
      id
    }
  }
`

export const DELETE_CUSTOMER: TypedDocumentNode<DeleteCustomer, FindByIdInput> =
  gql`
    mutation DeleteCustomer($id: String!) {
      deleteCustomer(id: $id)
    }
  `

export const DELETE_CUSTOMERS: TypedDocumentNode<
  DeleteManyCustomer,
  FindByIdsInput
> = gql`
  mutation DeleteManyCustomer($ids: [String!]!) {
    deleteManyCustomer(ids: $ids)
  }
`

export const REMOVE_CUSTOMER: TypedDocumentNode<RemoveCustomer, FindByIdInput> =
  gql`
    mutation RemoveCustomer($id: String!) {
      removeCustomer(id: $id)
    }
  `

export const REMOVE_CUSTOMERS: TypedDocumentNode<
  RemoveManyCustomer,
  FindByIdsInput
> = gql`
  mutation RemoveManyCustomer($ids: [String!]!) {
    removeManyCustomer(ids: $ids)
  }
`

export const RESTORE_CUSTOMER: TypedDocumentNode<
  RestoreCustomer,
  FindByIdInput
> = gql`
  mutation RestoreCustomer($id: String!) {
    restoreCustomer(id: $id)
  }
`

export const RESTORE_CUSTOMERS: TypedDocumentNode<
  RestoreManyCustomer,
  FindByIdsInput
> = gql`
  mutation RestoreManyCustomer($ids: [String!]!) {
    restoreManyCustomer(ids: $ids)
  }
`
