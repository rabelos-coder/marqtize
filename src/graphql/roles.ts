import { gql, TypedDocumentNode } from '@apollo/client'

import {
  FindByIdInput,
  FindByIdsInput,
  PaginatedInput,
  PaginatedObject,
  WhereAndOrderInput,
  WhereInput,
} from '@/types/common'
import {
  CountRole,
  CreateRole,
  CreateRoleInput,
  DeleteManyRole,
  DeleteRole,
  FindByIdRole,
  FindFirstRole,
  FindManyRole,
  Role,
  UpdateRole,
  UpdateRoleInput,
} from '@/types/role'

const FRAGMENT_ROLE_PROPS = gql`
  fragment RoleProps on Role {
    id
    customerId
    name
    claims
    isDeleteable
    isDefault
    createdAt
    updatedAt
    users {
      id
      name
      systemName
    }
    customer {
      id
      corporateName
      tradingName
      systemName
      slug
    }
  }
`

export const COUNT_ROLES: TypedDocumentNode<CountRole, WhereInput> = gql`
  query CountRole($where: SearchRoleInput) {
    countRole(where: $where)
  }
`

export const FIND_ROLES: TypedDocumentNode<FindManyRole, WhereAndOrderInput> =
  gql`
    ${FRAGMENT_ROLE_PROPS}
    query FindManyRole($where: SearchRoleInput, $orderBy: SortRoleInput) {
      findManyRole(where: $where, orderBy: $orderBy) {
        ...RoleProps
      }
    }
  `

export const PAGINATED_ROLES: TypedDocumentNode<
  PaginatedObject<Role>,
  PaginatedInput
> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query RolePaginated(
    $page: Int!
    $perPage: Int!
    $where: RoleWhereDto
    $orderBy: RoleOrderByDto
  ) {
    rolePaginated(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...RoleProps
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

export const FIND_ROLE: TypedDocumentNode<FindByIdRole, FindByIdInput> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query FindByIdRole($id: String!) {
    findByIdRole(id: $id) {
      ...RoleProps
    }
  }
`

export const FIND_FIRST_ROLE: TypedDocumentNode<
  FindFirstRole,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_ROLE_PROPS}
  query FindFirstRole($where: SearchRoleInput, $orderBy: SortRoleInput) {
    findFirstRole(where: $where, orderBy: $orderBy) {
      ...RoleProps
    }
  }
`

export const CREATE_ROLE: TypedDocumentNode<CreateRole, CreateRoleInput> = gql`
  mutation CreateRole($data: CreateRoleInput!) {
    createRole(data: $data) {
      id
    }
  }
`

export const UPDATE_ROLE: TypedDocumentNode<UpdateRole, UpdateRoleInput> = gql`
  mutation UpdateRole($data: UpdateRoleInput!) {
    updateRole(data: $data) {
      id
    }
  }
`

export const DELETE_ROLE: TypedDocumentNode<DeleteRole, FindByIdInput> = gql`
  mutation DeleteRole($id: String!) {
    deleteRole(id: $id)
  }
`

export const DELETE_ROLES: TypedDocumentNode<DeleteManyRole, FindByIdsInput> =
  gql`
    mutation DeleteManyRole($ids: [String!]!) {
      deleteManyRole(ids: $ids)
    }
  `
